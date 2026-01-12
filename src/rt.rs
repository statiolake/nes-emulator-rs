//! A minimal async runtime to cooperatively execute each chips of NES emulator.
//!
//! reference: https://techracho.bpsinc.jp/yoshi/2024_12_13/147377

use std::{
    mem,
    pin::{Pin, pin},
    sync::Arc,
    task::{Context, Poll, Wake, Waker},
    thread,
    time::{Duration, Instant},
};

const MASTER_HZ: u64 = 21_477_272; // 21.477272 MHz
const MASTER_TICK_DURATION: Duration = Duration::from_nanos(1_000_000_000 / MASTER_HZ);

pub struct Runtime {
    waker: Arc<Waker>,
    curr_time: u64,
    next_tick: Instant,
    main_chip: Option<Chip>,
    side_chips: Vec<Chip>,
}

impl Runtime {
    pub fn new() -> Self {
        let waker = Arc::new(Waker::from(CustomWaker::new()));
        Self {
            waker,
            curr_time: 0,
            next_tick: Instant::now(),
            main_chip: None,
            side_chips: vec![],
        }
    }

    pub fn add_chip<F>(&mut self, clock_mul: u64, future: F)
    where
        F: Future<Output = ()> + Send + 'static,
    {
        self.side_chips.push(Chip {
            clock_mul,
            future: Box::pin(future),
        });
    }

    pub fn run_main<F>(&mut self, clock_mul: u64, future: F)
    where
        F: Future<Output = ()> + Send + 'static,
    {
        self.main_chip = Some(Chip {
            clock_mul,
            future: Box::pin(future),
        });

        self.next_tick = Instant::now();
        loop {
            if !self.tick() {
                break;
            }
        }
    }

    fn tick(&mut self) -> bool {
        let now = Instant::now();
        if now < self.next_tick {
            thread::sleep(self.next_tick - now);
        }
        self.next_tick += MASTER_TICK_DURATION;

        let mut cx = Context::from_waker(&self.waker);

        let Some(main_chip) = &mut self.main_chip else {
            return false;
        };

        if self.curr_time.is_multiple_of(main_chip.clock_mul)
            && let Poll::Ready(()) = main_chip.future.as_mut().poll(&mut cx)
        {
            self.main_chip = None;
        }

        // Poll side chips and remove completed ones
        self.side_chips = mem::take(&mut self.side_chips)
            .into_iter()
            .filter_map(|mut chip| {
                if !self.curr_time.is_multiple_of(chip.clock_mul) {
                    return Some(chip);
                }

                match chip.future.as_mut().poll(&mut cx) {
                    Poll::Ready(()) => None,
                    Poll::Pending => Some(chip),
                }
            })
            .collect();

        self.curr_time += 1;

        self.main_chip.is_some()
    }
}

pub fn yield_now() -> impl Future<Output = ()> + Send {
    wait_for_cycles(1)
}

pub fn wait_for_cycles(cycles: u64) -> impl Future<Output = ()> + Send {
    struct Waiter {
        remaining_cycles: u64,
    }

    impl Future for Waiter {
        type Output = ();

        fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
            if self.remaining_cycles == 0 {
                Poll::Ready(())
            } else {
                self.get_mut().remaining_cycles -= 1;
                // すぐに次のポーリングがされてほしいので wake() する
                // (我々のランタイムでは常にポーリングするので実際には意味はないが...)
                cx.waker().wake_by_ref();
                Poll::Pending
            }
        }
    }

    Waiter {
        remaining_cycles: cycles,
    }
}

impl Default for Runtime {
    fn default() -> Self {
        Self::new()
    }
}

pub struct Chip {
    clock_mul: u64,
    future: Pin<Box<dyn Future<Output = ()> + Send>>,
}

pub fn block_on<F, T>(future: F) -> T
where
    F: Future<Output = T>,
{
    let mut future = pin!(future);

    let waker = Waker::from(CustomWaker::new());
    let mut cx = Context::from_waker(&waker);

    loop {
        if let Poll::Ready(value) = future.as_mut().poll(&mut cx) {
            return value;
        }
    }
}

struct CustomWaker;

impl CustomWaker {
    fn new() -> Arc<Self> {
        Arc::new(CustomWaker)
    }
}

impl Wake for CustomWaker {
    fn wake(self: Arc<Self>) {
        // todo
    }
}

#[cfg(test)]
mod tests {
    use std::sync::{Arc, Mutex};

    use crate::rt::{self, Runtime};

    #[test]
    fn test_hw_rt_empty() {
        let mut hw = Runtime::new();
        hw.run_main(3, async {});
    }

    #[test]
    fn test_hw_rt_sync_cycles() {
        let log = Arc::new(Mutex::new(vec![]));
        let cycles = Arc::new(Mutex::new(0u64));

        let mut hw = Runtime::new();

        hw.add_chip(1, {
            let cycles = Arc::clone(&cycles);
            async move {
                loop {
                    *cycles.lock().unwrap() += 1;
                    rt::yield_now().await;
                }
            }
        });

        hw.add_chip(5, {
            let log = Arc::clone(&log);
            async move {
                log.lock().unwrap().push("side start");
                loop {
                    log.lock().unwrap().push("side tick");
                    rt::wait_for_cycles(1).await;
                }
            }
        });

        hw.run_main(3, {
            let log = Arc::clone(&log);
            async move {
                log.lock().unwrap().push("main start");
                for _ in 0..5 {
                    log.lock().unwrap().push("main tick");
                    rt::wait_for_cycles(1).await;
                }
            }
        });

        assert_eq!(
            log.lock().unwrap().as_slice(),
            [
                "main start",
                "main tick",
                "side start",
                "side tick",
                "main tick",
                "side tick",
                "main tick",
                "main tick",
                "side tick",
                "main tick",
                "side tick",
            ]
        );

        // time == 0 から 15 まで進むので 16 回動作するはず
        assert_eq!(*cycles.lock().unwrap(), 16);
    }
}
