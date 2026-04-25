//! A minimal async runtime to cooperatively execute each chips of NES emulator.
//!
//! reference: https://techracho.bpsinc.jp/yoshi/2024_12_13/147377

use std::{
    mem,
    pin::Pin,
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
}

pub struct Schedule {
    main: Option<ClockedFuture>,
    subs: Vec<ClockedFuture>,
}

pub struct ClockedFuture {
    clock_mul: u64,
    fut: Pin<Box<dyn Future<Output = ()>>>,
}

impl Runtime {
    pub fn new() -> Self {
        Self::with_current_time(0)
    }

    pub fn with_current_time(curr_time: u64) -> Self {
        let waker = Arc::new(Waker::from(CustomWaker::new()));
        Self {
            waker,
            curr_time,
            next_tick: Instant::now(),
        }
    }

    pub fn run<F>(&mut self, sched: Schedule) {
        let mut sched = sched;
        self.next_tick = Instant::now();
        loop {
            if !self.run_step(&mut sched) {
                break;
            }
        }
    }

    fn run_step(&mut self, sched: &mut Schedule) -> bool {
        let now = Instant::now();
        if now < self.next_tick {
            thread::sleep(self.next_tick - now);
        }
        self.next_tick += MASTER_TICK_DURATION;

        let mut cx = Context::from_waker(&self.waker);

        let Some(main_task) = &mut sched.main else {
            return false;
        };

        if self.curr_time.is_multiple_of(main_task.clock_mul)
            && let Poll::Ready(()) = main_task.future.as_mut().poll(&mut cx)
        {
            sched.main = None;
        }

        // Poll side chips and remove completed ones
        sched.subs = mem::take(&mut sched.subs)
            .into_iter()
            .filter_map(|mut task| {
                if !self.curr_time.is_multiple_of(task.clock_mul) {
                    return Some(task);
                }

                match task.future.as_mut().poll(&mut cx) {
                    Poll::Ready(()) => None,
                    Poll::Pending => Some(task),
                }
            })
            .collect();

        self.curr_time += 1;

        sched.main_chip.is_some()
    }
}

impl Schedule {
    pub fn new() -> Self {
        Schedule {
            main: None,
            subs: vec![],
        }
    }

    pub fn with_main(mut self, main: ClockedFuture) -> Self {
        self.main = Some(main);
        self
    }

    pub fn with_sub(mut self, sub: ClockedFuture) -> Self {
        self.subs.push(sub);
        self
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

#[cfg(test)]
mod tests {
    use std::sync::{Arc, Mutex};

    use crate::rt::{self, ClockedFuture, Runtime, Schedule};

    #[test]
    fn test_hw_rt_empty() {
        let mut rt = Runtime::new();
        rt.run(Schedule {
            main: Some(ClockedFuture {
                clock_mul: 3,
                fut: Box::pin(async {}),
            }),
            subs: vec![],
        });
    }

    #[test]
    fn test_hw_rt_sync_cycles() {
        let log = Arc::new(Mutex::new(vec![]));
        let cycles = Arc::new(Mutex::new(0u64));

        let mut rt = Runtime::new();

        let sched = Schedule::new()
            .with_main(ClockedFuture {
                clock_mul: 1,
                fut: {
                    let cycles = Arc::clone(&cycles);
                    Box::pin(async move {
                        loop {
                            *cycles.lock().unwrap() += 1;
                            rt::yield_now().await;
                        }
                    })
                },
            })
            .with_sub(ClockedFuture {
                clock_mul: 5,
                fut: {
                    let log = Arc::clone(&log);
                    Box::pin(async move {
                        log.lock().unwrap().push("side start");
                        loop {
                            log.lock().unwrap().push("side tick");
                            rt::wait_for_cycles(1).await;
                        }
                    })
                },
            })
            .with_sub(ClockedFuture {
                clock_mul: 3,
                fut: {
                    let log = Arc::clone(&log);
                    Box::pin(async move {
                        log.lock().unwrap().push("main start");
                        for _ in 0..5 {
                            log.lock().unwrap().push("main tick");
                            rt::wait_for_cycles(1).await;
                        }
                    })
                },
            });

        rt.run(sched);

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
