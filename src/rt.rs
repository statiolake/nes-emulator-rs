//! A minimal async runtime to cooperatively execute each chips of NES emulator.
//!
//! reference: https://techracho.bpsinc.jp/yoshi/2024_12_13/147377

use std::{
    pin::Pin,
    sync::Arc,
    task::{Context, Poll, Wake, Waker},
    thread,
    time::{Duration, Instant},
};

const MASTER_HZ: u64 = 21_477_272; // 21.477272 MHz
const SYNC_HZ: u64 = 60; // synchronize at 60 Hz

const SYNC_INTERVAL_TICKS: u64 = MASTER_HZ / SYNC_HZ;
const SYNC_DURATION: Duration = Duration::from_nanos(1_000_000_000 / SYNC_HZ);

pub struct Runtime {
    waker: Arc<Waker>,
    curr_time: u64,
    next_sync: Instant,
}

pub struct Schedule<T> {
    main: Task<T>,
    subs: Vec<Task<()>>,
}

pub enum Task<T> {
    Running(ClockedFuture<T>),
    Finished,
}

pub struct ClockedFuture<T> {
    pub clock_mul: u64,
    pub future: Pin<Box<dyn Future<Output = T>>>,
}

impl Runtime {
    pub fn new() -> Self {
        Self {
            waker: Arc::new(Waker::from(CustomWaker::new())),
            curr_time: 0,
            next_sync: Instant::now(),
        }
    }

    pub fn run<T>(&mut self, mut sched: Schedule<T>) -> T {
        self.curr_time = 0;
        self.next_sync = Instant::now();

        loop {
            self.sync();
            if let Ok(ret) = self.run_step(&mut sched) {
                break ret;
            }
        }
    }

    fn sync(&mut self) {
        if self.curr_time.is_multiple_of(SYNC_INTERVAL_TICKS) {
            let now = Instant::now();
            if now < self.next_sync {
                thread::sleep(self.next_sync - now);
            }

            self.next_sync += SYNC_DURATION;
        }
    }

    fn run_step<T>(&mut self, sched: &mut Schedule<T>) -> Result<T, ()> {
        let mut cx = Context::from_waker(&self.waker);

        // Poll main and side chips and remove completed ones
        let ret = sched.main.step(self.curr_time, &mut cx);
        for task in &mut sched.subs {
            task.step(self.curr_time, &mut cx);
        }

        self.curr_time += 1;

        ret
    }
}

impl<T> Schedule<T> {
    pub fn new() -> Self {
        Schedule {
            main: Task::Finished,
            subs: vec![],
        }
    }

    pub fn with_main(mut self, main: ClockedFuture<T>) -> Self {
        self.main = Task::Running(main);
        self
    }

    pub fn with_sub(mut self, sub: ClockedFuture<()>) -> Self {
        self.subs.push(Task::Running(sub));
        self
    }
}

impl<T> Task<T> {
    fn step(&mut self, curr_time: u64, cx: &mut Context<'_>) -> Result<T, ()> {
        match self {
            Self::Finished => panic!("step() called after finish"),
            Self::Running(task) => {
                if curr_time.is_multiple_of(task.clock_mul)
                    && let Poll::Ready(ret) = task.future.as_mut().poll(cx)
                {
                    // Remove as completed
                    *self = Self::Finished;
                    Ok(ret)
                } else {
                    Err(())
                }
            }
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

    use super::*;
    use crate::rt;

    #[test]
    fn test_hw_rt_empty() {
        let mut rt = Runtime::new();
        rt.run(Schedule::new().with_main(ClockedFuture {
            clock_mul: 3,
            future: Box::pin(async {}),
        }));
    }

    #[test]
    fn test_hw_rt_sync_cycles() {
        let log = Arc::new(Mutex::new(vec![]));
        let cycles = Arc::new(Mutex::new(0u64));

        let mut rt = Runtime::new();

        let sched = Schedule::new()
            .with_main(ClockedFuture {
                clock_mul: 1,
                future: {
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
                future: {
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
                future: {
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
