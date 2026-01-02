use std::{
    thread,
    time::{Duration, Instant},
};

const MASTER_HZ: u64 = 21_477_272; // 21.477272 MHz

pub struct Clock {
    callbacks: Vec<Box<dyn FnMut() + Send + Sync + 'static>>,
    next_timing: Instant,
}

impl Clock {
    pub fn new() -> Self {
        Clock {
            callbacks: vec![],
            next_timing: Instant::now(),
        }
    }

    pub fn register_callback<F>(&mut self, callback: F)
    where
        F: FnMut() + Send + Sync + 'static,
    {
        self.callbacks.push(Box::new(callback));
    }

    pub fn tick(&mut self) {
        self.next_timing += Duration::from_nanos(1_000_000_000 / MASTER_HZ);
        thread::sleep(self.next_timing.saturating_duration_since(Instant::now()));
        for callback in &mut self.callbacks {
            callback();
        }
    }
}

impl Default for Clock {
    fn default() -> Self {
        Self::new()
    }
}
