pub struct Clock {
    callbacks: Vec<Box<dyn FnMut() + Send + Sync + 'static>>,
}

impl Clock {
    pub fn new() -> Self {
        Clock { callbacks: vec![] }
    }

    pub fn register_callback<F>(&mut self, callback: F)
    where
        F: FnMut() + Send + Sync + 'static,
    {
        self.callbacks.push(Box::new(callback));
    }

    pub fn tick(&mut self) {
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
