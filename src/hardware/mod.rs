use std::sync::{Arc, Mutex};

use crate::hardware::{
    bus::{Bus, MirroredRange},
    cpu::Cpu,
    ram::Ram,
    rom::Rom,
};

pub mod bus;
pub mod cpu;
pub mod ram;
pub mod rom;

pub struct Hardware {
    pub cpu: Arc<Mutex<Cpu>>,

    pub ram: Arc<Mutex<Ram>>,
    pub rom: Arc<Mutex<Rom>>,
}

impl Hardware {
    pub fn assemble(rom: Rom) -> Self {
        let mut bus = Bus::new();

        let ram = Arc::new(Mutex::new(Ram::new()));
        bus.connect(
            MirroredRange::new(0x0000..=0x1fff, 0b0000_0111_1111_1111),
            Arc::clone(&ram),
        );

        let rom = Arc::new(Mutex::new(rom));
        bus.connect(0x8000..=0xffff, Arc::clone(&rom));

        let cpu = Arc::new(Mutex::new(Cpu::new(bus)));

        Hardware { cpu, ram, rom }
    }

    pub fn power_on(&self) {
        self.cpu.lock().unwrap().reset();
    }
}
