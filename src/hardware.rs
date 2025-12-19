use std::sync::{Arc, Mutex};

use crate::hardware::{
    bus::{Bus, MirroredRange},
    cpu::Cpu,
    memory::Memory,
};

pub mod bus;
pub mod cpu;
pub mod memory;

pub struct Hardware {
    pub cpu: Cpu,

    pub mem: Arc<Mutex<Memory>>,
}

impl Hardware {
    pub fn assemble() -> Self {
        let mut bus = Bus::new();

        let mem = Arc::new(Mutex::new(Memory::new()));
        bus.connect(
            MirroredRange::new(0x0000..0x2000, 0b0000_0111_1111_1111),
            Arc::clone(&mem),
        );

        let cpu = Cpu::new(bus);

        Hardware { cpu, mem }
    }
}
