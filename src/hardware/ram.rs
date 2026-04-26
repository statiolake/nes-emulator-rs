use std::sync::Arc;

use crate::hardware::bus::{Bus, Peripheral};

pub struct Ram {
    data: Vec<u8>,
}

impl Ram {
    pub fn mount(size: usize) -> Self {
        Ram {
            data: vec![0; size],
        }
    }

    pub fn load(&mut self, start_address: u16, program: &[u8]) {
        self.data[start_address as usize..(start_address as usize + program.len())]
            .copy_from_slice(program);
    }

    pub fn read(&mut self, address: u16) -> u8 {
        self.data[address as usize]
    }

    pub fn read_u16(&mut self, address: u16) -> u16 {
        let lo = self.read(address);
        let hi = self.read(address + 1);
        u16::from_le_bytes([lo, hi])
    }

    pub fn write(&mut self, address: u16, value: u8) {
        self.data[address as usize] = value;
    }

    pub fn write_u16(&mut self, address: u16, value: u16) {
        let bytes = value.to_le_bytes();
        self.write(address, bytes[0]);
        self.write(address + 1, bytes[1]);
    }
}

impl Peripheral for Ram {
    fn read(&mut self, address: u16) -> u8 {
        self.read(address)
    }

    fn write(&mut self, address: u16, value: u8) {
        self.write(address, value);
    }
}

pub struct CpuRam(Ram);

impl CpuRam {
    pub fn mount(cpu_bus: Arc<Bus>) -> CpuRam {
        CpuRam(Ram::mount(cpu_bus, 0x0000..=0x1fff, 0b0000_0111_1111_1111))
    }
}
