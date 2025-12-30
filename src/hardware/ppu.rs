use log::warn;

use crate::hardware::bus::{Bus, Peripheral};

pub struct Ppu {
    _bus: Bus,

    /// $2000
    controller: u8,

    /// $2001
    mask: u8,

    /// $2002
    status: u8,

    /// $2003
    oam_address: u8,

    /// $2004
    oam_data: u8,

    /// $2005
    scroll: u8,

    /// $2006
    address: u8,

    /// $2007
    data: u8,

    /// $4014
    oam_dma: u8,
}

impl Ppu {
    pub fn new(bus: Bus) -> Self {
        Ppu {
            _bus: bus,
            controller: 0,
            mask: 0,
            status: 0,
            oam_address: 0,
            oam_data: 0,
            scroll: 0,
            address: 0,
            data: 0,
            oam_dma: 0,
        }
    }

    pub fn step(&mut self) {
        // TODO
    }
}

impl Peripheral for Ppu {
    fn read(&mut self, address: u16) -> u8 {
        if address == 0x4014 {
            return self.oam_dma;
        }

        match (address - 0x2000) % 8 {
            0x0 => self.controller,
            0x1 => self.mask,
            0x2 => self.status,
            0x3 => self.oam_address,
            0x4 => self.oam_data,
            0x5..=0x7 => {
                warn!("PPU: Read from write-only address {:04X}", address);
                0
            }
            _ => {
                warn!("PPU: Read from unknown address {:04X}", address);
                0
            }
        }
    }

    fn write(&mut self, address: u16, value: u8) {
        if address == 0x4014 {
            self.oam_dma = value;
            return;
        }

        match (address - 0x2000) % 8 {
            0x0 => self.controller = value,
            0x1 => self.mask = value,
            0x2 => {
                warn!("PPU: Write to read-only address {:04X}", address);
            }
            0x3 => self.oam_address = value,
            0x4 => self.oam_data = value,
            0x5 => self.scroll = value,
            0x6 => self.address = value,
            0x7 => self.data = value,
            _ => {
                warn!("PPU: Write to unknown address {:04X}", address);
            }
        }
    }
}
