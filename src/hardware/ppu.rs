use log::warn;

use crate::hardware::bus::{Bus, Peripheral};

bitflags::bitflags! {
    #[derive(Debug, Clone, Copy, Eq, PartialEq)]
    pub struct Controller : u8 {
        const NAMETABLE_X = 0b0000_0001;
        const NAMETABLE_Y = 0b0000_0010;
        const VRAM_ADDRESS_INC = 0b0000_0100;
        const SPRITE_PATTERN_ADDR = 0b0000_1000;
        const BACKGROUND_PATTERN_ADDR = 0b0001_0000;
        const SPRITE_SIZE = 0b0010_0000;
        const MASTER_SLAVE = 0b0100_0000;
        const GENERATE_NMI = 0b1000_0000;
    }
}

pub struct Ppu {
    bus: Bus,

    /// $2000
    controller: Controller,

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
    // The write itself is 8 bits, but it forms a 16-bit address
    // (first write is the high byte, second write is the low byte)
    address: Address,

    /// $2007
    data: u8,

    /// $4014
    oam_dma: u8,
}

impl Ppu {
    pub fn new(bus: Bus) -> Self {
        Ppu {
            bus,
            controller: Controller::empty(),
            mask: 0,
            status: 0,
            oam_address: 0,
            oam_data: 0,
            scroll: 0,
            address: Address::new(),
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
            0x2 => self.status,
            0x3 => self.oam_address,
            0x4 => self.oam_data,
            0x6 => (self.address.get() & 0x00ff) as u8, // Return low byte of address
            0x7 => {
                // For the first read, PPU returns the data at the current address
                let res = self.data;
                // Then it loads new data from the address preparing for the next read
                self.data = self.bus.read(self.address.get());
                // Finally, it increments the address
                self.address
                    .increment(self.controller.contains(Controller::VRAM_ADDRESS_INC));

                res
            }
            0x0 | 0x1 | 0x5 => {
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
            0x0 => self.controller = Controller::from_bits_truncate(value),
            0x1 => self.mask = value,
            0x3 => self.oam_address = value,
            0x4 => self.oam_data = value,
            0x5 => self.scroll = value,
            0x6 => self.address.update(value),
            0x7 => self.data = value,
            0x2 => {
                warn!("PPU: Write to read-only address {:04X}", address);
            }
            _ => {
                warn!("PPU: Write to unknown address {:04X}", address);
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Address {
    value: u16,
    high_bits: bool,
}

impl Address {
    pub fn new() -> Self {
        Address {
            value: 0,
            high_bits: true,
        }
    }

    pub fn update(&mut self, byte: u8) {
        if self.high_bits {
            self.value = (u16::from(byte) << 8) | (self.value & 0x00ff);
        } else {
            self.value = (self.value & 0xff00) | u16::from(byte);
        }

        self.high_bits = !self.high_bits;
    }

    pub fn set(&mut self, value: u16) {
        self.value = value;
        self.high_bits = true;
    }

    pub fn get(self) -> u16 {
        self.value
    }

    pub fn increment(&mut self, vram_address_inc_set: bool) {
        if vram_address_inc_set {
            self.value = self.value.wrapping_add(32);
        } else {
            self.value = self.value.wrapping_add(1);
        }
    }
}

impl Default for Address {
    fn default() -> Self {
        Self::new()
    }
}
