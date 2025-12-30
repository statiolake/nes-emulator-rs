use log::warn;

use crate::hardware::{
    bus::{Bus, Connect, Peripheral},
    rom::ScreenMirroring,
};

const PPU_CLOCK_MUL: usize = 4;

pub struct Ppu {
    ticks_to_wait: usize,

    bus: Bus,
    vblank_start_callbacks: Vec<Box<dyn FnMut() + Send + Sync>>,

    /// $2000
    control: Control,

    /// $2001
    mask: u8,

    /// $2002
    status: Status,

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
    data_buf: u8,

    /// $4014
    oam_dma: u8,
}

impl Ppu {
    pub fn new(bus: Bus) -> Self {
        Ppu {
            ticks_to_wait: 0,
            bus,
            vblank_start_callbacks: vec![],
            control: Control::empty(),
            mask: 0,
            status: Status::empty(),
            oam_address: 0,
            oam_data: 0,
            scroll: 0,
            address: Address::new(),
            data_buf: 0,
            oam_dma: 0,
        }
    }

    pub fn tick(&mut self) {
        if self.ticks_to_wait != 0 {
            self.ticks_to_wait -= 1;
            return;
        }

        self.step();
    }

    pub fn step(&mut self) {
        // TODO
        self.ticks_to_wait = PPU_CLOCK_MUL - 1;
    }

    pub fn register_vblank_start_callback(
        &mut self,
        callback: impl FnMut() + Send + Sync + 'static,
    ) {
        self.vblank_start_callbacks.push(Box::new(callback));
    }

    fn trigger_vblank_start(&mut self) {
        for callback in &mut self.vblank_start_callbacks {
            callback();
        }
    }
}

impl Peripheral for Ppu {
    fn read(&mut self, address: u16) -> u8 {
        if address == 0x4014 {
            return self.oam_dma;
        }

        match (address - 0x2000) % 8 {
            0x2 => self.status.bits(),
            0x3 => self.oam_address,
            0x4 => self.oam_data,
            0x6 => (self.address.get() & 0x00ff) as u8, // Return low byte of address
            0x7 => {
                // For the first read, PPU returns the data at the current address
                let res = self.data_buf;
                // Then it loads new data from the address preparing for the next read
                self.data_buf = self.bus.read(self.address.get());
                // Finally, it increments the address
                self.address
                    .increment(self.control.contains(Control::VRAM_ADDRESS_INC));

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
            0x0 => {
                let old_control = self.control;
                self.control = Control::from_bits_truncate(value);
                // If PPU is VBLANK and NMI generation is just enabled, trigger NMI
                if self.status.contains(Status::VBLANK)
                    && !old_control.contains(Control::GENERATE_NMI)
                    && self.control.contains(Control::GENERATE_NMI)
                {
                    self.trigger_vblank_start();
                }
            }
            0x1 => self.mask = value,
            0x3 => self.oam_address = value,
            0x4 => self.oam_data = value,
            0x5 => self.scroll = value,
            0x6 => self.address.update(value),
            0x7 => self.data_buf = value,
            0x2 => {
                warn!("PPU: Write to read-only address {:04X}", address);
            }
            _ => {
                warn!("PPU: Write to unknown address {:04X}", address);
            }
        }
    }
}

bitflags::bitflags! {
    #[derive(Debug, Clone, Copy, Eq, PartialEq)]
    pub struct Control : u8 {
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

bitflags::bitflags! {
    #[derive(Debug, Clone, Copy, Eq, PartialEq)]
    pub struct Status : u8 {
        const SPRITE_OVERFLOW = 0b0010_0000;
        const SPRITE_ZERO_HIT = 0b0100_0000;
        const VBLANK = 0b1000_0000;
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

pub struct PpuRamConnector {
    screen_mirroring: ScreenMirroring,
}

impl PpuRamConnector {
    pub fn new(screen_mirroring: ScreenMirroring) -> Self {
        PpuRamConnector { screen_mirroring }
    }
}

impl Connect for PpuRamConnector {
    fn bus_addr_range(&self) -> std::ops::RangeInclusive<u16> {
        0x2000..=0x3fff
    }

    fn to_device_addr(&self, address: u16) -> u16 {
        let (nametable_index, address) = ((address & 0x0c00) >> 10, address & 0x03ff);
        match self.screen_mirroring {
            ScreenMirroring::Horizontal => match nametable_index {
                0 | 1 => address,
                2 | 3 => address + 0x0400,
                _ => unreachable!(),
            },
            ScreenMirroring::Vertical => match nametable_index {
                0 | 2 => address,
                1 | 3 => address + 0x0400,
                _ => unreachable!(),
            },
            ScreenMirroring::FourScreen => {
                panic!("Four-screen mirroring not supported")
            }
        }
    }
}
