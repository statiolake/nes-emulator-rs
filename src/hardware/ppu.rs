use log::warn;

use crate::hardware::{
    bus::{Bus, Connect, Peripheral},
    rom::ScreenMirroring,
};

const PPU_CLOCK_MUL: usize = 4;
pub const DISPLAY_WIDTH: usize = 256;
pub const DISPLAY_HEIGHT: usize = 240;

#[rustfmt::skip]
pub static SYSTEM_PALLETE: [(u8,u8,u8); 64] = [
   (0x80, 0x80, 0x80), (0x00, 0x3D, 0xA6), (0x00, 0x12, 0xB0), (0x44, 0x00, 0x96), (0xA1, 0x00, 0x5E),
   (0xC7, 0x00, 0x28), (0xBA, 0x06, 0x00), (0x8C, 0x17, 0x00), (0x5C, 0x2F, 0x00), (0x10, 0x45, 0x00),
   (0x05, 0x4A, 0x00), (0x00, 0x47, 0x2E), (0x00, 0x41, 0x66), (0x00, 0x00, 0x00), (0x05, 0x05, 0x05),
   (0x05, 0x05, 0x05), (0xC7, 0xC7, 0xC7), (0x00, 0x77, 0xFF), (0x21, 0x55, 0xFF), (0x82, 0x37, 0xFA),
   (0xEB, 0x2F, 0xB5), (0xFF, 0x29, 0x50), (0xFF, 0x22, 0x00), (0xD6, 0x32, 0x00), (0xC4, 0x62, 0x00),
   (0x35, 0x80, 0x00), (0x05, 0x8F, 0x00), (0x00, 0x8A, 0x55), (0x00, 0x99, 0xCC), (0x21, 0x21, 0x21),
   (0x09, 0x09, 0x09), (0x09, 0x09, 0x09), (0xFF, 0xFF, 0xFF), (0x0F, 0xD7, 0xFF), (0x69, 0xA2, 0xFF),
   (0xD4, 0x80, 0xFF), (0xFF, 0x45, 0xF3), (0xFF, 0x61, 0x8B), (0xFF, 0x88, 0x33), (0xFF, 0x9C, 0x12),
   (0xFA, 0xBC, 0x20), (0x9F, 0xE3, 0x0E), (0x2B, 0xF0, 0x35), (0x0C, 0xF0, 0xA4), (0x05, 0xFB, 0xFF),
   (0x5E, 0x5E, 0x5E), (0x0D, 0x0D, 0x0D), (0x0D, 0x0D, 0x0D), (0xFF, 0xFF, 0xFF), (0xA6, 0xFC, 0xFF),
   (0xB3, 0xEC, 0xFF), (0xDA, 0xAB, 0xEB), (0xFF, 0xA8, 0xF9), (0xFF, 0xAB, 0xB3), (0xFF, 0xD2, 0xB0),
   (0xFF, 0xEF, 0xA6), (0xFF, 0xF7, 0x9C), (0xD7, 0xE8, 0x95), (0xA6, 0xED, 0xAF), (0xA2, 0xF2, 0xDA),
   (0x99, 0xFF, 0xFC), (0xDD, 0xDD, 0xDD), (0x11, 0x11, 0x11), (0x11, 0x11, 0x11)
];

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

pub struct Frame {
    pub data: Vec<u8>,
}

impl Frame {
    const WIDTH: usize = 256;
    const HEIGHT: usize = 240;

    pub fn new() -> Self {
        Self {
            data: vec![0; Self::WIDTH * Self::HEIGHT * 3],
        }
    }
}

impl Default for Frame {
    fn default() -> Self {
        Self::new()
    }
}
