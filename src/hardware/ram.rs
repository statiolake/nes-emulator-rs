use std::{cell::Cell, ops::RangeInclusive, rc::Rc};

use itertools::izip;

use crate::{
    hardware::bus::{Bus, BusState},
    rt,
};

pub struct Ram {
    data: Vec<Cell<u8>>,

    bus: Rc<Bus>,
    address_range: RangeInclusive<u16>,
    address_mask: u16,
}

pub struct RamMountOptions {
    pub address_range: RangeInclusive<u16>,
    pub address_mask: u16,
}

impl Ram {
    pub fn mount(bus: Rc<Bus>, opts: RamMountOptions) -> Self {
        let RamMountOptions {
            address_range,
            address_mask,
        } = opts;

        let size = *address_range.end() as usize - *address_range.start() as usize + 1;

        Ram {
            data: vec![Cell::new(0); size],

            bus,
            address_range,
            address_mask,
        }
    }

    pub async fn run(&self) {
        let step = move || match self.bus.state.get() {
            BusState::Read { address } => {
                if !self.address_range.contains(&address) {
                    return;
                }

                let data = self.read(address);
                self.bus.state.set(BusState::ReadComplete {
                    responder: "ram",
                    address,
                    data,
                });
            }
            BusState::Write { address, value } => {
                if !self.address_range.contains(&address) {
                    return;
                }

                self.write(address, value);
                self.bus.state.set(BusState::WriteComplete {
                    responder: "ram",
                    address,
                    value,
                })
            }
            _ => return,
        };

        loop {
            step();
            rt::yield_now().await;
        }
    }

    pub fn load(&self, start_address: u16, data: &[u8]) {
        let end_address = start_address
            .checked_add(data.len() as u16)
            .expect("data too large to load into RAM");

        self.assert_address_range(start_address);
        self.assert_address_range(end_address);

        let start_index = start_address - self.address_range.start();
        let end_index = end_address - self.address_range.end();

        for (target, &byte) in izip!(&self.data[start_index as usize..end_index as usize], data) {
            target.set(byte);
        }
    }

    pub fn read(&self, address: u16) -> u8 {
        self.assert_address_range(address);
        let address = address & self.address_mask;

        self.data[address as usize].get()
    }

    pub fn read_u16(&self, address: u16) -> u16 {
        let lo = self.read(address);
        let hi = self.read(address + 1);
        u16::from_le_bytes([lo, hi])
    }

    pub fn write(&self, address: u16, value: u8) {
        self.assert_address_range(address);
        let address = address & self.address_mask;
        self.data[address as usize].set(value);
    }

    pub fn write_u16(&self, address: u16, value: u16) {
        let bytes = value.to_le_bytes();
        self.write(address, bytes[0]);
        self.write(address + 1, bytes[1]);
    }

    fn assert_address_range(&self, address: u16) {
        if !self.address_range.contains(&address) || address != address & self.address_mask {
            panic!(
                "address out of bounds: {:x} is not in {:x}..={:x} with mask {:x}",
                address,
                self.address_range.start(),
                self.address_range.end(),
                self.address_mask,
            );
        }
    }
}

pub struct CpuRam {
    pub inner: Ram,
}

impl CpuRam {
    pub fn mount(cpu_bus: Rc<Bus>) -> CpuRam {
        CpuRam {
            inner: Ram::mount(
                cpu_bus,
                RamMountOptions {
                    address_range: 0x0000..=0x1fff,
                    address_mask: 0b0000_0111_1111_1111,
                },
            ),
        }
    }
}
