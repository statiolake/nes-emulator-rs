use std::cell::Cell;

use crate::rt;

#[derive(Debug)]
pub struct Bus {
    pub state: Cell<BusState>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum BusState {
    Empty,
    Read {
        address: u16,
    },
    ReadComplete {
        // who responds this request: for debugging purpose
        responder: &'static str,

        address: u16,
        data: u8,
    },
    Write {
        address: u16,
        value: u8,
    },
    WriteComplete {
        // who responds this request: for debugging purpose
        responder: &'static str,

        address: u16,
        value: u8,
    },
}

impl Bus {
    pub fn new() -> Self {
        Bus {
            state: Cell::new(BusState::Empty),
        }
    }

    pub async fn read(&self, address: u16) -> u8 {
        let state = self.state.get();
        if let BusState::Empty = state {
            panic!("previous bus operation is not correctly finished: {state:?}");
        }
        self.state.set(BusState::Read { address });

        // Chips must be respond within this one cycle.
        rt::yield_now().await;

        let state = self.state.get();
        let BusState::ReadComplete { data, .. } = state else {
            panic!("chip does not respond to the read request: {state:?}");
        };

        self.state.set(BusState::Empty);

        data
    }

    pub async fn read_u16(&self, address: u16) -> u16 {
        let lo = self.read(address).await;
        let hi = self.read(address.wrapping_add(1)).await;
        u16::from_le_bytes([lo, hi])
    }

    pub async fn write(&self, address: u16, value: u8) {
        let state = self.state.get();
        if let BusState::Empty = state {
            panic!("previous bus operation is not correctly finished: {state:?}");
        }

        self.state.set(BusState::Write { address, value });

        // Chips must be respond within this one cycle.
        rt::yield_now().await;

        let state = self.state.get();
        let BusState::WriteComplete { .. } = state else {
            panic!("chip does not respond to the read request: {state:?}");
        };

        self.state.set(BusState::Empty);
    }

    pub fn write_u16(&self, bus_addr: u16, value: u16) {
        let bytes = value.to_le_bytes();
        self.write(bus_addr, bytes[0]);
        self.write(bus_addr.wrapping_add(1), bytes[1]);
    }
}

// pub trait Peripheral {
//     fn read(&mut self, address: u16) -> u8;
//     fn write(&mut self, address: u16, value: u8);
// }
//
// impl<P: Peripheral> Peripheral for Arc<Mutex<P>> {
//     fn read(&mut self, address: u16) -> u8 {
//         let mut peripheral = self.lock().expect("failed to get lock on peripheral");
//         peripheral.read(address)
//     }
//
//     fn write(&mut self, address: u16, value: u8) {
//         let mut peripheral = self.lock().expect("failed to get lock on peripheral");
//         peripheral.write(address, value);
//     }
// }
//
// pub trait Connect {
//     fn bus_addr_range(&self) -> RangeInclusive<u16>;
//     fn to_device_addr(&self, address: u16) -> u16;
// }
//
// impl Connect for RangeInclusive<u16> {
//     fn bus_addr_range(&self) -> RangeInclusive<u16> {
//         *self.start()..=*self.end()
//     }
//
//     fn to_device_addr(&self, address: u16) -> u16 {
//         address - self.start()
//     }
// }
//
// pub struct MirroredRange {
//     pub base_range: RangeInclusive<u16>,
//     pub target_mirror_mask: u16,
// }
//
// impl MirroredRange {
//     pub fn new(base_range: RangeInclusive<u16>, target_mirror_mask: u16) -> Self {
//         MirroredRange {
//             base_range,
//             target_mirror_mask,
//         }
//     }
// }
//
// impl Connect for MirroredRange {
//     fn bus_addr_range(&self) -> RangeInclusive<u16> {
//         *self.base_range.start()..=*self.base_range.end()
//     }
//
//     fn to_device_addr(&self, bus_addr: u16) -> u16 {
//         (bus_addr - self.base_range.start()) & self.target_mirror_mask
//     }
// }
//
// pub struct IdentityRange {
//     pub range: RangeInclusive<u16>,
// }
//
// impl IdentityRange {
//     pub fn new(range: RangeInclusive<u16>) -> Self {
//         IdentityRange { range }
//     }
// }
//
// impl Connect for IdentityRange {
//     fn bus_addr_range(&self) -> RangeInclusive<u16> {
//         *self.range.start()..=*self.range.end()
//     }
//
//     fn to_device_addr(&self, bus_addr: u16) -> u16 {
//         bus_addr
//     }
// }
//
// pub struct Bus {
//     peripherals: Vec<(
//         Box<dyn Connect + Send + Sync>,
//         Box<dyn Peripheral + Send + Sync>,
//     )>,
// }
//
// impl Bus {
//     pub fn new() -> Self {
//         Bus {
//             peripherals: Vec::new(),
//         }
//     }
//
//     pub fn connect<C, P>(&mut self, conn: C, peri: P)
//     where
//         C: Connect + Send + Sync + 'static,
//         P: Peripheral + Send + Sync + 'static,
//     {
//         self.peripherals.push((Box::new(conn), Box::new(peri)));
//     }
//
//     pub fn read(&mut self, bus_addr: u16) -> u8 {
//         for (conn, peri) in &mut self.peripherals {
//             let range = conn.bus_addr_range();
//             if range.contains(&bus_addr) {
//                 let device_address = conn.to_device_addr(bus_addr);
//                 return peri.read(device_address);
//             }
//         }
//
//         warn!("Read from unmapped address: {:#06X}", bus_addr);
//         0
//     }
//
//     pub fn read_u16(&mut self, bus_addr: u16) -> u16 {
//         let lo = self.read(bus_addr);
//         let hi = self.read(bus_addr.wrapping_add(1));
//         u16::from_le_bytes([lo, hi])
//     }
//
//     pub fn write(&mut self, bus_addr: u16, value: u8) {
//         for (conn, peri) in &mut self.peripherals {
//             let range = conn.bus_addr_range();
//             if range.contains(&bus_addr) {
//                 let device_address = conn.to_device_addr(bus_addr);
//                 peri.write(device_address, value);
//                 return;
//             }
//         }
//
//         warn!("Write {} to unmapped address: {:#06X}", value, bus_addr);
//     }
//
//     pub fn write_u16(&mut self, bus_addr: u16, value: u16) {
//         let bytes = value.to_le_bytes();
//         self.write(bus_addr, bytes[0]);
//         self.write(bus_addr.wrapping_add(1), bytes[1]);
//     }
// }
//
// impl Default for Bus {
//     fn default() -> Self {
//         Self::new()
//     }
// }
