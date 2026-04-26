use std::sync::Arc;

use crate::{
    hardware::{
        bus::{Bus, IdentityRange, MirroredRange},
        cpu::Cpu,
        ram::{CpuRam, Ram},
        rom::Rom,
    },
    rt::{self, Runtime},
};

pub mod bus;
pub mod clock;
pub mod cpu;
pub mod ppu;
pub mod ram;
pub mod rom;

pub struct Hardware {
    cpu_bus: Arc<Bus>,
    cpu: Cpu,
    cpu_ram: CpuRam,

    rom_slot: RomSlot,
}

impl Hardware {
    pub fn assemble() -> Self {
        let cpu_bus = Arc::new(Bus::new());

        let cpu = Cpu::mount(Arc::clone(&cpu_bus));
        let cpu_ram = CpuRam::mount(Arc::clone(&cpu_bus));

        let rom_slot = RomSlot::mount(Arc::clone(&cpu_bus));

        Hardware {
            cpu_bus,
            cpu,
            cpu_ram,

            rom_slot,
        }
    }

    pub fn insert(&mut self, rom: Rom) {
        self.rom_slot.insert(rom)
    }

    pub fn eject(&mut self) -> Option<Rom> {
        self.rom_slot.eject()
    }

    //     fn assemble_ppu(
    //         rom: Arc<Mutex<Rom>>,
    //         ram: Arc<Mutex<Ram>>,
    //         palette: Arc<Mutex<Ram>>,
    //     ) -> Arc<Mutex<Ppu>> {
    //         let screen_mirroring = {
    //             let rom = rom.lock().unwrap();
    //             rom.screen_mirroring()
    //         };
    //         let mut ppu_bus = Bus::new();
    //
    //         ppu_bus.connect(0x0000..=0x1fff, Rom::as_chr_peri(rom));
    //         ppu_bus.connect(PpuRamConnector::new(screen_mirroring), ram);
    //         ppu_bus.connect(0x3f00..=0x3fff, palette);
    //         // TODO: connect other hardwares to PPU bus
    //
    //         Arc::new(Mutex::new(Ppu::new(ppu_bus)))
    //     }

    //     fn assemble_cpu(
    //         ram: Arc<Mutex<Ram>>,
    //         rom: Arc<Mutex<Rom>>,
    //         ppu: Arc<Mutex<Ppu>>,
    //     ) -> Arc<Mutex<Cpu>> {
    //         use {IdentityRange as IR, MirroredRange as MR};
    //         let mut cpu_bus = Bus::new();
    //         cpu_bus.connect(MR::new(0x0000..=0x1fff, 0b0000_0111_1111_1111), ram);
    //         // Memory-mapped PPU registers
    //         cpu_bus.connect(IR::new(0x2000..=0x3fff), Arc::clone(&ppu));
    //         cpu_bus.connect(IR::new(0x4014..=0x4014), Arc::clone(&ppu)); // DMA
    //         cpu_bus.connect(0x8000..=0xffff, Rom::as_prg_peri(rom));
    //
    //         Arc::new(Mutex::new(Cpu::mount(cpu_bus)))
    //     }

    //     fn connect_nmi_pins(cpu: Arc<Mutex<Cpu>>, ppu: Arc<Mutex<Ppu>>) {
    //         ppu.lock().unwrap().register_vblank_start_callback(move || {
    //             cpu.lock().unwrap().interrupt_nmi();
    //         });
    //     }
    //
    //     fn connect_clock_pins(clock: Arc<Mutex<Clock>>, cpu: Arc<Mutex<Cpu>>, ppu: Arc<Mutex<Ppu>>) {
    //         let mut clock = clock.lock().unwrap();
    //
    //         let cpu = Arc::clone(&cpu);
    //         clock.register_callback(move || {
    //             cpu.lock().unwrap().tick();
    //         });
    //
    //         let ppu = Arc::clone(&ppu);
    //         clock.register_callback(move || {
    //             ppu.lock().unwrap().tick();
    //         });
    //     }

    pub fn power_on(&self) {
        self.cpu.lock().unwrap().interrupt_reset();
    }

    pub fn tick(&self) {
        self.clock.lock().unwrap().tick();
    }
}

#[test]
fn test1() {
    let hw = Hardware::assemble();
    let rom = Rom::load();
    hw.insert_rom(rom);
    rt.run(&mut hw);
    let rom = hw.eject_rom();

    rt = Runtime { clock };

    hw = Hardware { cpu, ppu, ram, rom };
}
