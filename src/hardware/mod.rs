use std::{
    rc::Rc,
    sync::mpsc::{self, Receiver},
};

use crate::{
    hardware::{
        bus::Bus,
        cpu::{Cpu, CpuMountOptions},
        ram::CpuRam,
        rom::Rom,
    },
    rt::{ClockedFuture, Schedule},
};

pub mod bus;
pub mod clock;
pub mod cpu;
pub mod ppu;
pub mod ram;
pub mod rom;

pub struct Hardware {
    pub cpu_bus: Rc<Bus>,
    pub cpu: Rc<Cpu>,
    pub cpu_debug_rx: Receiver<String>,
    pub cpu_ram: Rc<CpuRam>,
    // pub rom_slot: RomSlot,
}

impl Hardware {
    pub fn assemble() -> Self {
        let cpu_bus = Rc::new(Bus::new());

        let (cpu_debug_tx, cpu_debug_rx) = mpsc::channel();
        let cpu = Rc::new(Cpu::mount(
            Rc::clone(&cpu_bus),
            CpuMountOptions {
                debug_tx: Some(cpu_debug_tx),
            },
        ));
        let cpu_ram = Rc::new(CpuRam::mount(Rc::clone(&cpu_bus)));

        // let rom_slot = RomSlot::mount(Arc::clone(&cpu_bus));

        Hardware {
            cpu_bus,
            cpu,
            cpu_ram,
            cpu_debug_rx,
            // rom_slot,
        }
    }

    pub fn insert(&self, rom: Rom) {
        self.rom_slot.insert(rom)
    }

    pub fn eject(&self) -> Option<Rom> {
        self.rom_slot.eject()
    }

    pub fn to_schedule(&self) -> Schedule<()> {
        Schedule::new()
            .with_main(ClockedFuture {
                clock_mul: 12,
                future: Box::pin({
                    let cpu = Rc::clone(&self.cpu);
                    async move {
                        cpu.interrupt_reset().await;
                        cpu.run().await
                    }
                }),
            })
            .with_sub(ClockedFuture {
                clock_mul: 1,
                future: Box::pin({
                    let mem = Rc::clone(&self.cpu_ram);
                    async move { mem.inner.run().await }
                }),
            })
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
}
