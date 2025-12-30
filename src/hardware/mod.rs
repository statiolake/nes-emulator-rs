use std::sync::{Arc, Mutex};

use crate::hardware::{
    bus::{Bus, IdentityRange, MirroredRange},
    clock::Clock,
    cpu::Cpu,
    ppu::Ppu,
    ram::Ram,
    rom::Rom,
};

pub mod bus;
pub mod clock;
pub mod cpu;
pub mod ppu;
pub mod ram;
pub mod rom;

pub struct Hardware {
    pub clock: Arc<Mutex<Clock>>,

    pub cpu: Arc<Mutex<Cpu>>,
    pub ppu: Arc<Mutex<Ppu>>,

    pub ram: Arc<Mutex<Ram>>,
    pub rom: Arc<Mutex<Rom>>,
}

impl Hardware {
    pub fn assemble(rom: Rom) -> Self {
        let clock = Arc::new(Mutex::new(Clock::new()));
        let cpu_ram = Arc::new(Mutex::new(Ram::new(2048)));
        let ppu_ram = Arc::new(Mutex::new(Ram::new(2048)));
        let ppu_palette_ram = Arc::new(Mutex::new(Ram::new(32)));
        let rom = Arc::new(Mutex::new(rom));

        let ppu = Self::assemble_ppu(
            Arc::clone(&rom),
            Arc::clone(&ppu_ram),
            Arc::clone(&ppu_palette_ram),
        );
        let cpu = Self::assemble_cpu(Arc::clone(&cpu_ram), Arc::clone(&rom), Arc::clone(&ppu));
        Self::register_clock_callbacks(
            &mut clock.lock().unwrap(),
            Arc::clone(&cpu),
            Arc::clone(&ppu),
        );

        Hardware {
            clock,
            cpu,
            ppu,
            ram: cpu_ram,
            rom,
        }
    }

    fn assemble_ppu(
        rom: Arc<Mutex<Rom>>,
        ram: Arc<Mutex<Ram>>,
        palette: Arc<Mutex<Ram>>,
    ) -> Arc<Mutex<Ppu>> {
        let mut ppu_bus = Bus::new();

        ppu_bus.connect(0x0000..=0x1fff, Rom::as_chr_peri(rom));
        ppu_bus.connect(0x2000..=0x3eff, ram);
        ppu_bus.connect(0x3f00..=0x3fff, palette);
        // TODO: connect other hardwares to PPU bus

        Arc::new(Mutex::new(Ppu::new(ppu_bus)))
    }

    fn assemble_cpu(
        ram: Arc<Mutex<Ram>>,
        rom: Arc<Mutex<Rom>>,
        ppu: Arc<Mutex<Ppu>>,
    ) -> Arc<Mutex<Cpu>> {
        use MirroredRange as MR;
        let mut cpu_bus = Bus::new();
        cpu_bus.connect(MR::new(0x0000..=0x1fff, 0b0000_0111_1111_1111), ram);
        cpu_bus.connect(IdentityRange::new(0x2000..=0x3fff), Arc::clone(&ppu));
        cpu_bus.connect(IdentityRange::new(0x4014..=0x4014), Arc::clone(&ppu)); // DMA
        cpu_bus.connect(0x8000..=0xffff, Rom::as_prg_peri(rom));

        Arc::new(Mutex::new(Cpu::new(cpu_bus)))
    }

    fn register_clock_callbacks(clock: &mut Clock, cpu: Arc<Mutex<Cpu>>, ppu: Arc<Mutex<Ppu>>) {
        let cpu = Arc::clone(&cpu);
        clock.register_callback(move || {
            // TODO: compute cycles
            cpu.lock().unwrap().step();
        });

        let ppu = Arc::clone(&ppu);
        clock.register_callback(move || {
            // TODO: compute cycles
            ppu.lock().unwrap().step();
        });
    }

    pub fn power_on(&self) {
        self.cpu.lock().unwrap().reset();
    }

    pub fn tick(&self) {
        self.clock.lock().unwrap().tick();
    }
}
