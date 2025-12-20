use crate::hardware::{Hardware, rom::Rom};

pub mod hardware;

#[cfg(test)]
mod test;

// const WIDTH: u32 = 32;
// const HEIGHT: u32 = 32;
// const SCALE: f32 = 30.0;

// #[derive(Resource)]
// struct GameHardware(Arc<RwLock<Hardware>>);

fn main() -> anyhow::Result<()> {
    let rom = Rom::parse(include_bytes!("../rom/nestest.nes"))?;
    let hw = Hardware::assemble(rom);
    hw.power_on();

    while !hw.cpu.lock().unwrap().is_halted() {
        println!("{}", hw.cpu.lock().unwrap().dump_state());
        hw.cpu.lock().unwrap().step();
    }

    Ok(())
}
