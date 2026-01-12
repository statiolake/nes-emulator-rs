use std::{
    sync::{Arc, mpsc},
    thread,
};

use eframe::NativeOptions;
use egui::{CentralPanel, Color32, Pos2, Rect, Vec2, ViewportBuilder};

use crate::hardware::{
    Hardware,
    ppu::{DISPLAY_HEIGHT, DISPLAY_WIDTH},
    rom::Rom,
};

pub mod hardware;

pub mod rt;
#[cfg(test)]
mod test;

const PIXEL_SIZE: f32 = 2.0;

// #[derive(Resource)]
// struct GameHardware(Arc<RwLock<Hardware>>);

fn main() -> anyhow::Result<()> {
    let options = NativeOptions {
        viewport: ViewportBuilder::default().with_inner_size(Vec2::new(
            DISPLAY_WIDTH as f32 * PIXEL_SIZE,
            DISPLAY_HEIGHT as f32 * PIXEL_SIZE,
        )),
        ..NativeOptions::default()
    };

    let rom = Rom::parse(include_bytes!("../rom/nestest.nes"))?;
    let hardware = Arc::new(Hardware::assemble(rom));

    let (exit_tx, exit_rx) = mpsc::channel();
    let handle = {
        let hardware = Arc::clone(&hardware);
        thread::spawn(move || {
            hardware.power_on();
            while exit_rx.try_recv().is_err() {
                println!("{}", hardware.cpu.lock().unwrap().dump_state());
                hardware.tick();
            }
        })
    };

    eframe::run_native(
        "NES Emulator",
        options,
        Box::new(|_cc| Ok(Box::new(App::new(hardware)))),
    )
    .map_err(|e| anyhow::anyhow!("{}", e))?;

    exit_tx.send(()).unwrap();
    handle.join().unwrap();

    Ok(())
}

struct App {
    hardware: Arc<Hardware>,
}

impl App {
    pub fn new(hardware: Arc<Hardware>) -> Self {
        Self { hardware }
    }
}

impl eframe::App for App {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        CentralPanel::default().show(ctx, |ui| {
            for i in 0..DISPLAY_HEIGHT {
                for j in 0..DISPLAY_WIDTH {
                    let rect = Rect::from_min_size(
                        Pos2::new(j as f32 * PIXEL_SIZE, i as f32 * PIXEL_SIZE),
                        Vec2::new(PIXEL_SIZE, PIXEL_SIZE),
                    );
                    let color = if (i + j) % 2 == 0 {
                        Color32::from_rgb(0, 0, 0)
                    } else {
                        Color32::from_rgb(255, 255, 255)
                    };
                    ui.painter().rect_filled(rect, 0.0, color);
                }
            }
        });
    }
}
