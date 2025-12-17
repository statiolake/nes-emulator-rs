use std::{
    sync::{Arc, RwLock, mpsc},
    thread,
    time::Duration,
};

use bevy::{
    app::App,
    diagnostic::{FrameTimeDiagnosticsPlugin, LogDiagnosticsPlugin},
    prelude::*,
    window::WindowResolution,
};
use rand::{Rng, SeedableRng, rngs::SmallRng};

use crate::cpu::{Cpu, Status};

mod cpu;

const WIDTH: u32 = 32;
const HEIGHT: u32 = 32;
const SCALE: f32 = 30.0;

#[derive(Resource)]
struct GameHardware(Arc<RwLock<Cpu>>);

fn main() -> anyhow::Result<()> {
    let (exit_tx, exit_rx) = mpsc::channel::<()>();
    let cpu = Arc::new(RwLock::new(Cpu::new()));
    let cpu_clone = Arc::clone(&cpu);
    let mut rng = SmallRng::from_os_rng();
    let handle = thread::spawn(move || {
        loop {
            {
                let mut cpu = cpu_clone.write().expect("failed to lock CPU for writing");
                if !cpu.status.contains(Status::BREAK_COMMAND) {
                    cpu.mem.write(0x00FE, rng.random());
                    cpu.step();
                }
            }
            if exit_rx.try_recv().is_ok() {
                break;
            }
            thread::sleep(Duration::from_micros(1_000_000 / 10000))
        }
    });

    App::new()
        .add_plugins(
            DefaultPlugins.set(WindowPlugin {
                primary_window: Some(Window {
                    title: "Snake Game Emulator".to_string(),
                    resolution: WindowResolution::new(
                        (WIDTH as f32 * SCALE) as u32,
                        (HEIGHT as f32 * SCALE) as u32,
                    )
                    .with_scale_factor_override(SCALE),
                    ..Default::default()
                }),
                ..Default::default()
            }),
        )
        // .add_plugins((
        //     FrameTimeDiagnosticsPlugin::default(),
        //     LogDiagnosticsPlugin::default(),
        // ))
        .insert_resource(GameHardware(cpu))
        .add_systems(Startup, (setup, initialize_cpu))
        .add_systems(Update, (handle_key, render))
        .run();

    exit_tx
        .send(())
        .expect("failed to send exit signal to CPU thread");

    handle
        .join()
        .map_err(|e| anyhow::anyhow!("failed to join CPU: {e:?}"))?;

    Ok(())
}

#[derive(Component)]
struct CellPosition {
    i: u32,
    j: u32,
}

fn setup(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<ColorMaterial>>,
) {
    commands.spawn((
        Camera2d,
        Transform::from_xyz(WIDTH as f32 / 2.0, HEIGHT as f32 / 2.0, 0.0),
    ));

    for i in 0..HEIGHT {
        for j in 0..WIDTH {
            let mesh = Mesh2d(meshes.add(Rectangle::new(1.0, 1.0)));
            let color = if (i + j) % 2 == 0 {
                Color::WHITE
            } else {
                Color::BLACK
            };
            let material = MeshMaterial2d(materials.add(color));
            let transform = Transform::from_xyz(j as f32 + 0.5, i as f32 + 0.5, 0.0);
            // CPU memory layout is up-to-bottom but Bevy coordinate is down-to-top as in normal
            // math
            let cell_position = CellPosition {
                i: (HEIGHT - i - 1),
                j,
            };
            commands.spawn((mesh, material, transform, cell_position));
        }
    }
}

fn initialize_cpu(hw: Res<GameHardware>) {
    const SNAKE_GAME_PROGRAM: &[u8] = &[
        0x20, 0x06, 0x06, 0x20, 0x38, 0x06, 0x20, 0x0d, 0x06, 0x20, 0x2a, 0x06, 0x60, 0xa9, 0x02,
        0x85, 0x02, 0xa9, 0x04, 0x85, 0x03, 0xa9, 0x11, 0x85, 0x10, 0xa9, 0x10, 0x85, 0x12, 0xa9,
        0x0f, 0x85, 0x14, 0xa9, 0x04, 0x85, 0x11, 0x85, 0x13, 0x85, 0x15, 0x60, 0xa5, 0xfe, 0x85,
        0x00, 0xa5, 0xfe, 0x29, 0x03, 0x18, 0x69, 0x02, 0x85, 0x01, 0x60, 0x20, 0x4d, 0x06, 0x20,
        0x8d, 0x06, 0x20, 0xc3, 0x06, 0x20, 0x19, 0x07, 0x20, 0x20, 0x07, 0x20, 0x2d, 0x07, 0x4c,
        0x38, 0x06, 0xa5, 0xff, 0xc9, 0x77, 0xf0, 0x0d, 0xc9, 0x64, 0xf0, 0x14, 0xc9, 0x73, 0xf0,
        0x1b, 0xc9, 0x61, 0xf0, 0x22, 0x60, 0xa9, 0x04, 0x24, 0x02, 0xd0, 0x26, 0xa9, 0x01, 0x85,
        0x02, 0x60, 0xa9, 0x08, 0x24, 0x02, 0xd0, 0x1b, 0xa9, 0x02, 0x85, 0x02, 0x60, 0xa9, 0x01,
        0x24, 0x02, 0xd0, 0x10, 0xa9, 0x04, 0x85, 0x02, 0x60, 0xa9, 0x02, 0x24, 0x02, 0xd0, 0x05,
        0xa9, 0x08, 0x85, 0x02, 0x60, 0x60, 0x20, 0x94, 0x06, 0x20, 0xa8, 0x06, 0x60, 0xa5, 0x00,
        0xc5, 0x10, 0xd0, 0x0d, 0xa5, 0x01, 0xc5, 0x11, 0xd0, 0x07, 0xe6, 0x03, 0xe6, 0x03, 0x20,
        0x2a, 0x06, 0x60, 0xa2, 0x02, 0xb5, 0x10, 0xc5, 0x10, 0xd0, 0x06, 0xb5, 0x11, 0xc5, 0x11,
        0xf0, 0x09, 0xe8, 0xe8, 0xe4, 0x03, 0xf0, 0x06, 0x4c, 0xaa, 0x06, 0x4c, 0x35, 0x07, 0x60,
        0xa6, 0x03, 0xca, 0x8a, 0xb5, 0x10, 0x95, 0x12, 0xca, 0x10, 0xf9, 0xa5, 0x02, 0x4a, 0xb0,
        0x09, 0x4a, 0xb0, 0x19, 0x4a, 0xb0, 0x1f, 0x4a, 0xb0, 0x2f, 0xa5, 0x10, 0x38, 0xe9, 0x20,
        0x85, 0x10, 0x90, 0x01, 0x60, 0xc6, 0x11, 0xa9, 0x01, 0xc5, 0x11, 0xf0, 0x28, 0x60, 0xe6,
        0x10, 0xa9, 0x1f, 0x24, 0x10, 0xf0, 0x1f, 0x60, 0xa5, 0x10, 0x18, 0x69, 0x20, 0x85, 0x10,
        0xb0, 0x01, 0x60, 0xe6, 0x11, 0xa9, 0x06, 0xc5, 0x11, 0xf0, 0x0c, 0x60, 0xc6, 0x10, 0xa5,
        0x10, 0x29, 0x1f, 0xc9, 0x1f, 0xf0, 0x01, 0x60, 0x4c, 0x35, 0x07, 0xa0, 0x00, 0xa5, 0xfe,
        0x91, 0x00, 0x60, 0xa6, 0x03, 0xa9, 0x00, 0x81, 0x10, 0xa2, 0x00, 0xa9, 0x01, 0x81, 0x10,
        0x60, 0xa2, 0x00, 0xea, 0xea, 0xca, 0xd0, 0xfb, 0x60,
    ];

    let GameHardware(cpu) = &*hw;
    let mut cpu = cpu.write().expect("failed to lock CPU for memory writing");

    cpu.load(SNAKE_GAME_PROGRAM);
    cpu.reset();
}

fn handle_key(mut hw: ResMut<GameHardware>, keys: Res<ButtonInput<KeyCode>>) {
    let GameHardware(cpu) = &mut *hw;
    let mut cpu = cpu.write().expect("failed to lock CPU for memory writing");
    if keys.pressed(KeyCode::KeyW) {
        cpu.mem.write(0x00FF, b'w');
    } else if keys.pressed(KeyCode::KeyS) {
        cpu.mem.write(0x00FF, b's');
    } else if keys.pressed(KeyCode::KeyA) {
        cpu.mem.write(0x00FF, b'a');
    } else if keys.pressed(KeyCode::KeyD) {
        cpu.mem.write(0x00FF, b'd');
    }
}

fn render(
    mut hw: ResMut<GameHardware>,
    mut materials: ResMut<Assets<ColorMaterial>>,
    cells: Query<(&mut MeshMaterial2d<ColorMaterial>, &CellPosition)>,
) {
    let GameHardware(cpu) = &mut *hw;
    let mut cpu = cpu.write().expect("failed to lock CPU for memory reading");

    for (material, cell) in cells {
        let byte = cpu.mem.read(0x0200 + (cell.i * WIDTH + cell.j) as u16);
        let color = match byte {
            0 => Color::linear_rgb(0.0, 0.0, 0.0),
            1 => Color::linear_rgb(1.0, 1.0, 1.0),
            2 | 9 => Color::linear_rgb(0.5, 0.5, 0.5),
            3 | 10 => Color::linear_rgb(1.0, 0.0, 0.0),
            4 | 11 => Color::linear_rgb(0.0, 1.0, 0.0),
            5 | 12 => Color::linear_rgb(0.0, 0.0, 1.0),
            6 | 13 => Color::linear_rgb(1.0, 0.0, 1.0),
            7 | 14 => Color::linear_rgb(1.0, 1.0, 0.0),
            _ => Color::linear_rgb(0.0, 1.0, 1.0),
        };

        materials
            .get_mut(&material.0)
            .expect("unknown resource")
            .color = color;
    }
}
