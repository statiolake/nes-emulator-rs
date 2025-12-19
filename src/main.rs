use std::sync::{Arc, RwLock};

use bevy::ecs::resource::Resource;

use crate::hardware::Hardware;

mod hardware;

const WIDTH: u32 = 32;
const HEIGHT: u32 = 32;
const SCALE: f32 = 30.0;

#[derive(Resource)]
struct GameHardware(Arc<RwLock<Hardware>>);

fn main() -> anyhow::Result<()> {
    Ok(())
}
