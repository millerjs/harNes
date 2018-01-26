pub mod mmc1;
pub mod empty;

use ::types::*;
use ::memory::*;
use ::cartridge::Cartridge;

pub trait Mapper: Memory {}

pub fn from_cartridge(cartridge: Cartridge) -> Box<Mapper> {
    let mapper_code = cartridge.mapper_code();
    match mapper_code {
        001 => mmc1::from_cartridge(cartridge),
        _ => panic!("Unknown mapper code {}", mapper_code)
    }
}
