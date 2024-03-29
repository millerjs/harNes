pub mod mapper0;
pub mod mapper1;
pub mod empty;

use ::types::*;
use ::memory::*;
use ::cartridge::Cartridge;

pub trait Mapper: Memory {
    fn slice<'a>(&'a self, start: Word) -> &'a [Byte];
}

pub fn from_cartridge(cartridge: Cartridge) -> Box<Mapper> {
    let mapper_code = cartridge.mapper_code();
    match mapper_code {
        000 => mapper0::from_cartridge(cartridge),
        001 => mapper1::from_cartridge(cartridge),
        _ => panic!("Unknown mapper code {}", mapper_code)
    }
}
