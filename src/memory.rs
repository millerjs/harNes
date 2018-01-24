use address::*;
use types::*;
use cartridge::Cartridge;

pub const MEMORY_SIZE: usize = 65536;
pub const RAM_SIZE: usize = 2048;

impl Default for Ram {
    fn default() -> Ram {
        Ram { bytes: vec![0; RAM_SIZE] }
    }
}

pub trait Memory: Default {
    fn read(&self, address: Word) -> Byte;
    fn write(&mut self, address: Word, value: Byte);
}

struct Ram {
    bytes: Vec<Byte>,
}

impl Memory for Ram {
    fn read(&self, address: Word) -> Byte {
        self.bytes[(address & 0x7FF) as usize]
    }
    fn write(&mut self, address: Word, value: Byte) {
        self.bytes[(address & 0x7FF) as usize] = value
    }
}

#[derive(Default)]
pub struct MappedMemory {
    ram: Ram,
    cartridge: Cartridge,
}

impl MappedMemory {
    pub fn cartridge(mut self, cartridge: Cartridge) -> Self {
        self.cartridge = cartridge;
        self
    }
}

impl Memory for MappedMemory {
    fn read(&self, address: Word) -> Byte {
        match AddressMapping::from_word(address) {
            AddressMapping::Ram    => self.ram.read(address),
            AddressMapping::Ppu    => unimplemented!(),
            AddressMapping::Input  => unimplemented!(),
            AddressMapping::Apu    => unimplemented!(),
            AddressMapping::Mapper => unimplemented!(),
        }
    }
    fn write(&mut self, address: Word, value: Byte) {
        match AddressMapping::from_word(address) {
            AddressMapping::Ram    => self.ram.write(address, value),
            AddressMapping::Ppu    => unimplemented!(),
            AddressMapping::Input  => unimplemented!(),
            AddressMapping::Apu    => unimplemented!(),
            AddressMapping::Mapper => unimplemented!(),
        }
    }
}
