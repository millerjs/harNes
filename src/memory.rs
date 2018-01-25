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

pub enum AddressMapping {
    Ram,
    Ppu,
    Input,
    Apu,
    Mapper,
}

impl AddressMapping {
    pub fn from_word(word: Word) -> AddressMapping {
        match word {
            word if word <= 0x1FFF => AddressMapping::Ram,
            word if word <= 0x3FFF => AddressMapping::Ppu,
            word if word == 0x4016 => AddressMapping::Input,
            word if word <= 0x4018 => AddressMapping::Apu,
                                 _ => AddressMapping::Apu
        }
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


impl MappedMemory {
    pub fn slice<'a>(&'a self, start: Word) -> &'a [Byte] {
        match AddressMapping::from_word(start) {
            AddressMapping::Ram    => &self.ram.bytes[start as usize..],
            AddressMapping::Ppu    => unimplemented!(),
            AddressMapping::Input  => unimplemented!(),
            AddressMapping::Apu    => unimplemented!(),
            AddressMapping::Mapper => unimplemented!(),
        }
    }
}
