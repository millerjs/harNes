use address::*;
use types::*;
use cartridge::Cartridge;
use mappers;

pub const MEMORY_SIZE: usize = 65536;
pub const RAM_SIZE: usize = 2048;

pub trait Memory {
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

pub struct MappedMemory {
    ram: Ram,
    mapper: Box<mappers::Mapper>,
}

impl Default for MappedMemory {
    fn default() -> MappedMemory {
        let ram = Ram { bytes: vec![0; RAM_SIZE] };
        let mapper = Box::new(mappers::empty::Empty::default());
        MappedMemory { ram, mapper }
    }
}


impl MappedMemory {
    pub fn insert_cartridge(mut self, cartridge: Cartridge) -> Self {
        self.mapper = mappers::from_cartridge(cartridge);
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
                                 _ => AddressMapping::Mapper
        }
    }
}

impl Memory for MappedMemory {
    fn read(&self, address: Word) -> Byte {
        match AddressMapping::from_word(address) {
            AddressMapping::Ram    => self.ram.read(address),
            AddressMapping::Ppu    => 0,
            AddressMapping::Input  => 0,
            AddressMapping::Apu    => 0,
            AddressMapping::Mapper => self.mapper.read(address),
        }
    }
    fn write(&mut self, address: Word, value: Byte) {
        match AddressMapping::from_word(address) {
            AddressMapping::Ram    => self.ram.write(address, value),
            AddressMapping::Ppu    => (),
            AddressMapping::Input  => (),
            AddressMapping::Apu    => (),
            AddressMapping::Mapper => self.mapper.write(address, value),
        }
    }
}


impl MappedMemory {
    pub fn slice<'a>(&'a self, start: Word) -> &'a [Byte] {
        trace!("Slicing MappedMemory at {:#x}", start);
        match AddressMapping::from_word(start) {
            AddressMapping::Ram    => &self.ram.bytes[start as usize..],
            AddressMapping::Ppu    => unimplemented!(),
            AddressMapping::Input  => unimplemented!(),
            AddressMapping::Apu    => unimplemented!(),
            AddressMapping::Mapper => self.mapper.slice(start),
        }
    }
}
