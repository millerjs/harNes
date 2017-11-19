use address::*;
use types::*;

pub const MOS_6502_MEMORY_SIZE: usize = 65536;
pub const MOS_6502_RAM_SIZE: usize = 2048;

impl Default for Ram {
    fn default() -> Ram {
        Ram { bytes: vec![0; MOS_6502_RAM_SIZE] }
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
