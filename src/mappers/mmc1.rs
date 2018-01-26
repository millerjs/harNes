use ::types::*;
use ::address::*;
use ::memory::*;
use ::mappers::Mapper;
use ::cartridge::Cartridge;

#[derive(Default)]
pub struct MMC1 {
    program_rom: Vec<Byte>,
    character_rom: Vec<Byte>,
}


enum Bank {
    ProgramRam,
    ProgramRom,
    CharacterRom,
}


impl Bank {
    fn from_address(address: Word) -> Bank {
        match address {
            address if address > 0x6000 && address < 0x7FFF => Bank::ProgramRam,
            address if address > 0x8000 && address < 0xBFFF => Bank::ProgramRom,
            address if address > 0xC000 && address < 0xFFFF => Bank::ProgramRam,
            address if address > 0x0000 && address < 0x1FFF => Bank::ProgramRam,
            _ => panic!("invalid mapper address {}", address)
        }
    }
}
impl Memory for MMC1 {
    fn read(&self, address: Word) -> Byte {
        trace!("Reading {:#x} from MMC1", address);
        0
    }

    fn write(&mut self, address: Word, value: Byte) {
        trace!("Writing {} to {:#x} from MMC1", value, address);
    }
}

impl Mapper for MMC1 {}

pub fn from_cartridge(cartridge: Cartridge) -> Box<MMC1> {
    Box::new(MMC1 {
        program_rom: cartridge.program_memory,
        character_rom: cartridge.character_memory,
    })
}
