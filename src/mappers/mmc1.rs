use ::types::*;
use ::address::*;
use ::memory::*;
use ::mappers::Mapper;
use ::cartridge::Cartridge;

pub struct MMC1 {
    program_ram: Vec<Byte>,
    program_rom: Vec<Byte>,
    character_rom: Vec<Byte>,
}

const PROGRAM_RAM_SIZE: usize = 8191;
const PROGRAM_ROM_SIZE: usize = 16383;

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
            address if address > 0xC000 && address < 0xFFFF => Bank::CharacterRom,
            address if address > 0x0000 && address < 0x1FFF => Bank::CharacterRom,
            _ => panic!("invalid mapper address {}", address)
        }
    }
}
impl Memory for MMC1 {
    fn read(&self, address: Word) -> Byte {
        trace!("Reading {:#x} from MMC1", address);
        match Bank::from_address(address) {
            Bank::ProgramRam => self.program_ram[address as usize & PROGRAM_ROM_SIZE],
            Bank::ProgramRom => unimplemented!(),
            Bank::CharacterRom => unimplemented!(),
        }
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
        program_ram: vec![0; 0],
    })
}
