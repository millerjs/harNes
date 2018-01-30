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

const PROGRAM_RAM_MASK: usize = 8191;
const PROGRAM_ROM_BANK_SIZE: usize = 16383;

#[derive(Debug)]
enum Bank {
    ProgramRam,
    ProgramRom,
    CharacterRom,
}

impl Bank {
    fn from_address(address: Word) -> Bank {
        match address {
            address if address <= 0x0FFF                      => Bank::CharacterRom,
            address if address >= 0x1000 && address <= 0x1FFF => Bank::CharacterRom,
            address if address >= 0x8000 && address <= 0xBFFF => Bank::ProgramRom,
            address if address >= 0xC000                      => Bank::ProgramRom,
            _ => panic!("invalid mapper address {}", address)
        }
    }
}
impl Memory for MMC1 {
    fn read(&self, address: Word) -> Byte {
        trace!("Reading {:#x} from MMC1", address);
        let index = address as usize - 0x8000;
        trace!("Index {:#x} from MMC1 program rom", index);
        self.program_rom[index]
    }

    fn write(&mut self, address: Word, value: Byte) {
        trace!("Writing {} to {:#x} from MMC1", value, address);
    }
}

impl Mapper for MMC1 {
    fn slice<'a>(&'a self, start: Word) -> &'a [Byte] {
        trace!("Slicing {:#x} from MMC1", start);
        let index = start as usize - 0x8000;
        trace!("Index {:#x} from MMC1 program rom", index);
        &self.program_rom[index..]
    }
}

pub fn from_cartridge(cartridge: Cartridge) -> Box<MMC1> {
    // println!("scanning program_memory");
    // let mut last_byte = 0;
    // for (index, byte) in cartridge.program_memory.iter().enumerate() {
    //     if *byte == 0xe7 && last_byte == 0x83 {
    //         println!("Found at index {:#x}", index - 1)
    //     };
    //     last_byte = *byte;
    // }
    // println!("Scanned program_memory");

    // println!("0xfffd: {:#x}", cartridge.program_memory[0x7ffd]);

    Box::new(MMC1 {
        program_rom: cartridge.program_memory,
        character_rom: cartridge.character_memory,
        program_ram: vec![0; 0],
    })
}
