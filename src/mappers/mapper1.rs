use ::types::*;
use ::address::*;
use ::memory::*;
use ::mappers::Mapper;
use ::cartridge::Cartridge;

pub struct MMC1 {
    program_banks: Vec<Vec<Byte>>,
    program_ram: Vec<Byte>,
    character_rom: Vec<Byte>,
    register_control: Byte,
    program_bank: Byte,
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
        let address = (address - 0x8000) as usize & PROGRAM_ROM_BANK_SIZE;
        self.program_banks[self.program_bank as usize][address]
    }

    fn write(&mut self, address: Word, value: Byte) {
        trace!("Writing {} to {:#x} from MMC1", value, address);
    }
}

impl Mapper for MMC1 {
    fn slice<'a>(&'a self, start: Word) -> &'a [Byte] {
        let address = (start - 0x8000) as usize & PROGRAM_ROM_BANK_SIZE;
        &self.program_banks[self.program_bank as usize][address..]

        // trace!("Slicing {:#x} from MMC1", start);
        // let index = start as usize - 0x8000;
        // trace!("Index {:#x} from MMC1 program rom", index);
        // &self.program_rom[index..]
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
    let n_program_banks = (cartridge.program_memory.len() + 8*1024) / PROGRAM_ROM_BANK_SIZE;
    let program_banks: Vec<_> = (0..n_program_banks).map(|i| cartridge.program_memory[i*PROGRAM_ROM_BANK_SIZE..(i+1)*PROGRAM_ROM_BANK_SIZE].to_vec()).collect();
    // println!("{}", cartridge.program_memory.len());

    Box::new(MMC1 {
        program_banks: program_banks,
        character_rom: cartridge.character_memory,
        register_control: 0b1100,
        program_bank: 0,
        program_ram: vec![],
    })
}
