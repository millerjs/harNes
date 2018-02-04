use ::types::*;
use ::address::*;
use ::memory::*;
use ::mappers::Mapper;
use ::cartridge::Cartridge;

pub struct NRom {
    program_rom: Vec<Byte>,
    program_ram: Vec<Byte>,
    character_rom: Vec<Byte>,
}

impl Memory for NRom {
    fn read(&self, address: Word) -> Byte {
        if 0x6000 <= address && address <= 0x7fff {
            self.program_ram[((address - 0x6000) & 0x1fff) as usize]
        } else {
            self.program_rom[((address - 0x8000) & 0x7fff) as usize]
        }
    }

    fn write(&mut self, address: Word, value: Byte) {
        trace!("Writing {} to {:#x} from MMC1", value, address);
    }
}

impl Mapper for NRom {
    fn slice<'a>(&'a self, address: Word) -> &'a [Byte] {
        if 0x6000 <= address && address <= 0x7fff {
            &self.program_ram[((address - 0x6000) & 0x1fff) as usize..]
        } else {
            &self.program_rom[((address - 0x8000) & 0x7fff) as usize..]
        }
    }
}

pub fn from_cartridge(cartridge: Cartridge) -> Box<NRom> {
    Box::new(NRom {
        program_rom: cartridge.program_memory,
        character_rom: cartridge.character_memory,
        program_ram: vec![0; 8192],
    })
}
