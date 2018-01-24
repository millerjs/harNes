use types::*;
use std::io::Read;
use std::io::Error as IOError;
use std::path::Path;
use std::fs::File;
use std::fmt;

quick_error! {
    #[derive(Debug)]
    pub enum CartridgeError {
        LoadError(err: String) { from() }
        IOError(err: IOError) { from() }
    }
}

pub type Flags = Byte;

#[repr(C, packed)]
#[derive(Debug, Default)]
pub struct Header {
    /// Constant $4E $45 $53 $1A ("NES" followed by MS-DOS end-of-file)
    constant: [Byte; 4],
    /// Size of PRG ROM in 16 KB units
    program_rom_size: Byte,
    /// Size of CHR ROM in 8 KB units (Value 0 means the board uses CHR RAM)
    character_rom_size: Byte,
    /// 76543210
    /// ||||||||
    /// |||||||+- Mirroring: 0: horizontal (vertical arrangement) (CIRAM A10 = PPU A11)
    /// |||||||              1: vertical (horizontal arrangement) (CIRAM A10 = PPU A10)
    /// ||||||+-- 1: Cartridge contains battery-backed PRG RAM ($6000-7FFF) or other persistent memory
    /// |||||+--- 1: 512-byte trainer at $7000-$71FF (stored before PRG data)
    /// ||||+---- 1: Ignore mirroring control or above mirroring bit; instead provide four-screen VRAM
    /// ++++----- Lower nybble of mapper number
    flags_6: Byte,
    /// 76543210
    /// ||||||||
    /// |||||||+- VS Unisystem
    /// ||||||+-- PlayChoice-10 (8KB of Hint Screen data stored after CHR data)
    /// ||||++--- If equal to 2, flags 8-15 are in NES 2.0 format
    /// ++++----- Upper nybble of mapper number
    flags_7: Byte,
    /// Size of PRG RAM in 8 KB units (Value 0 infers 8 KB for compatibility)
    program_ram_size: Byte,
    /// 76543210
    /// ||||||||
    /// |||||||+- TV system (0: NTSC; 1: PAL)
    /// +++++++-- Reserved, set to zero
    flags_9: Byte,
    /// 76543210
    ///   ||  ||
    ///   ||  ++- TV system (0: NTSC; 2: PAL; 1/3: dual compatible)
    ///   |+----- PRG RAM ($6000-$7FFF) (0: present; 1: not present)
    ///   +------ 0: Board has no bus conflicts; 1: Board has bus conflicts
    flags_10: Byte,
    zero_filled: [Byte; 5]
}

#[derive(Default)]
pub struct Cartridge {
    header: Header,
    program_rom: Vec<Byte>,
    character_rom: Vec<Byte>,
}

impl Header {
    pub fn load<R: Read>(source: &mut R) -> Result<Header, CartridgeError> {
        use std::{mem, slice};
        let mut header: Header = unsafe { mem::zeroed() };
        unsafe {
            let buffer = &mut header as *mut _ as *mut u8;
            let header_slice = slice::from_raw_parts_mut(buffer, mem::size_of::<Header>());
            source.read_exact(header_slice)?;
        }

        if header.constant != [0x4E, 0x45, 0x53, 0x1A] {
            Err(CartridgeError::LoadError(String::from("Invalid header constant")))
        } else {
            Ok(header)
        }
    }

    pub fn trainer(&self) -> bool {
        self.flags_6 & 0b00000100 != 0
    }
}

impl Cartridge {
    pub fn load<R: Read>(source: &mut R) -> Result<Cartridge, CartridgeError> {
        let header = Header::load(source)?;
        let mut trainer = vec![0; 512];
        let mut program_rom = vec![0; header.program_rom_size as usize * 16384];
        let mut character_rom = vec![0; header.character_rom_size as usize * 8192];

        if header.trainer() {
            source.read_exact(&mut trainer)?;
        }

        source.read_exact(&mut program_rom)?;
        source.read_exact(&mut character_rom)?;

        let cartridge = Cartridge {
            header: header,
            program_rom,
            character_rom
        };

        info!("Loaded iNES cartridge {}", cartridge);
        Ok(cartridge)
    }

    pub fn load_file<P: AsRef<Path>>(path: &P) -> Result<Cartridge, CartridgeError> {
        let mut f = File::open(path.as_ref())?;
        Cartridge::load(&mut f)
    }
}

impl fmt::Display for Cartridge {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "<Rom(PRom: {}K, CRom: {}k, trainer: {})>",
               self.program_rom.len() / 1028,
               self.character_rom.len() / 1028,
               self.header.trainer())
    }
}
