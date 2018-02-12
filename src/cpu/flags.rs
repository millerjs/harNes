use ::types::*;
use ::cpu::Cpu;

#[derive(Debug)]
pub struct Flags {
    pub carry: bool,
    pub zero: bool,
    pub interrupt_disable: bool,
    pub decimal_mode: bool,
    pub break_mode: bool,
    pub overflow: bool,
    pub negative: bool,
}

impl Default for Flags {
    fn default() -> Flags {
        Flags::from_byte(0b00000100)
    }
}


impl Flags {
    pub fn to_byte(&self) -> Byte {
        let mut status = 0;
        status |=  self.carry             as u8;
        status |= (self.zero              as u8) << 1;
        status |= (self.interrupt_disable as u8) << 2;
        status |= (self.decimal_mode      as u8) << 3;
        status |= (self.break_mode        as u8) << 4;
        status |= (self.overflow          as u8) << 6;
        status |= (self.negative          as u8) << 7;
        status
    }

    pub fn from_byte(byte: Byte) -> Flags {
        Flags {
            carry:             is!(byte & 0b00000001),
            zero:              is!(byte & 0b00000010),
            interrupt_disable: is!(byte & 0b00000100),
            decimal_mode:      is!(byte & 0b00001000),
            break_mode:        is!(byte & 0b00010000),
            overflow:          is!(byte & 0b01000000),
            negative:          is!(byte & 0b10000000),
        }
    }
}

impl Cpu {
    /// Update the sign and zero flags via accumulator
    #[inline(always)]
    pub fn update_flags(&mut self) {
        let value = self.accumulator;
        self.update_flags_with(value);
    }

    /// Update the sign and zero flags via `value`
    pub fn update_flags_with(&mut self, value: Byte) {
        self.flags.zero = value == 0;
        self.flags.negative = is!(value & 0b10000000);
    }
}
