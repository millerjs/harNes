use ::address::Address;
use ::cpu::Cpu;
use ::types::*;
use ::memory::*;

impl Cpu {
    #[inline(always)]
    pub fn compute_address(&self, address: &Address) -> Word {
        match *address {
            Address::Absolute(dword) => {
                dword
            },
            Address::AbsoluteIndexedX(byte) => {
                self.register_x as Word + byte as Word
            },
            Address::AbsoluteIndexedY(byte) => {
                self.register_y as Word + byte as Word
            },
            Address::IndirectIndexed(byte) => {
                self.load(&Address::Absolute(byte as Word)) as Word + self.register_y as Word
            },
            Address::IndexedIndirect(byte) => {
                self.load(&Address::Absolute(byte as Word + self.register_y as Word)) as Word
            },
            Address::Relative(byte) => {
                ((self.program_counter as i32) + (byte as i8) as i32) as Word
            },
            Address::ZeroPage(byte) => {
                byte as Word
            },
            Address::ZeroPageX(byte) => {
                (self.register_x + byte) as Word
            },
            Address::ZeroPageY(byte) => {
                (self.register_y + byte) as Word
            }
            _ => unreachable!()
        }
    }

    /// Delegates loading of address in memory or loads from register
    #[inline(always)]
    pub fn load(&self, address: &Address) -> Byte {
        trace!("Loading byte from {:?}", address);
        match *address {
            Address::Accumulator => self.accumulator,
            Address::Immediate(byte) => byte,
            _ => {
                let computed = self.compute_address(address);
                self.memory.read(computed)
            },
        }
    }

    /// Delegates loading of word from address in memory
    #[inline(always)]
    pub fn load_word(&self, address: &Address) -> Word {
        trace!("Loading word from {:?}", address);
        let computed = self.compute_address(address);
        Word::from_bytes(self.memory.read(computed), self.memory.read(computed + 1))
    }

    /// Delegates writing of value to address in memory or writes to
    /// register
    #[inline(always)]
    pub fn store(&mut self, address: &Address, value: Byte) {
        trace!("Storing word {:} to {:?}", value, address);
        match *address {
            Address::Accumulator => self.accumulator = value,
            _ => {
                let computed = self.compute_address(address);
                self.memory.write(computed, value)
            },
        }
    }
}
