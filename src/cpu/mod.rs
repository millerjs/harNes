pub mod flags;
pub mod instruction;
pub mod instruction_set;
pub mod stack;

use ::flags::*;
use ::stack::*;
use ::types::*;
use ::address::*;
use ::memory::*;
use ::instruction::Instruction;

#[derive(Default)]
pub struct Cpu {
    pub memory: MappedMemory,
    pub cycles: usize,
    pub program_counter: Word,
    pub stack_pointer: Byte,
    pub accumulator: Byte,
    pub register_x: Byte,
    pub register_y: Byte,
    pub interrupt: Byte,
    pub stall: usize,
    pub flags: Flags,
}

pub enum Interrupt {
    None,
    NMI,
    IRQ,
}


impl Cpu {
    pub fn memory(mut self, memory: MappedMemory) -> Self {
        self.memory = memory;
        self
    }

    pub fn run(mut self) {
    }

    fn step(&mut self) {
        let op_address = self.program_counter;
        self.program_counter += 1;

        // TODO update cycles
        self.cycles += 1
    }

    // fn read_op(&mut self) -> Operation {

    // }

    /// Delegates loading of address in memory or loads from register
    #[inline(always)]
    pub fn load(&self, address: &Address) -> Byte {
        match *address {
            Address::Accumulator => self.accumulator,
            _ => {
                let computed = self.compute_address(address);
                self.memory.read(computed)
            },
        }
    }

    /// Delegates loading of word from address in memory
    #[inline(always)]
    pub fn load_word(&self, address: &Address) -> Word {
        let computed = self.compute_address(address);
        Word::from_bytes(self.memory.read(computed), self.memory.read(computed + 1))
    }

    /// Delegates writing of value to address in memory or writes to
    /// register
    #[inline(always)]
    pub fn store(&mut self, address: &Address, value: Byte) {
        match *address {
            Address::Accumulator => self.accumulator = value,
            _ => {
                let computed = self.compute_address(address);
                self.memory.write(computed, value)
            },
        }
    }

    #[inline(always)]
    fn compute_address(&self, address: &Address) -> Word {
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
                self.load(&Address::Absolute(byte as Word + self.register_y as Word)) as Word
            },
            Address::ZeroPage(byte) => {
                byte as Word
            },
            Address::ZeroPageX(byte) => {
                (self.register_x + byte) as Word
            },
            Address::ZeroPageY(byte) => {
                (self.register_y + byte) as Word
            },
            _ => unreachable!()
        }
    }

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

    /// Increments the program and returns the previous value
    #[inline(always)]
    pub fn increment_program_counter(&mut self) -> Word {
        let previous_program_counter = self.program_counter;
        self.program_counter += 1;
        previous_program_counter
    }

    /// Increments the program and returns the previous value
    #[inline(always)]
    pub fn compare<T: Ord>(&mut self, a: T, b: T) {
        self.flags.zero     = a.eq(&b);
        self.flags.negative = a.gt(&b);
    }


    /// All branches are relative mode and have a length of two
    /// bytes
    ///
    /// A branch not taken requires two machine cycles. Add one if the
    /// branch is taken and add one more if the branch crosses a page
    /// boundary.
    #[inline(always)]
    pub fn branch(&mut self, condition: bool) {
        let address = self.increment_program_counter();
        let delta = self.load(&Address::Absolute(address)) as i8;
        if condition {
            self.program_counter = (self.program_counter as i32 + delta as i32) as Word;
        }
    }
}
