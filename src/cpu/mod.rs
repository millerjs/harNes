pub mod flags;
pub mod instruction_set;
pub mod stack;
pub mod interrupt;
pub mod memory;

use ::flags::*;
use ::stack::*;
use ::types::*;
use ::address::*;
use ::memory::*;
use ::instruction::Instruction;

pub use ::instruction_set::InstructionSet;

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

    /// Increments the program and returns the previous value
    #[inline(always)]
    pub fn increment_program_counter(&mut self) -> Word {
        let previous_program_counter = self.program_counter;
        self.program_counter += 1;
        previous_program_counter
    }
}
