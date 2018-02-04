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
        info!("Starting NES!");
        self.reset();
        for i in 0..100 {
            trace!("Step number {}", i);
            self.step();
        }
    }

    fn step(&mut self) {
        let program_counter = self.program_counter;
        let instruction = Instruction::from_byte_code(self.memory.slice(program_counter));
        trace!("Read instruction {:?}", instruction);
        self.cycles += instruction.length();
        self.execute_instruction(&instruction);
        self.program_counter += instruction.length() as Word;
    }

    fn reset(&mut self) {
        self.stack_pointer = 255;
        self.program_counter = self.load_word(&Address::Absolute(0xFFFC));
        trace!("Reset program counter to {:#x}", self.program_counter);
    }

    /// Increments the program and returns the previous value
    #[inline(always)]
    pub fn increment_program_counter(&mut self) -> Word {
        let previous_program_counter = self.program_counter;
        self.program_counter += 1;
        previous_program_counter
    }
}
