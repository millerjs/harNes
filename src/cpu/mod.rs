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
        for step in 0..100000 {
            print!("{}\t", step);
            self.trace();
            self.step();
        }
    }

    pub fn trace(&self) {
        print!("{:#x}\t", self.program_counter);
        let program = self.memory.slice(self.program_counter);
        print!("{:#x}\t{:#x}\t{:#x}\t", program[0], program[1], program[2]);
        print!("{:#x}\t{:#x}\t{:#x}\t{:#x}", self.accumulator, self.register_x, self.register_y, self.flags.to_byte());
        print!("\t{}", self.stack_trace());
        println!("");
    }

    fn step(&mut self) {
        let instruction = Instruction::from_byte_code(self.memory.slice(self.program_counter));
        // self.cycles += instruction.length();
        self.program_counter += instruction.length() as Word;
        self.execute_instruction(&instruction);
    }

    fn reset(&mut self) {
        self.stack_pointer = 0xfd;
        self.program_counter = self.load_word(&Address::Absolute(0xFFFC));
        trace!("Reset program counter to {:#x}", self.program_counter);
    }
}
