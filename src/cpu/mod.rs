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
        for step in 0..10000 {
            // if step >= 115 { self.print_registers() }
            print!("{}:\t", step);
            self.step();
        }
    }

    pub fn print_registers(&self) {
        print!("A: {}\tY: {}\tX: {}\t{:?}\n", self.accumulator, self.register_y, self.register_x, self.flags);
    }

    fn step(&mut self) {
        print!("{:#x}: ", self.program_counter);
        let instruction = Instruction::from_byte_code(self.memory.slice(self.program_counter));
        print!("\t{:?}", instruction);
        trace!("Read instruction {:?}", instruction);
        // self.cycles += instruction.length();
        self.program_counter += instruction.length() as Word;
        self.execute_instruction(&instruction);
        println!("");
    }

    fn reset(&mut self) {
        self.stack_pointer = 255;
        self.program_counter = self.load_word(&Address::Absolute(0xFFFC));
        trace!("Reset program counter to {:#x}", self.program_counter);
    }
}
