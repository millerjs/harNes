use ::types::*;
use ::cpu::Cpu;
use ::memory::Memory;

pub const STACK_OFFSET: Word = 256;

pub trait Stack {
    fn push(&mut self, value: Byte);
    fn pop(&mut self) -> Byte;
    fn push_word(&mut self, value: Word);
    fn pop_word(&mut self) -> Word;
}

impl Stack for Cpu {
    fn push(&mut self, value: Byte) {
        let address = self.stack_pointer as Word;
        self.stack_pointer -= 1;
        self.memory.write(address, value);
    }

    fn pop(&mut self) -> Byte {
        let address = self.stack_pointer as Word;
        let value = self.memory.read(address);
        self.stack_pointer += 1;
        value
    }

    fn push_word(&mut self, value: Word) {
        let (little, big) = value.as_bytes();
        self.push(little);
        self.push(big);
    }

    fn pop_word(&mut self) -> Word {
        let little = self.pop();
        let big = self.pop();
        Word::from_bytes(little, big)
    }
}
