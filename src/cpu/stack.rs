use ::types::*;
use ::cpu::Cpu;
use ::memory::Memory;

pub const STACK_OFFSET: Word = 256;

pub trait Stack {
    fn push(&mut self, value: Byte);
    fn pop(&mut self) -> Byte;
    fn push_word(&mut self, value: Word);
    fn pop_word(&mut self, value: Word) -> Word;
}

impl Stack for Cpu {
    fn push(&mut self, value: Byte) {
        let address = STACK_OFFSET + (self.stack_pointer as Word);
        self.stack_pointer -= 1;
        self.memory.write(address, value);
    }

    fn pop(&mut self) -> Byte {
        self.stack_pointer += 1;
        let address = STACK_OFFSET + (self.stack_pointer as Word);
        self.memory.read(address)
    }

    fn push_word(&mut self, value: Word) {
        let (little, big) = value.as_bytes();
        self.push(little);
        self.push(big);
    }

    fn pop_word(&mut self, value: Word) -> Word {
        let (little, big) = (self.pop(), self.pop());
        Word::from_bytes(little, big)
    }
}
