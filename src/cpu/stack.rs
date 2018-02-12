use ::types::*;
use ::cpu::Cpu;
use ::memory::Memory;

pub const STACK_OFFSET: Word = 0x100;

pub trait Stack {
    fn push(&mut self, value: Byte);
    fn pop(&mut self) -> Byte;
    fn push_word(&mut self, value: Word);
    fn pop_word(&mut self) -> Word;
    fn stack_trace(&self) -> String;
}

impl Stack for Cpu {
    fn push(&mut self, value: Byte) {
        let address = STACK_OFFSET + self.stack_pointer as Word;
        self.memory.write(address, value);
        self.stack_pointer -= 1;
    }

    fn pop(&mut self) -> Byte {
        self.stack_pointer += 1;
        let address = STACK_OFFSET + self.stack_pointer as Word;
        let value = self.memory.read(address);
        value
    }

    fn push_word(&mut self, value: Word) {
        let (little, big) = value.as_bytes();
        self.push(big);
        self.push(little);
    }

    fn pop_word(&mut self) -> Word {
        let little = self.pop();
        let big = self.pop();
        Word::from_bytes(little, big)
    }

    fn stack_trace(&self) -> String {
        let mut trace = "'".to_string();
        for sp in self.stack_pointer..0xfd {
            trace += &*format!("{:} ", self.memory.read(STACK_OFFSET + 1 + sp as Word))
        }
        trace += "'";
        trace
    }
}
