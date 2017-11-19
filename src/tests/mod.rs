#![allow(dead_code, unused_imports, unused_variables)]

use ::*;
pub mod cpu;

struct LinearMemory {
    inner: Vec<Word>,
}

impl Default for LinearMemory {
    fn default() -> LinearMemory {
        LinearMemory { inner: vec![0; MOS_6502_MEMORY_SIZE] }
    }
}

impl Memory for LinearMemory {
    fn read(&self, address: &Address) -> Word {
        match *address {
            Address::Absolute(address) => self.inner[address as usize],
            _ => 0
        }
    }

    fn write(&mut self, address: &Address, value: Word) {

    }
}
