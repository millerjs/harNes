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
        match *address {
            Address::Absolute(address) => self.inner[address as usize] = value,
            _ => (),
        }
    }
}

#[derive(Default)]
pub struct CpuBuilder {
    cpu: Cpu<LinearMemory>,
}

impl CpuBuilder {
    fn accumulator(mut self, value: Word) -> CpuBuilder {
        self.cpu.store(&Address::Accumulator, value);
        self
    }

    fn build(self) -> Cpu<LinearMemory> {
        self.cpu
    }

    fn store(mut self, address: &Address, value: Word) -> CpuBuilder {
        self.cpu.store(address, value);
        self
    }
}
