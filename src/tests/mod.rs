#![allow(dead_code, unused_imports, unused_variables)]

use ::*;
use ::types::*;
use ::memory::*;
pub mod cpu;

#[derive(Default)]
pub struct CpuBuilder {
    cpu: Cpu<MappedMemory>,
}

impl CpuBuilder {
    fn accumulator(mut self, value: Byte) -> CpuBuilder {
        self.cpu.store(&Address::Accumulator, value);
        self
    }

    fn carry(mut self, value: bool) -> CpuBuilder {
        self.cpu.flags.carry = true;
        self
    }

    fn build(self) -> Cpu<MappedMemory> {
        self.cpu
    }

    fn store(mut self, address: &Address, value: Byte) -> CpuBuilder {
        self.cpu.store(address, value);
        self
    }
}
