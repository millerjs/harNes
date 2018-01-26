use ::types::*;
use ::memory::Memory;
use ::mappers::Mapper;

pub struct Empty {}
impl Mapper for Empty {}

impl Memory for Empty {
    fn read(&self, address: Word) -> Byte {
        panic!("Attempted to read from empty mapper");
    }

    fn write(&mut self, address: Word, value: Byte) {
        panic!("Attempted to write to empty mapper");
    }
}
