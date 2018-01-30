use ::types::*;
use ::memory::Memory;
use ::mappers::Mapper;

#[derive(Default)]
pub struct Empty {
    mem: Vec<Byte>
}

impl Mapper for Empty {
    fn slice<'a>(&'a self, start: Word) -> &'a [Byte] {
        &*self.mem
    }
}

impl Memory for Empty {
    fn read(&self, address: Word) -> Byte {
        panic!("Attempted to read from empty mapper");
    }

    fn write(&mut self, address: Word, value: Byte) {
        panic!("Attempted to write to empty mapper");
    }
}
