use ::types::*;

#[derive(Clone, Debug)]
pub enum Address {
    /// A full 16-bit address is specified and the byte at that
    /// address is used to perform the computation. (e.g. LDX)
    Absolute(Word),

    /// The value in X is added to the specified address for a sum
    /// address. The value at the sum address is used to perform the
    /// computation. (e.g. ADC)
    AbsoluteIndexedX(Byte),

    /// The value in X is added to the specified address for a sum
    /// address. The value at the sum address is used to perform the
    /// computation. (e.g. INC)
    AbsoluteIndexedY(Byte),

    /// The Accumulator is implied as the operand, so no address needs
    /// to be specified. (e.g. ASL)
    Accumulator,

    /// The operand is used directly to perform the
    /// computation. (e.g. LDA)
    Immediate(Byte),

    /// The operand is implied, so it does not need to be
    /// specified. (e.g. TXA)
    Implicit,

    /// The JMP instruction is the only instruction that uses this
    /// addressing mode. It is a 3 byte instruction - the 2nd and 3rd
    /// bytes are an absolute address.
    Indirect(Byte),

    /// This mode is only used with the Y register. It differs in the
    /// order that Y is applied to the indirectly fetched address. An
    /// example instruction that uses indirect index addressing is LDA
    /// ($86),Y . To calculate the target address, the CPU will first
    /// fetch the address stored at zero page location $86. That
    /// address will be added to register Y to get the final target
    /// address. For LDA ($86),Y, if the address stored at $86 is
    /// $4028 (memory is 0086: 28 40, remember little endian) and
    /// register Y contains $10, then the final target address would
    /// be $4038. Register A will be loaded with the contents of
    /// memory at $4038.
    ///
    /// Indirect Indexed instructions are 2 bytes - the second byte is
    /// the zero-page address - $20 in the example. (So the fetched
    /// address has to be stored in the zero page.)
    ///
    /// While indexed indirect addressing will only generate a
    /// zero-page address, this mode's target address is not wrapped -
    /// it can be anywhere in the 16-bit address space.
    IndirectIndexed(Byte),

    /// This mode is only used with the X register. Consider a
    /// situation where the instruction is LDA ($20,X), X contains
    /// $04, and memory at $24 contains 0024: 74 20, First, X is added
    /// to $20 to get $24. The target address will be fetched from $24
    /// resulting in a target address of $2074. Register A will be
    /// loaded with the contents of memory at $2074.
    ///
    /// If X + the immediate byte will wrap around to a zero-page
    /// address. So you could code that like targetAddress = X +
    /// opcode[1]) & 0xFF .
    ///
    ///Indexed Indirect instructions are 2 bytes - the second byte is
    /// the zero-page address - $20 in the example. Obviously the
    /// fetched address has to be stored in the zero page.
    IndexedIndirect(Byte),

    /// The offset specified is added to the current address stored in
    /// the Program Counter (PC). Offsets can range from -128 to
    /// +127. (e.g. BPL)
    Relative(Byte),

    /// A single byte specifies an address in the first page of memory
    /// ($00xx), also known as the zero page, and the byte at that
    /// address is used to perform the computation. (e.g. LDY)
    ZeroPage(Byte),

    /// The value in X is added to the specified zero page address for
    /// a sum address. The value at the sum address is used to perform
    /// the computation.
    ZeroPageX(Byte),

    /// The value in Y is added to the specified zero page address for
    /// a sum address. The value at the sum address is used to perform
    /// the computation.
    ZeroPageY(Byte),
}

pub enum AddressMapping {
    Ram,
    Ppu,
    Input,
    Apu,
    Mapper,
}

impl AddressMapping {
    pub fn from_word(word: Word) -> AddressMapping {
        match word {
            word if word <= 0x1FFF => AddressMapping::Ram,
            word if word <= 0x3FFF => AddressMapping::Ppu,
            word if word == 0x4016 => AddressMapping::Input,
            word if word <= 0x4018 => AddressMapping::Apu,
                                 _ => AddressMapping::Apu
        }
    }
}
