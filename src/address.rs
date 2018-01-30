use ::types::*;
use std::fmt;

pub enum Address {
    /// A full 16-bit address is specified and the byte at that
    /// address is used to perform the computation. (e.g. LDX)
    Absolute(Word),

    /// The value in X is added to the specified address for a sum
    /// address. The value at the sum address is used to perform the
    /// computation. (e.g. ADC)
    AbsoluteIndexedX(Word),

    /// The value in X is added to the specified address for a sum
    /// address. The value at the sum address is used to perform the
    /// computation. (e.g. INC)
    AbsoluteIndexedY(Word),

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

impl Address {
    pub fn length(&self) -> usize {
        match *self {
            Address::Absolute(_)         => 1,
            Address::AbsoluteIndexedX(_) => 3,
            Address::AbsoluteIndexedY(_) => 3,
            Address::Accumulator         => 1,
            Address::Immediate(_)        => 2,
            Address::Implicit            => 1,
            Address::Indirect(_)         => 3,
            Address::IndirectIndexed(_)  => 2,
            Address::IndexedIndirect(_)  => 2,
            Address::Relative(_)         => 2,
            Address::ZeroPage(_)         => 2,
            Address::ZeroPageX(_)        => 2,
            Address::ZeroPageY(_)        => 2,
        }
    }
}

impl fmt::Debug for Address {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Address::Absolute(inner)         => write!(f, "Address::Absolute({:#x})", inner),
            Address::AbsoluteIndexedX(inner) => write!(f, "Address::AbsoluteIndexedX({:#x})", inner),
            Address::AbsoluteIndexedY(inner) => write!(f, "Address::AbsoluteIndexedY({:#x})", inner),
            Address::Accumulator             => write!(f, "Address::Accumulator"),
            Address::Immediate(inner)        => write!(f, "Address::Immediate({:#x})", inner),
            Address::Implicit                => write!(f, "Address::Implicit"),
            Address::Indirect(inner)         => write!(f, "Address::Indirect({:#x})", inner),
            Address::IndirectIndexed(inner)  => write!(f, "Address::IndirectIndexed({:#x})", inner),
            Address::IndexedIndirect(inner)  => write!(f, "Address::IndexedIndirect({:#x})", inner),
            Address::Relative(inner)         => write!(f, "Address::Relative({:#x})", inner),
            Address::ZeroPage(inner)         => write!(f, "Address::ZeroPage({:#x})", inner),
            Address::ZeroPageX(inner)        => write!(f, "Address::ZeroPageX({:#x})", inner),
            Address::ZeroPageY(inner)        => write!(f, "Address::ZeroPageY({:#x})", inner),
        }
    }
}
