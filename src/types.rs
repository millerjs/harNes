pub type BitOffset = u8;
pub type Byte = u8;
pub type Word = u16;

pub trait LittleEndianWord {
    fn from_bytes(little: Byte, big: Byte) -> Word;
    fn as_bytes(&self) -> (Byte, Byte);
}

impl LittleEndianWord for Word {
    /// Creates a Word from (Little, Big)
    fn from_bytes(little: Byte, big: Byte) -> Word {
        (big as Word) << 8 | (little as Word)
    }

    /// Returns (Little, Big)
    fn as_bytes(&self) -> (Byte, Byte) {
        let little = *self as Byte;
        let big = (*self >> 8) as Byte;
        (little, big)
    }
}
