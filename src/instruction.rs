use address::Address;
use types::*;

#[derive(Debug)]
pub enum Instruction {
    ADC(Address), // add with carry
    AND(Address), // and (with accumulator)
    ASL(Address), // arithmetic shift left
    BCC(Address), // branch on carry clear
    BCS(Address), // branch on carry set
    BEQ(Address), // branch on equal (zero set)
    BIT(Address), // bit test
    BMI(Address), // branch on minus (negative set)
    BNE(Address), // branch on not equal (zero clear)
    BPL(Address), // branch on plus (negative clear)
    BRK(Address), // interrupt
    BVC(Address), // branch on overflow clear
    BVS(Address), // branch on overflow set
    CLC(Address), // clear carry
    CLD(Address), // clear decimal
    CLI(Address), // clear interrupt disable
    CLV(Address), // clear overflow
    CMP(Address), // compare (with accumulator)
    CPX(Address), // compare with X
    CPY(Address), // compare with Y
    DEC(Address), // decrement
    DEX(Address), // decrement X
    DEY(Address), // decrement Y
    EOR(Address), // exclusive or (with accumulator)
    INC(Address), // increment
    INX(Address), // increment X
    INY(Address), // increment Y
    JMP(Address), // jump
    JSR(Address), // jump subroutine
    LDA(Address), // load accumulator
    LDX(Address), // load X
    LDY(Address), // load Y
    LSR(Address), // logical shift right
    NOP(Address), // no instruction
    ORA(Address), // or with accumulator
    PHA(Address), // push accumulator
    PHP(Address), // push processor status (SR)
    PLA(Address), // pull accumulator
    PLP(Address), // pull processor status (SR)
    ROL(Address), // rotate left
    ROR(Address), // rotate right
    RTI(Address), // return from interrupt
    RTS(Address), // return from subroutine
    SBC(Address), // subtract with carry
    SEC(Address), // set carry
    SED(Address), // set decimal
    SEI(Address), // set interrupt disable
    STA(Address), // store accumulator
    STX(Address), // store X
    STY(Address), // store Y
    TAX(Address), // transfer accumulator to X
    TAY(Address), // transfer accumulator to Y
    TSX(Address), // transfer stack pointer to X
    TXA(Address), // transfer X to accumulator
    TXS(Address), // transfer X to stack pointer
    TYA(Address), // transfer Y to accumulator
}

#[inline(always)]
fn byte(program: &[Byte]) -> Byte { program[1] }

#[inline(always)]
fn word(program: &[Byte]) -> Word { Word::from_bytes(program[1], program[2]) }

impl Instruction {
    pub fn from_byte_code(program: &[Byte]) -> Instruction {
        let size = 0;
        let opcode = program[0];
        trace!("Parsing opcode {:#x}", opcode);

        let instruction = match opcode {
            // ADC
            0x69 => Instruction::ADC (Address::Immediate        (byte (program))),
            0x65 => Instruction::ADC (Address::ZeroPage         (byte (program))),
            0x75 => Instruction::ADC (Address::ZeroPageX        (byte (program))),
            0x6D => Instruction::ADC (Address::Absolute         (word (program))),
            0x7D => Instruction::ADC (Address::AbsoluteIndexedX (word (program))),
            0x79 => Instruction::ADC (Address::AbsoluteIndexedY (word (program))),
            0x61 => Instruction::ADC (Address::IndexedIndirect  (byte (program))),
            0x71 => Instruction::ADC (Address::IndirectIndexed  (byte (program))),

            // AND
            0x29 => Instruction::AND (Address::Immediate        (byte (program))),
            0x25 => Instruction::AND (Address::ZeroPage         (byte (program))),
            0x35 => Instruction::AND (Address::ZeroPageX        (byte (program))),
            0x2D => Instruction::AND (Address::Absolute         (word (program))),
            0x3D => Instruction::AND (Address::AbsoluteIndexedX (word (program))),
            0x39 => Instruction::AND (Address::AbsoluteIndexedY (word (program))),
            0x21 => Instruction::AND (Address::IndexedIndirect  (byte (program))),
            0x31 => Instruction::AND (Address::IndirectIndexed  (byte (program))),

            // ASL
            0x0A => Instruction::ASL (Address::Accumulator),
            0x06 => Instruction::ASL (Address::ZeroPage         (byte (program))),
            0x16 => Instruction::ASL (Address::ZeroPageX        (byte (program))),
            0x0E => Instruction::ASL (Address::Absolute         (word (program))),
            0x1E => Instruction::ASL (Address::AbsoluteIndexedX (word (program))),

            // BIT
            0x24 => Instruction::BIT (Address::ZeroPage         (byte (program))),
            0x2C => Instruction::BIT (Address::Absolute         (word (program))),

            // BRK
            0x00 => Instruction::BRK (Address::Implicit),

            // ADC
            0xC9 => Instruction::CMP (Address::Immediate        (byte (program))),
            0xC5 => Instruction::CMP (Address::ZeroPage         (byte (program))),
            0xD5 => Instruction::CMP (Address::ZeroPageX        (byte (program))),
            0xCD => Instruction::CMP (Address::Absolute         (word (program))),
            0xDD => Instruction::CMP (Address::AbsoluteIndexedX (word (program))),
            0xD9 => Instruction::CMP (Address::AbsoluteIndexedY (word (program))),
            0xC1 => Instruction::CMP (Address::IndexedIndirect  (byte (program))),
            0xD1 => Instruction::CMP (Address::IndirectIndexed  (byte (program))),

            // CPX
            0xE0 => Instruction::CPX (Address::Immediate        (byte (program))),
            0xE4 => Instruction::CPX (Address::ZeroPage         (byte (program))),
            0xEC => Instruction::CPX (Address::Absolute         (word (program))),

            // CPY
            0xC0 => Instruction::CPY (Address::Immediate        (byte (program))),
            0xC4 => Instruction::CPY (Address::ZeroPage         (byte (program))),
            0xCC => Instruction::CPY (Address::Absolute         (word (program))),

            // DEC
            0xC6 => Instruction::DEC (Address::ZeroPage         (byte (program))),
            0xD6 => Instruction::DEC (Address::ZeroPageX        (byte (program))),
            0xCE => Instruction::DEC (Address::Absolute         (word (program))),
            0xDE => Instruction::DEC (Address::AbsoluteIndexedX (word (program))),

            // EOR
            0x49 => Instruction::EOR (Address::Immediate        (byte (program))),
            0x45 => Instruction::EOR (Address::ZeroPage         (byte (program))),
            0x55 => Instruction::EOR (Address::ZeroPageX        (byte (program))),
            0x4D => Instruction::EOR (Address::Absolute         (word (program))),
            0x5D => Instruction::EOR (Address::AbsoluteIndexedX (word (program))),
            0x59 => Instruction::EOR (Address::AbsoluteIndexedY (word (program))),
            0x41 => Instruction::EOR (Address::IndexedIndirect  (byte (program))),
            0x51 => Instruction::EOR (Address::IndirectIndexed  (byte (program))),

            // INC
            0xE6 => Instruction::INC (Address::ZeroPage         (byte (program))),
            0xF6 => Instruction::INC (Address::ZeroPageX        (byte (program))),
            0xEE => Instruction::INC (Address::Absolute         (word (program))),
            0xFE => Instruction::INC (Address::AbsoluteIndexedX (word (program))),

            // JMP
            0x4C => Instruction::JMP (Address::Absolute         (word (program))),
            0x6C => Instruction::JMP (Address::Indirect         (byte (program))),

            // JSR
            0x20 => Instruction::JSR (Address::Absolute         (word (program))),

            // LDA
            0xA9 => Instruction::LDA (Address::Immediate        (byte (program))),
            0xA5 => Instruction::LDA (Address::ZeroPage         (byte (program))),
            0xB5 => Instruction::LDA (Address::ZeroPageX        (byte (program))),
            0xAD => Instruction::LDA (Address::Absolute         (word (program))),
            0xBD => Instruction::LDA (Address::AbsoluteIndexedX (word (program))),
            0xB9 => Instruction::LDA (Address::AbsoluteIndexedY (word (program))),
            0xA1 => Instruction::LDA (Address::IndexedIndirect  (byte (program))),
            0xB1 => Instruction::LDA (Address::IndirectIndexed  (byte (program))),

            // LDX
            0xa2 => Instruction::LDX (Address::Immediate        (byte (program))),
            0xA6 => Instruction::LDX (Address::ZeroPage         (byte (program))),
            0xB6 => Instruction::LDX (Address::ZeroPageY        (byte (program))),
            0xAE => Instruction::LDX (Address::Absolute         (word (program))),
            0xBE => Instruction::LDX (Address::AbsoluteIndexedY (word (program))),

            // LDY
            0xA0 => Instruction::LDY (Address::Immediate        (byte (program))),
            0xA4 => Instruction::LDY (Address::ZeroPage         (byte (program))),
            0xB4 => Instruction::LDY (Address::ZeroPageX        (byte (program))),
            0xAC => Instruction::LDY (Address::Absolute         (word (program))),
            0xBC => Instruction::LDY (Address::AbsoluteIndexedX (word (program))),


            // LSR
            0x4A => Instruction::LSR (Address::Accumulator),
            0x46 => Instruction::LSR (Address::ZeroPage         (byte (program))),
            0x56 => Instruction::LSR (Address::ZeroPageX        (byte (program))),
            0x4E => Instruction::LSR (Address::Absolute         (word (program))),
            0x5E => Instruction::LSR (Address::AbsoluteIndexedX (word (program))),

            // NOP
            0xEA => Instruction::NOP (Address::Implicit),

            // ORA
            0x09 => Instruction::ORA (Address::Immediate        (byte (program))),
            0x05 => Instruction::ORA (Address::ZeroPage         (byte (program))),
            0x15 => Instruction::ORA (Address::ZeroPageX        (byte (program))),
            0x0D => Instruction::ORA (Address::Absolute         (word (program))),
            0x1D => Instruction::ORA (Address::AbsoluteIndexedX (word (program))),
            0x19 => Instruction::ORA (Address::AbsoluteIndexedY (word (program))),
            0x01 => Instruction::ORA (Address::IndexedIndirect  (byte (program))),
            0x11 => Instruction::ORA (Address::IndirectIndexed  (byte (program))),

            // Transfers
            0xAA => Instruction::TAX (Address::Implicit),
            0x8A => Instruction::TXA (Address::Implicit),
            0xCA => Instruction::DEX (Address::Implicit),
            0xE8 => Instruction::INX (Address::Implicit),
            0xA8 => Instruction::TAY (Address::Implicit),
            0x98 => Instruction::TYA (Address::Implicit),
            0x88 => Instruction::DEY (Address::Implicit),
            0xC8 => Instruction::INY (Address::Implicit),

            // ROL
            0x2A => Instruction::ROL (Address::Accumulator),
            0x26 => Instruction::ROL (Address::ZeroPage         (byte (program))),
            0x36 => Instruction::ROL (Address::ZeroPageX        (byte (program))),
            0x2E => Instruction::ROL (Address::Absolute         (word (program))),
            0x3E => Instruction::ROL (Address::AbsoluteIndexedX (word (program))),

            // ROR
            0x6A => Instruction::ROR (Address::Accumulator),
            0x66 => Instruction::ROR (Address::ZeroPage         (byte (program))),
            0x76 => Instruction::ROR (Address::ZeroPageX        (byte (program))),
            0x6E => Instruction::ROR (Address::Absolute         (word (program))),
            0x7E => Instruction::ROR (Address::AbsoluteIndexedX (word (program))),

            // Returns
            0x40 => Instruction::RTI (Address::Implicit),
            0x60 => Instruction::RTS (Address::Implicit),

            // SBC
            0xE9 => Instruction::SBC (Address::Immediate        (byte (program))),
            0xE5 => Instruction::SBC (Address::ZeroPage         (byte (program))),
            0xF5 => Instruction::SBC (Address::ZeroPageX        (byte (program))),
            0xED => Instruction::SBC (Address::Absolute         (word (program))),
            0xFD => Instruction::SBC (Address::AbsoluteIndexedX (word (program))),
            0xF9 => Instruction::SBC (Address::AbsoluteIndexedY (word (program))),
            0xE1 => Instruction::SBC (Address::IndexedIndirect  (byte (program))),
            0xF1 => Instruction::SBC (Address::IndirectIndexed  (byte (program))),

            // STA
            0x85 => Instruction::STA (Address::ZeroPage         (byte (program))),
            0x95 => Instruction::STA (Address::ZeroPageX        (byte (program))),
            0x8D => Instruction::STA (Address::Absolute         (word (program))),
            0x9D => Instruction::STA (Address::AbsoluteIndexedX (word (program))),
            0x99 => Instruction::STA (Address::AbsoluteIndexedY (word (program))),
            0x81 => Instruction::STA (Address::IndexedIndirect  (byte (program))),
            0x91 => Instruction::STA (Address::IndirectIndexed  (byte (program))),

            // Stack instructions
            0x9A => Instruction::TXS (Address::Implicit),
            0xBA => Instruction::TSX (Address::Implicit),
            0x48 => Instruction::PHA (Address::Implicit),
            0x68 => Instruction::PLA (Address::Implicit),
            0x08 => Instruction::PHP (Address::Implicit),
            0x28 => Instruction::PLP (Address::Implicit),

            // STX
            0x86 => Instruction::STX (Address::ZeroPage         (byte (program))),
            0x96 => Instruction::STX (Address::ZeroPageY        (byte (program))),
            0x8E => Instruction::STX (Address::Absolute         (word (program))),

            // STY
            0x84 => Instruction::STY (Address::ZeroPage         (byte (program))),
            0x94 => Instruction::STY (Address::ZeroPageX        (byte (program))),
            0x8C => Instruction::STY (Address::Absolute         (word (program))),

            0xF8 => Instruction::SED (Address::Implicit),
            0x78 => Instruction::SEI (Address::Implicit),
            0xd8 => Instruction::CLD (Address::Implicit),

            0xd0 => Instruction::BNE (Address::Relative         (byte (program))),

            _    => unreachable!(),
        };
        trace!("Parsed opcode {:#x} -> {:?}", opcode, instruction);
        instruction
    }

    pub fn length(&self) -> usize {
        match *self {
            Instruction::ADC(ref address) => address.length(),
            Instruction::AND(ref address) => address.length(),
            Instruction::ASL(ref address) => address.length(),
            Instruction::BCC(ref address) => address.length(),
            Instruction::BCS(ref address) => address.length(),
            Instruction::BEQ(ref address) => address.length(),
            Instruction::BIT(ref address) => address.length(),
            Instruction::BMI(ref address) => address.length(),
            Instruction::BNE(ref address) => address.length(),
            Instruction::BPL(ref address) => address.length(),
            Instruction::BRK(ref address) => address.length(),
            Instruction::BVC(ref address) => address.length(),
            Instruction::BVS(ref address) => address.length(),
            Instruction::CLC(ref address) => address.length(),
            Instruction::CLD(ref address) => address.length(),
            Instruction::CLI(ref address) => address.length(),
            Instruction::CLV(ref address) => address.length(),
            Instruction::CMP(ref address) => address.length(),
            Instruction::CPX(ref address) => address.length(),
            Instruction::CPY(ref address) => address.length(),
            Instruction::DEC(ref address) => address.length(),
            Instruction::DEX(ref address) => address.length(),
            Instruction::DEY(ref address) => address.length(),
            Instruction::EOR(ref address) => address.length(),
            Instruction::INC(ref address) => address.length(),
            Instruction::INX(ref address) => address.length(),
            Instruction::INY(ref address) => address.length(),
            Instruction::JMP(ref address) => address.length(),
            Instruction::JSR(ref address) => address.length(),
            Instruction::LDA(ref address) => address.length(),
            Instruction::LDX(ref address) => address.length(),
            Instruction::LDY(ref address) => address.length(),
            Instruction::LSR(ref address) => address.length(),
            Instruction::NOP(ref address) => address.length(),
            Instruction::ORA(ref address) => address.length(),
            Instruction::PHA(ref address) => address.length(),
            Instruction::PHP(ref address) => address.length(),
            Instruction::PLA(ref address) => address.length(),
            Instruction::PLP(ref address) => address.length(),
            Instruction::ROL(ref address) => address.length(),
            Instruction::ROR(ref address) => address.length(),
            Instruction::RTI(ref address) => address.length(),
            Instruction::RTS(ref address) => address.length(),
            Instruction::SBC(ref address) => address.length(),
            Instruction::SEC(ref address) => address.length(),
            Instruction::SED(ref address) => address.length(),
            Instruction::SEI(ref address) => address.length(),
            Instruction::STA(ref address) => address.length(),
            Instruction::STX(ref address) => address.length(),
            Instruction::STY(ref address) => address.length(),
            Instruction::TAX(ref address) => address.length(),
            Instruction::TAY(ref address) => address.length(),
            Instruction::TSX(ref address) => address.length(),
            Instruction::TXA(ref address) => address.length(),
            Instruction::TXS(ref address) => address.length(),
            Instruction::TYA(ref address) => address.length(),
        }
    }
}
