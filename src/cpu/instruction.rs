use address::Address;

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

// impl Instruction {
//     pub fn decode(op_code: Byte) -> {

//     }
// }
