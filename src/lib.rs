#![allow(dead_code, unused_imports)]

#[macro_use]
extern crate lazy_static;

const MOS_6502_CLOCK_RATE: usize = 1789773;
const MOS_6502_MEMORY_SIZE: usize = 65536;

pub type BitOffset = u8;
pub type Word = u8;
pub type DoubleWord = u16;
pub type Pointer = u16;

struct Ram {
    main: Vec<Word>,
}

impl Memory for Ram {
    fn read(&self, address: Address) -> Word {
        0
    }

    fn write(&self, address: Address, value: Word) -> Word {
        0
    }
}

pub struct Clock {
    rate: usize,
}

#[derive(Clone)]
pub enum Address {
    Absolute(Pointer),
    AbsoluteIndexedX(Pointer),
    AbsoluteIndexedY(Pointer),
    Accumulator(Pointer),
    Immediate(Pointer),
    Implicit(Pointer),
    Indirect(Pointer),
    IndirectIndexed(Pointer),
    IndexedIndirect(Pointer),
    Relative(Pointer),
    ZeroPage(Pointer),
    ZeroPageX(Pointer),
    ZeroPageY(Pointer),
}

pub enum Interrupt {
    None,
    NMI,
    IRQ,
}

impl Default for Ram {
    fn default() -> Ram {
        Ram {
            main: Vec::with_capacity(MOS_6502_MEMORY_SIZE),
        }
    }
}

pub trait Memory: Default {
    fn read(&self, address: Address) -> Word;
    fn write(&self, address: Address, value: Word) -> Word;
}

#[no_mangle]
#[derive(Default)]
pub struct Flags {
    carry: bool,
    zero: bool,
    interupt_disable: bool,
    decimal_mode_flag: bool,
    break_mode: bool,
    unused: bool,
    overflow: bool,
    negative: bool,
}

#[derive(Default)]
pub struct Cpu<M> {
    memory: Box<M>,
    cycles: usize,
    program_counter: Pointer,
    stack_pointer: Word,
    accumulator: Word,
    register_x: Word,
    register_y: Word,
    interrupt: Word,
    stall: usize,
    flags: Flags,
}

macro_rules! is {
    ($value: expr) => {
        $value != 0
    };
}

impl<M> Cpu<M> where M: Memory {
    /// ADC - Add with Carry
    ///
    /// This instruction adds the contents of a memory location to the
    /// accumulator together with the carry bit. If overflow occurs
    /// the carry bit is set, this enables multiple byte addition to
    /// be performed.
    #[inline(always)]
    fn adc(&mut self, address: Address) {
        let val = self.memory.read(address) as u16;
        let prev = self.accumulator as DoubleWord;
        let sum = prev + val + self.flags.carry as DoubleWord;
        self.flags.carry = is!(sum & 255);
        self.flags.overflow = (prev ^ val) & 128 == 0 && (prev as Word ^ sum as Word) & 128 == 128;
        self.flags.zero = is!(self.accumulator);
        self.flags.negative = is!(self.accumulator);
        self.accumulator = sum as Word;
    }

    /// AND - Logical AND
    ///
    /// A logical AND is performed, bit by bit, on the accumulator
    /// contents using the contents of a byte of memory.
    fn and(&mut self) {}

    /// ASL - Arithmetic Shift Left
    ///
    /// This operation shifts all the bits of the accumulator or
    /// memory contents one bit left. Bit 0 is set to 0 and bit 7 is
    /// placed in the carry flag. The effect of this operation is to
    /// multiply the memory contents by 2 (ignoring 2's complement
    /// considerations), setting the carry if the result will not fit
    /// in 8 bits.
    fn asl(&mut self) {}

    /// BCC - Branch if Carry Clear
    ///
    /// If the carry flag is clear then add the relative displacement
    /// to the program counter to cause a branch to a new location.
    fn bcc(&mut self) {}

    /// BCS - Branch if Carry Set
    ///
    /// If the carry flag is set then add the relative displacement to
    /// the program counter to cause a branch to a new location.
    fn bcs(&mut self) {}

    /// BEQ - Branch if Equal
    ///
    /// If the zero flag is set then add the relative displacement to
    /// the program counter to cause a branch to a new location.
    fn beq(&mut self) {}

    /// BIT - Bit Test
    ///
    /// This instructions is used to test if one or more bits are set
    /// in a target memory location. The mask pattern in A is ANDed
    /// with the value in memory to set or clear the zero flag, but
    /// the result is not kept. Bits 7 and 6 of the value from memory
    /// are copied into the N and V flags.
    fn bit(&mut self) {}

    /// BMI - Branch if Minus
    ///
    /// If the negative flag is set then add the relative displacement
    /// to the program counter to cause a branch to a new location.
    fn bmi(&mut self) {}

    /// BNE - Branch if Not Equal
    ///
    /// If the zero flag is clear then add the relative displacement
    /// to the program counter to cause a branch to a new location.
    fn bne(&mut self) {}

    /// BPL - Branch if Positive
    ///
    /// If the negative flag is clear then add the relative
    /// displacement to the program counter to cause a branch to a new
    /// location.
    fn bpl(&mut self) {}

    /// BRK - Force Interrupt
    ///
    /// The BRK instruction forces the generation of an interrupt
    /// request. The program counter and processor status are pushed
    /// on the stack then the IRQ interrupt vector at $FFFE/F is
    /// loaded into the PC and the break flag in the status set to
    /// one.
    fn brk(&mut self) {}

    /// BVC - Branch if Overflow Clear
    ///
    /// If the overflow flag is clear then add the relative
    /// displacement to the program counter to cause a branch to a new
    /// location.
    fn bvc(&mut self) {}

    /// BVS - Branch if Overflow Set
    ///
    /// If the overflow flag is set then add the relative displacement
    /// to the program counter to cause a branch to a new location.
    fn bvs(&mut self) {}

    /// CLC - Clear Carry Flag
    ///
    /// Set the carry flag to zero.
    fn clc(&mut self) {}

    /// CLD - Clear Decimal Mode
    ///
    /// Sets the decimal mode flag to zero.
    fn cld(&mut self) {}

    /// CLI - Clear Interrupt Disable
    ///
    /// Clears the interrupt disable flag allowing normal interrupt
    /// requests to be serviced.
    fn cli(&mut self) {}

    /// CLV - Clear Overflow Flag
    ///
    /// Clears the overflow flag.
    fn clv(&mut self) {}

    /// CMP - Compare
    ///
    /// This instruction compares the contents of the accumulator with
    /// another memory held value and sets the zero and carry flags as
    /// appropriate.
    fn cmp(&mut self) {}

    /// CPX - Compare X Register
    ///
    /// This instruction compares the contents of the X register with
    /// another memory held value and sets the zero and carry flags as
    /// appropriate.
    fn cpx(&mut self) {}

    /// CPY - Compare Y Register
    ///
    /// This instruction compares the contents of the Y register with
    /// another memory held value and sets the zero and carry flags as
    /// appropriate.
    fn cpy(&mut self) {}

    /// DEC - Decrement Memory
    ///
    /// Subtracts one from the value held at a specified memory
    /// location setting the zero and negative flags as appropriate.
    fn dec(&mut self) {}

    /// DEX - Decrement X Register
    ///
    /// Subtracts one from the X register setting the zero and
    /// negative flags as appropriate.
    fn dex(&mut self) {}

    /// DEY - Decrement Y Register
    ///
    /// Subtracts one from the Y register setting the zero and
    /// negative flags as appropriate.
    fn dey(&mut self) {}

    /// EOR - Exclusive OR
    ///
    /// An exclusive OR is performed, bit by bit, on the accumulator
    /// contents using the contents of a byte of memory.
    fn eor(&mut self) {}

    /// INC - Increment Memory
    ///
    /// Adds one to the value held at a specified memory location
    /// setting the zero and negative flags as appropriate.
    fn inc(&mut self) {}

    /// INX - Increment X Register
    ///
    /// Adds one to the X register setting the zero and negative flags
    /// as appropriate.
    fn inx(&mut self) {}

    /// INY - Increment Y Register
    ///
    /// Adds one to the Y register setting the zero and negative flags
    /// as appropriate.
    fn iny(&mut self) {}

    /// JMP - Jump
    ///
    /// Sets the program counter to the address specified by the
    /// operand.
    fn jmp(&mut self) {}

    /// JSR - Jump to Subroutine
    ///
    /// The JSR instruction pushes the address (minus one) of the
    /// return point on to the stack and then sets the program counter
    /// to the target memory address.
    fn jsr(&mut self) {}

    /// LDA - Load Accumulator
    ///
    /// Loads a byte of memory into the accumulator setting the zero
    /// and negative flags as appropriate.
    fn lda(&mut self) {}

    /// LDX - Load X Register
    ///
    /// Loads a byte of memory into the X register setting the zero
    /// and negative flags as appropriate.
    fn ldx(&mut self) {}

    /// LDY - Load Y Register
    ///
    /// Loads a byte of memory into the Y register setting the zero
    /// and negative flags as appropriate.
    fn ldy(&mut self) {}

    /// LSR - Logical Shift Right
    ///
    /// Each of the bits in A or M is shift one place to the
    /// right. The bit that was in bit 0 is shifted into the carry
    /// flag. Bit 7 is set to zero.
    fn lsr(&mut self) {}

    /// NOP - No Operation
    ///
    /// The NOP instruction causes no changes to the processor other than the normal incrementing of the program counter to the next instruction.
    fn nop(&mut self) {}

    /// ORA - Logical Inclusive OR
    ///
    /// An inclusive OR is performed, bit by bit, on the accumulator
    /// contents using the contents of a byte of memory.
    fn ora(&mut self) {}

    /// PHA - Push Accumulator
    ///
    /// Pushes a copy of the accumulator on to the stack.
    fn pha(&mut self) {}

    /// PHP - Push Processor Status
    ///
    /// Pushes a copy of the status flags on to the stack.
    fn php(&mut self) {}

    /// PLA - Pull Accumulator
    ///
    /// Pulls an 8 bit value from the stack and into the
    /// accumulator. The zero and negative flags are set as
    /// appropriate.
    fn pla(&mut self) {}

    /// PLP - Pull Processor Status
    ///
    /// Pulls an 8 bit value from the stack and into the processor
    /// flags. The flags will take on new states as determined by the
    /// value pulled.
    fn plp(&mut self) {}

    /// ROL - Rotate Left
    ///
    /// aMove each of the bits in either A or M one place to the
    /// left. Bit 0 is filled with the current value of the carry flag
    /// whilst the old bit 7 becomes the new carry flag value.
    fn rol(&mut self) {}

    /// ROR - Rotate Right
    ///
    /// Move each of the bits in either A or M one place to the
    /// right. Bit 7 is filled with the current value of the carry
    /// flag whilst the old bit 0 becomes the new carry flag value.
    fn ror(&mut self) {}

    /// RTI - Return from Interrupt
    ///
    /// The RTI instruction is used at the end of an interrupt
    /// processing routine. It pulls the processor flags from the
    /// stack followed by the program counter.
    fn rti(&mut self) {}

    /// RTS - Return from Subroutine
    ///
    /// The RTS instruction is used at the end of a subroutine to
    /// return to the calling routine. It pulls the program counter
    /// (minus one) from the stack.
    fn rts(&mut self) {}

    /// SBC - Subtract with Carry
    ///
    /// This instruction subtracts the contents of a memory location
    /// to the accumulator together with the not of the carry bit. If
    /// overflow occurs the carry bit is clear, this enables multiple
    /// byte subtraction to be performed.
    fn sbc(&mut self) {}

    /// SEC - Set Carry Flag
    ///
    /// Set the carry flag to one.
    fn sec(&mut self) {}

    /// SED - Set Decimal Flag
    ///
    /// Set the decimal mode flag to one.
    fn sed(&mut self) {}

    /// SEI - Set Interrupt Disable
    ///
    /// Set the interrupt disable flag to one.
    fn sei(&mut self) {}

    /// STA - Store Accumulator
    ///
    /// Stores the contents of the accumulator into memory.
    fn sta(&mut self) {}

    /// STX - Store X Register
    ///
    /// Stores the contents of the X register into memory.
    fn stx(&mut self) {}

    /// STY - Store Y Register
    ///
    /// Stores the contents of the Y register into memory.
    fn sty(&mut self) {}

    /// TAX - Transfer Accumulator to X
    ///
    /// Copies the current contents of the accumulator into the X
    /// register and sets the zero and negative flags as appropriate.
    fn tax(&mut self) {}

    /// TAY - Transfer Accumulator to Y
    ///
    /// Copies the current contents of the accumulator into the Y
    /// register and sets the zero and negative flags as appropriate.
    fn tay(&mut self) {}

    /// TSX - Transfer Stack Pointer to X
    ///
    /// Copies the current contents of the stack register into the X
    /// register and sets the zero and negative flags as appropriate.
    fn tsx(&mut self) {}

    /// TXA - Transfer X to Accumulator
    ///
    /// Copies the current contents of the X register into the
    /// accumulator and sets the zero and negative flags as
    /// appropriate.
    fn txa(&mut self) {}

    /// TXS - Transfer X to Stack Pointer
    ///
    /// Copies the current contents of the X register into the stack
    /// register.  fn txs(&mut self) {}
    fn tya(&mut self) {}
}
