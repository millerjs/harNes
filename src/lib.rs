#![allow(dead_code, unused_imports, unused_variables)]

#[macro_use]
extern crate lazy_static;

use std::num::Wrapping;
use std::cmp::Ord;

#[cfg(test)]
mod tests;

const MOS_6502_CLOCK_RATE: usize = 1789773;
const MOS_6502_MEMORY_SIZE: usize = 65536;

pub type BitOffset = u8;
pub type Word = u8;
pub type DWord = u16;
pub type Pointer = u16;

struct Ram {
    main: Vec<Word>,
}

impl Memory for Ram {
    fn read(&self, address: Address) -> Word {
        0
    }

    fn write(&mut self, address: Address, value: Word) -> Word {
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
    fn write(&mut self, address: Address, value: Word) -> Word;
}

#[no_mangle]
#[derive(Default)]
pub struct Flags {
    pub carry: bool,
    pub zero: bool,
    pub interrupt_disable: bool,
    pub decimal_mode: bool,
    pub break_mode: bool,
    pub unused: bool,
    pub overflow: bool,
    pub negative: bool,
}

#[derive(Default)]
pub struct Cpu<M> {
    memory: Box<M>,
    cycles: usize,
    pub program_counter: Pointer,
    pub stack_pointer: Word,
    pub accumulator: Word,
    pub register_x: Word,
    pub register_y: Word,
    interrupt: Word,
    pub stall: usize,
    pub flags: Flags,
}

macro_rules! is { ($value: expr) => { $value != 0 }; }


macro_rules! compare {
    ($self:ident, $a: expr, $b: expr) => {{
        let a = $a;
        let b = $b;
        $self.compare(a, b);
    }};
}

macro_rules! branch {
    ($self:ident, $condition: expr) => {{
        let condition = $condition;
        $self.branch(condition);
    }};
}

macro_rules! increment {
    ($self:ident, $value: expr, $amount: expr) => {{
        let result = ($value as Word).wrapping_add($amount as Word);
        compare!($self, result, 0);
        result
    }};
}


impl<M> Cpu<M> where M: Memory {
    /// Delegates loading of address in memory
    #[inline(always)]
    fn load(&self, address: Address) -> Word {
        self.memory.read(address)
    }

    /// Delegates writing of value to address in memory
    #[inline(always)]
    fn store(&mut self, address: Address, value: Word) -> Word {
        self.memory.write(address, value)
    }

    ///  the sign and zero flags
    #[inline(always)]
    fn update_flags(&mut self) {
        self.flags.zero = self.accumulator == 0;
        self.flags.negative = is!(self.accumulator & 0b10000000)
    }

    /// Increments the program and returns the previous value
    #[inline(always)]
    fn increment_program_counter(&mut self) -> DWord {
        let previous_program_counter = self.program_counter;
        self.program_counter += 1;
        previous_program_counter
    }

    /// Increments the program and returns the previous value
    #[inline(always)]
    fn compare<T: Ord>(&mut self, a: T, b: T) {
        self.flags.zero     = a.eq(&b);
        self.flags.negative = a.gt(&b);
    }


    /// All branches are relative mode and have a length of two
    /// bytes
    ///
    /// A branch not taken requires two machine cycles. Add one if the
    /// branch is taken and add one more if the branch crosses a page
    /// boundary.
    #[inline(always)]
    fn branch(&mut self, condition: bool) {
        // TODO
        // let delta = self.load(self.increment_program_counter()) as i8;
        // if condition {
        //     self.program_counter = (self.program_counter as i32 + delta as i32) as DWord;
        // }
    }

    // ======================================================================
    // Instructions

    /// ADC - Add with Carry
    ///
    /// Add the contents of a memory location to the accumulator
    /// together with the carry bit. If overflow occurs the carry bit
    /// is set, this enables multiple byte addition to be performed.
    #[inline(always)]
    fn adc(&mut self, address: Address) {
        let mem = self.load(address) as DWord;
        let sum = mem + self.accumulator as DWord + self.flags.carry as DWord;

        self.flags.carry = is!(sum & 0b10000000);
        self.flags.overflow =
            (((self.accumulator ^ mem as Word) & 0b01000000) == 0b00000000) &&
            (((self.accumulator ^ sum as Word) & 0b01000000) == 0b01000000);

        self.accumulator = sum as Word;
        self.update_flags()
    }

    /// AND - Logical AND
    ///
    /// A logical AND is performed, bit by bit, on the accumulator
    /// contents using the contents of a byte of memory.
    fn and(&mut self, address: Address) {
        self.accumulator &= self.load(address);
        self.update_flags()
    }

    /// ASL - Arithmetic Shift Left
    ///
    /// This operation shifts all the bits of the accumulator or
    /// memory contents one bit left. Bit 0 is set to 0 and bit 7 is
    /// placed in the carry flag. The effect of this operation is to
    /// multiply the memory contents by 2 (ignoring 2's complement
    /// considerations), setting the carry if the result will not fit
    /// in 8 bits.
    fn asl(&mut self) {
        self.flags.carry = is!(self.accumulator & 0b10000000);
        self.accumulator <<= 1;
        self.update_flags()
    }

    /// BCC - Branch if Carry Clear
    ///
    /// If the carry flag is clear then add the relative displacement
    /// to the program counter to cause a branch to a new location.
    fn bcc(&mut self) {
        branch!(self, !self.flags.carry)
    }

    /// BCS - Branch if Carry Set
    ///
    /// If the carry flag is set then add the relative displacement to
    /// the program counter to cause a branch to a new location.
    fn bcs(&mut self) {
        branch!(self, self.flags.carry)
    }

    /// BEQ - Branch if Equal
    ///
    /// If the zero flag is set then add the relative displacement to
    /// the program counter to cause a branch to a new location.
    fn beq(&mut self) {
        branch!(self, self.flags.zero)
    }

    /// BIT - Bit Test
    ///
    /// This instructions is used to test if one or more bits are set
    /// in a target memory location. The mask pattern in A is ANDed
    /// with the value in memory to set or clear the zero flag, but
    /// the result is not kept. Bits 7 and 6 of the value from memory
    /// are copied into the N and V flags.
    fn bit(&mut self, address: Address) {
        let mem = self.load(address);
        self.flags.zero     = !is!(mem & self.accumulator);
        self.flags.negative =  is!(mem & 0b01000000);
        self.flags.overflow =  is!(mem & 0b00100000);
    }

    /// BMI - Branch if Minus
    ///
    /// If the negative flag is set then add the relative displacement
    /// to the program counter to cause a branch to a new location.
    fn bmi(&mut self) {
        branch!(self, self.flags.negative)
    }

    /// BNE - Branch if Not Equal
    ///
    /// If the zero flag is clear then add the relative displacement
    /// to the program counter to cause a branch to a new location.
    fn bne(&mut self) {
        branch!(self, !self.flags.zero)
    }

    /// BPL - Branch if Positive
    ///
    /// If the negative flag is clear then add the relative
    /// displacement to the program counter to cause a branch to a new
    /// location.
    fn bpl(&mut self) {
        branch!(self, !self.flags.negative)
    }

    /// BRK - Force Interrupt
    ///
    /// The BRK instruction forces the generation of an interrupt
    /// request. The program counter and processor status are pushed
    /// on the stack then the IRQ interrupt vector at $FFFE/F is
    /// loaded into the PC and the break flag in the status set to
    /// one.
    fn brk(&mut self) {
        unimplemented!()
    }

    /// BVC - Branch if Overflow Clear
    ///
    /// If the overflow flag is clear then add the relative
    /// displacement to the program counter to cause a branch to a new
    /// location.
    fn bvc(&mut self) {
        branch!(self, !self.flags.overflow)
    }

    /// BVS - Branch if Overflow Set
    ///
    /// If the overflow flag is set then add the relative displacement
    /// to the program counter to cause a branch to a new location.
    fn bvs(&mut self) {
        branch!(self, self.flags.overflow)
    }

    /// CLC - Clear Carry Flag
    ///
    /// Set the carry flag to zero.
    fn clc(&mut self) {
        self.flags.carry = false;
    }

    /// CLD - Clear Decimal Mode
    ///
    /// Sets the decimal mode flag to zero.
    fn cld(&mut self) {
        self.flags.decimal_mode = false;
    }

    /// CLI - Clear Interrupt Disable
    ///
    /// Clears the interrupt disable flag allowing normal interrupt
    /// requests to be serviced.
    fn cli(&mut self) {
        self.flags.interrupt_disable = false;
    }

    /// CLV - Clear Overflow Flag
    ///
    /// Clears the overflow flag.
    fn clv(&mut self) {
        self.flags.overflow = false;
    }

    /// CMP - Compare
    ///
    /// This instruction compares the contents of the accumulator with
    /// another memory held value and sets the zero and carry flags as
    /// appropriate.
    fn cmp(&mut self, address: Address) {
        compare!(self, self.load(address), self.accumulator)
    }

    /// CPX - Compare X Register
    ///
    /// This instruction compares the contents of the X register with
    /// another memory held value and sets the zero and carry flags as
    /// appropriate.
    fn cpx(&mut self, address: Address) {
        compare!(self, self.load(address), self.register_x)
    }

    /// CPY - Compare Y Register
    ///
    /// This instruction compares the contents of the Y register with
    /// another memory held value and sets the zero and carry flags as
    /// appropriate.
    fn cpy(&mut self, address: Address) {
        compare!(self, self.load(address), self.register_y)
    }

    /// DEC - Decrement Memory
    ///
    /// Subtracts one from the value held at a specified memory
    /// location setting the zero and negative flags as appropriate.
    fn dec(&mut self, address: Address) {
        let result = increment!(self, self.load(address.clone()), -1i8);
        self.store(address, result);
    }

    /// DEX - Decrement X Register
    ///
    /// Subtracts one from the X register setting the zero and
    /// negative flags as appropriate.
    fn dex(&mut self) {
        self.register_x = increment!(self, self.register_x, -1i8);
    }

    /// DEY - Decrement Y Register
    ///
    /// Subtracts one from the Y register setting the zero and
    /// negative flags as appropriate.
    fn dey(&mut self) {
        self.register_y = increment!(self, self.register_y, -1i8);
    }

    /// EOR - Exclusive OR
    ///
    /// An exclusive OR is performed, bit by bit, on the accumulator
    /// contents using the contents of a byte of memory.
    fn eor(&mut self) {
        unimplemented!()
    }

    /// INC - Increment Memory
    ///
    /// Adds one to the value held at a specified memory location
    /// setting the zero and negative flags as appropriate.
    fn inc(&mut self, address: Address) {
        let result = increment!(self, self.load(address.clone()), 1);
        self.store(address, result);
    }

    /// INX - Increment X Register
    ///
    /// Adds one to the X register setting the zero and negative flags
    /// as appropriate.
    fn inx(&mut self) {
        self.register_y = increment!(self, self.register_y, 1);
    }

    /// INY - Increment Y Register
    ///
    /// Adds one to the Y register setting the zero and negative flags
    /// as appropriate.
    fn iny(&mut self) {
        self.register_x = increment!(self, self.register_x, 1);
    }

    /// JMP - Jump
    ///
    /// Sets the program counter to the address specified by the
    /// operand.
    fn jmp(&mut self, address: Address) {
        unimplemented!()
    }

    /// JSR - Jump to Subroutine
    ///
    /// The JSR instruction pushes the address (minus one) of the
    /// return point on to the stack and then sets the program counter
    /// to the target memory address.
    fn jsr(&mut self) {
        unimplemented!()
    }

    /// LDA - Load Accumulator
    ///
    /// Loads a byte of memory into the accumulator setting the zero
    /// and negative flags as appropriate.
    fn lda(&mut self) {
        unimplemented!()
    }

    /// LDX - Load X Register
    ///
    /// Loads a byte of memory into the X register setting the zero
    /// and negative flags as appropriate.
    fn ldx(&mut self) {
        unimplemented!()
    }

    /// LDY - Load Y Register
    ///
    /// Loads a byte of memory into the Y register setting the zero
    /// and negative flags as appropriate.
    fn ldy(&mut self) {
        unimplemented!()
    }

    /// LSR - Logical Shift Right
    ///
    /// Each of the bits in A or M is shift one place to the
    /// right. The bit that was in bit 0 is shifted into the carry
    /// flag. Bit 7 is set to zero.
    fn lsr(&mut self) {
        unimplemented!()
    }

    /// NOP - No Operation
    ///
    /// The NOP instruction causes no changes to the processor other
    /// than the normal incrementing of the program counter to the
    /// next instruction.
    fn nop(&mut self) {
        unimplemented!()
    }

    /// ORA - Logical Inclusive OR
    ///
    /// An inclusive OR is performed, bit by bit, on the accumulator
    /// contents using the contents of a byte of memory.
    fn ora(&mut self) {
        unimplemented!()
    }

    /// PHA - Push Accumulator
    ///
    /// Pushes a copy of the accumulator on to the stack.
    fn pha(&mut self) {
        unimplemented!()
    }

    /// PHP - Push Processor Status
    ///
    /// Pushes a copy of the status flags on to the stack.
    fn php(&mut self) {
        unimplemented!()
    }

    /// PLA - Pull Accumulator
    ///
    /// Pulls an 8 bit value from the stack and into the
    /// accumulator. The zero and negative flags are set as
    /// appropriate.
    fn pla(&mut self) {
        unimplemented!()
    }

    /// PLP - Pull Processor Status
    ///
    /// Pulls an 8 bit value from the stack and into the processor
    /// flags. The flags will take on new states as determined by the
    /// value pulled.
    fn plp(&mut self) {
        unimplemented!()
    }

    /// ROL - Rotate Left
    ///
    /// aMove each of the bits in either A or M one place to the
    /// left. Bit 0 is filled with the current value of the carry flag
    /// whilst the old bit 7 becomes the new carry flag value.
    fn rol(&mut self) {
        unimplemented!()
    }

    /// ROR - Rotate Right
    ///
    /// Move each of the bits in either A or M one place to the
    /// right. Bit 7 is filled with the current value of the carry
    /// flag whilst the old bit 0 becomes the new carry flag value.
    fn ror(&mut self) {
        unimplemented!()
    }

    /// RTI - Return from Interrupt
    ///
    /// The RTI instruction is used at the end of an interrupt
    /// processing routine. It pulls the processor flags from the
    /// stack followed by the program counter.
    fn rti(&mut self) {
        unimplemented!()
    }

    /// RTS - Return from Subroutine
    ///
    /// The RTS instruction is used at the end of a subroutine to
    /// return to the calling routine. It pulls the program counter
    /// (minus one) from the stack.
    fn rts(&mut self) {
        unimplemented!()
    }

    /// SBC - Subtract with Carry
    ///
    /// This instruction subtracts the contents of a memory location
    /// to the accumulator together with the not of the carry bit. If
    /// overflow occurs the carry bit is clear, this enables multiple
    /// byte subtraction to be performed.
    fn sbc(&mut self) {
        unimplemented!()
    }

    /// SEC - Set Carry Flag
    ///
    /// Set the carry flag to one.
    fn sec(&mut self) {
        unimplemented!()
    }

    /// SED - Set Decimal Flag
    ///
    /// Set the decimal mode flag to one.
    fn sed(&mut self) {
        unimplemented!()
    }

    /// SEI - Set Interrupt Disable
    ///
    /// Set the interrupt disable flag to one.
    fn sei(&mut self) {
        unimplemented!()
    }

    /// STA - Store Accumulator
    ///
    /// Stores the contents of the accumulator into memory.
    fn sta(&mut self) {
        unimplemented!()
    }

    /// STX - Store X Register
    ///
    /// Stores the contents of the X register into memory.
    fn stx(&mut self) {
        unimplemented!()
    }

    /// STY - Store Y Register
    ///
    /// Stores the contents of the Y register into memory.
    fn sty(&mut self) {
        unimplemented!()
    }

    /// TAX - Transfer Accumulator to X
    ///
    /// Copies the current contents of the accumulator into the X
    /// register and sets the zero and negative flags as appropriate.
    fn tax(&mut self) {
        unimplemented!()
    }

    /// TAY - Transfer Accumulator to Y
    ///
    /// Copies the current contents of the accumulator into the Y
    /// register and sets the zero and negative flags as appropriate.
    fn tay(&mut self) {
        unimplemented!()
    }

    /// TSX - Transfer Stack Pointer to X
    ///
    /// Copies the current contents of the stack register into the X
    /// register and sets the zero and negative flags as appropriate.
    fn tsx(&mut self) {
        unimplemented!()
    }

    /// TXA - Transfer X to Accumulator
    ///
    /// Copies the current contents of the X register into the
    /// accumulator and sets the zero and negative flags as
    /// appropriate.
    fn txa(&mut self) {
        unimplemented!()
    }

    /// TXS - Transfer X to Stack Pointer
    ///
    /// Copies the current contents of the X register into the stack
    /// register.  fn txs(&mut self) {}
    fn tya(&mut self) {
        unimplemented!()
    }
}
