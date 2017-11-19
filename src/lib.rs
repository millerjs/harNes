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

struct Ram {
    main: Vec<Word>,
}

impl Memory for Ram {
    fn read(&self, address: &Address) -> Word {
        0
    }

    fn write(&mut self, address: &Address, value: Word) {

    }
}

pub struct Clock {
    rate: usize,
}

#[derive(Clone, Debug)]
pub enum Address {
    /// A full 16-bit address is specified and the byte at that
    /// address is used to perform the computation. (e.g. LDX)
    Absolute(DWord),

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
    Immediate(Word),

    /// The operand is implied, so it does not need to be
    /// specified. (e.g. TXA)
    Implicit,

    /// The JMP instruction is the only instruction that uses this
    /// addressing mode. It is a 3 byte instruction - the 2nd and 3rd
    /// bytes are an absolute address. The set the PC to the address
    /// stored at that address. So maybe this would be clearer.
    Indirect(Word),

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
    IndirectIndexed(Word),

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
    IndexedIndirect(Word),

    /// The offset specified is added to the current address stored in
    /// the Program Counter (PC). Offsets can range from -128 to
    /// +127. (e.g. BPL)
    Relative(Word),

    /// A single byte specifies an address in the first page of memory
    /// ($00xx), also known as the zero page, and the byte at that
    /// address is used to perform the computation. (e.g. LDY)
    ZeroPage(Word),

    /// The value in X is added to the specified zero page address for
    /// a sum address. The value at the sum address is used to perform
    /// the computation.
    ZeroPageX(Word),

    /// The value in Y is added to the specified zero page address for
    /// a sum address. The value at the sum address is used to perform
    /// the computation.
    ZeroPageY(Word),
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
    fn read(&self, address: &Address) -> Word;
    fn write(&mut self, address: &Address, value: Word);
}

#[derive(Default)]
pub struct Flags {
    pub carry: bool,
    pub zero: bool,
    pub interrupt_disable: bool,
    pub decimal_mode: bool,
    pub break_mode: bool,
    pub overflow: bool,
    pub negative: bool,
}

#[derive(Default)]
pub struct Cpu<M> {
    memory: Box<M>,
    cycles: usize,
    pub program_counter: DWord,
    pub stack_pointer: DWord,
    pub accumulator: Word,
    pub register_x: Word,
    pub register_y: Word,
    interrupt: Word,
    pub stall: usize,
    pub flags: Flags,
}

macro_rules! is { ($value: expr) => { $value != 0 }; }

macro_rules! mask {
    ($flag: expr, $bit: expr) => {
        $flag as i8 << $bit
    };
}

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

impl Flags {
    fn to_word(&self) -> Word {
        let mut status = 0;
        status &=  self.carry             as u8;
        status &= (self.zero              as u8) << 1;
        status &= (self.interrupt_disable as u8) << 2;
        status &= (self.decimal_mode      as u8) << 3;
        status &= (self.break_mode        as u8) << 4;
        status &= (self.overflow          as u8) << 5;
        status &= (self.negative          as u8) << 7;
        status
    }

    fn from_word(word: Word) -> Flags {
        Flags {
            carry:             is!(word & 0b00000001),
            zero:              is!(word & 0b00000010),
            interrupt_disable: is!(word & 0b00000100),
            decimal_mode:      is!(word & 0b00001000),
            break_mode:        is!(word & 0b00010000),
            overflow:          is!(word & 0b00100000),
            negative:          is!(word & 0b10000000),
        }
    }
}

impl<M> Cpu<M> where M: Memory {
    /// Delegates loading of address in memory or loads from register
    #[inline(always)]
    fn load(&self, address: &Address) -> Word {
        match *address {
            Address::Accumulator => self.accumulator,
            _                    => self.memory.read(address),
        }

    }

    /// Delegates writing of value to address in memory or writes to
    /// register
    #[inline(always)]
    fn store(&mut self, address: &Address, value: Word) {
        match *address {
            Address::Accumulator => self.accumulator = value,
            _                    => self.memory.write(address, value),
        }
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
        let address = self.increment_program_counter();
        let delta = self.load(&Address::Absolute(address)) as i8;
        if condition {
            self.program_counter = (self.program_counter as i32 + delta as i32) as DWord;
        }
    }

    // ======================================================================
    // Instructions

    /// ADC - Add with Carry
    ///
    /// Add the contents of a memory location to the accumulator
    /// together with the carry bit. If overflow occurs the carry bit
    /// is set, this enables multiple byte addition to be performed.
    #[inline(always)]
    fn adc(&mut self, address: &Address) {
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
    fn and(&mut self, address: &Address) {
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
    fn asl(&mut self, _: &Address) {
        self.flags.carry = is!(self.accumulator & 0b10000000);
        self.accumulator <<= 1;
        self.update_flags()
    }

    /// BCC - Branch if Carry Clear
    ///
    /// If the carry flag is clear then add the relative displacement
    /// to the program counter to cause a branch to a new location.
    fn bcc(&mut self, _: &Address) {
        branch!(self, !self.flags.carry)
    }

    /// BCS - Branch if Carry Set
    ///
    /// If the carry flag is set then add the relative displacement to
    /// the program counter to cause a branch to a new location.
    fn bcs(&mut self, _: &Address) {
        branch!(self, self.flags.carry)
    }

    /// BEQ - Branch if Equal
    ///
    /// If the zero flag is set then add the relative displacement to
    /// the program counter to cause a branch to a new location.
    fn beq(&mut self, _: &Address) {
        branch!(self, self.flags.zero)
    }

    /// BIT - Bit Test
    ///
    /// This instructions is used to test if one or more bits are set
    /// in a target memory location. The mask pattern in A is ANDed
    /// with the value in memory to set or clear the zero flag, but
    /// the result is not kept. Bits 7 and 6 of the value from memory
    /// are copied into the N and V flags.
    fn bit(&mut self, address: &Address) {
        let mem = self.load(address);
        self.flags.zero     = !is!(mem & self.accumulator);
        self.flags.negative =  is!(mem & 0b01000000);
        self.flags.overflow =  is!(mem & 0b00100000);
    }

    /// BMI - Branch if Minus
    ///
    /// If the negative flag is set then add the relative displacement
    /// to the program counter to cause a branch to a new location.
    fn bmi(&mut self, _: &Address) {
        branch!(self, self.flags.negative)
    }

    /// BNE - Branch if Not Equal
    ///
    /// If the zero flag is clear then add the relative displacement
    /// to the program counter to cause a branch to a new location.
    fn bne(&mut self, _: &Address) {
        branch!(self, !self.flags.zero)
    }

    /// BPL - Branch if Positive
    ///
    /// If the negative flag is clear then add the relative
    /// displacement to the program counter to cause a branch to a new
    /// location.
    fn bpl(&mut self, _: &Address) {
        branch!(self, !self.flags.negative)
    }

    /// BRK - Force Interrupt
    ///
    /// The BRK instruction forces the generation of an interrupt
    /// request. The program counter and processor status are pushed
    /// on the stack then the IRQ interrupt vector at $FFFE/F is
    /// loaded into the PC and the break flag in the status set to
    /// one.
    fn brk(&mut self, _: &Address) {
        unimplemented!()
    }

    /// BVC - Branch if Overflow Clear
    ///
    /// If the overflow flag is clear then add the relative
    /// displacement to the program counter to cause a branch to a new
    /// location.
    fn bvc(&mut self, _: &Address) {
        branch!(self, !self.flags.overflow)
    }

    /// BVS - Branch if Overflow Set
    ///
    /// If the overflow flag is set then add the relative displacement
    /// to the program counter to cause a branch to a new location.
    fn bvs(&mut self, _: &Address) {
        branch!(self, self.flags.overflow)
    }

    /// CLC - Clear Carry Flag
    ///
    /// Set the carry flag to zero.
    fn clc(&mut self, _: &Address) {
        self.flags.carry = false;
    }

    /// CLD - Clear Decimal Mode
    ///
    /// Sets the decimal mode flag to zero.
    fn cld(&mut self, _: &Address) {
        self.flags.decimal_mode = false;
    }

    /// CLI - Clear Interrupt Disable
    ///
    /// Clears the interrupt disable flag allowing normal interrupt
    /// requests to be serviced.
    fn cli(&mut self, _: &Address) {
        self.flags.interrupt_disable = false;
    }

    /// CLV - Clear Overflow Flag
    ///
    /// Clears the overflow flag.
    fn clv(&mut self, _: &Address) {
        self.flags.overflow = false;
    }

    /// CMP - Compare
    ///
    /// This instruction compares the contents of the accumulator with
    /// another memory held value and sets the zero and carry flags as
    /// appropriate.
    fn cmp(&mut self, address: &Address) {
        compare!(self, self.load(address), self.accumulator)
    }

    /// CPX - Compare X Register
    ///
    /// This instruction compares the contents of the X register with
    /// another memory held value and sets the zero and carry flags as
    /// appropriate.
    fn cpx(&mut self, address: &Address) {
        compare!(self, self.load(address), self.register_x)
    }

    /// CPY - Compare Y Register
    ///
    /// This instruction compares the contents of the Y register with
    /// another memory held value and sets the zero and carry flags as
    /// appropriate.
    fn cpy(&mut self, address: &Address) {
        compare!(self, self.load(address), self.register_y)
    }

    /// DEC - Decrement Memory
    ///
    /// Subtracts one from the value held at a specified memory
    /// location setting the zero and negative flags as appropriate.
    fn dec(&mut self, address: &Address) {
        let result = increment!(self, self.load(address), -1i8);
        self.store(address, result);
    }

    /// DEX - Decrement X Register
    ///
    /// Subtracts one from the X register setting the zero and
    /// negative flags as appropriate.
    fn dex(&mut self, _: &Address) {
        self.register_x = increment!(self, self.register_x, -1i8);
    }

    /// DEY - Decrement Y Register
    ///
    /// Subtracts one from the Y register setting the zero and
    /// negative flags as appropriate.
    fn dey(&mut self, _: &Address) {
        self.register_y = increment!(self, self.register_y, -1i8);
    }

    /// EOR - Exclusive OR
    ///
    /// An exclusive OR is performed, bit by bit, on the accumulator
    /// contents using the contents of a byte of memory.
    fn eor(&mut self, _: &Address) {
        unimplemented!()
    }

    /// INC - Increment Memory
    ///
    /// Adds one to the value held at a specified memory location
    /// setting the zero and negative flags as appropriate.
    fn inc(&mut self, address: &Address) {
        let result = increment!(self, self.load(address), 1);
        self.store(address, result);
    }

    /// INX - Increment X Register
    ///
    /// Adds one to the X register setting the zero and negative flags
    /// as appropriate.
    fn inx(&mut self, _: &Address) {
        self.register_y = increment!(self, self.register_y, 1);
    }

    /// INY - Increment Y Register
    ///
    /// Adds one to the Y register setting the zero and negative flags
    /// as appropriate.
    fn iny(&mut self, _: &Address) {
        self.register_x = increment!(self, self.register_x, 1);
    }

    /// JMP - Jump
    ///
    /// Sets the program counter to the address specified by the
    /// operand.
    fn jmp(&mut self, address: &Address) {
        unimplemented!()
    }

    /// JSR - Jump to Subroutine
    ///
    /// The JSR instruction pushes the address (minus one) of the
    /// return point on to the stack and then sets the program counter
    /// to the target memory address.
    fn jsr(&mut self, _: &Address) {
        unimplemented!()
    }

    /// LDA - Load Accumulator
    ///
    /// Loads a byte of memory into the accumulator setting the zero
    /// and negative flags as appropriate.
    fn lda(&mut self, address: &Address) {
        self.accumulator = self.load(address);
        compare!(self, self.accumulator, 0)
    }

    /// LDX - Load X Register
    ///
    /// Loads a byte of memory into the X register setting the zero
    /// and negative flags as appropriate.
    fn ldx(&mut self, address: &Address) {
        self.register_x = self.load(address);
        compare!(self, self.register_x, 0)
    }

    /// LDY - Load Y Register
    ///
    /// Loads a byte of memory into the Y register setting the zero
    /// and negative flags as appropriate.
    fn ldy(&mut self, address: &Address) {
        self.register_y = self.load(address);
        compare!(self, self.register_y, 0)
    }

    /// LSR - Logical Shift Right
    ///
    /// Each of the bits in A or M is shift one place to the
    /// right. The bit that was in bit 0 is shifted into the carry
    /// flag. Bit 7 is set to zero.
    fn lsr(&mut self, address: &Address) {
        let value = self.load(address);
        self.flags.carry = is!(0b10000000 & value);
        let result = (value << 1) & 0b1;
        compare!(self, result, 0);
        self.store(address, value);
    }

    /// NOP - No Operation
    ///
    /// The NOP instruction causes no changes to the processor other
    /// than the normal incrementing of the program counter to the
    /// next instruction.
    fn nop(&mut self, address: &Address) {}

    /// ORA - Logical Inclusive OR
    ///
    /// An inclusive OR is performed, bit by bit, on the accumulator
    /// contents using the contents of a byte of memory.
    fn ora(&mut self, address: &Address) {
        self.accumulator = self.load(address) | self.accumulator;
        compare!(self, self.accumulator, 0);
    }

    fn push(&mut self, value: Word) {
        let address = &Address::Absolute(0x100 + self.stack_pointer);
        self.stack_pointer -= 1;
        self.store(address, value);
    }

    fn pop(&mut self) -> Word {
        self.stack_pointer += 1;
        let address = &Address::Absolute(0x100 + self.stack_pointer);
        self.load(address)
    }

    /// PHA - Push Accumulator
    ///
    /// Pushes a copy of the accumulator on to the stack.
    fn pha(&mut self, _: &Address) {
        let value = self.accumulator;
        self.push(value);
    }

    /// PHP - Push Processor Status
    ///
    /// Pushes a copy of the status flags on to the stack.
    fn php(&mut self, _: &Address) {
        let status = self.flags.to_word();
        self.push(status);
    }

    /// PLA - Pull Accumulator
    ///
    /// Pulls an 8 bit value from the stack and into the
    /// accumulator. The zero and negative flags are set as
    /// appropriate.
    fn pla(&mut self, _: &Address) {
        self.accumulator = self.pop();
        compare!(self, self.accumulator, 0);
    }

    /// PLP - Pull Processor Status
    ///
    /// Pulls an 8 bit value from the stack and into the processor
    /// flags. The flags will take on new states as determined by the
    /// value pulled.
    fn plp(&mut self, _: &Address) {
        self.flags = Flags::from_word(self.pop());
    }

    /// ROL - Rotate Left
    ///
    /// Move each of the bits in either A or M one place to the
    /// left. Bit 0 is filled with the current value of the carry flag
    /// whilst the old bit 7 becomes the new carry flag value.
    fn rol(&mut self, address: &Address) {
        let value = self.load(address);
        let new_value = (value << 1) | (self.flags.carry as Word);
        self.flags.carry = is!(value & 0b010000000);
        self.store(address, new_value);
    }

    /// ROR - Rotate Right
    ///
    /// Move each of the bits in either A or M one place to the
    /// right. Bit 7 is filled with the current value of the carry
    /// flag whilst the old bit 0 becomes the new carry flag value.
    fn ror(&mut self, address: &Address) {
        let value = self.load(address);
        let new_value = (value >> 1) | ((self.flags.carry as Word) << 7);
        self.flags.carry = is!(value & 0b000000001);
        self.store(address, new_value);
    }

    /// RTI - Return from Interrupt
    ///
    /// The RTI instruction is used at the end of an interrupt
    /// processing routine. It pulls the processor flags from the
    /// stack followed by the program counter.
    fn rti(&mut self, _: &Address) {
        unimplemented!()
    }

    /// RTS - Return from Subroutine
    ///
    /// The RTS instruction is used at the end of a subroutine to
    /// return to the calling routine. It pulls the program counter
    /// (minus one) from the stack.
    fn rts(&mut self, _: &Address) {
        unimplemented!()
    }

    /// SBC - Subtract with Carry
    ///
    /// This instruction subtracts the contents of a memory location
    /// to the accumulator together with the not of the carry bit. If
    /// overflow occurs the carry bit is clear, this enables multiple
    /// byte subtraction to be performed.
    fn sbc(&mut self, _: &Address) {
        unimplemented!()
    }

    /// SEC - Set Carry Flag
    ///
    /// Set the carry flag to one.
    fn sec(&mut self, _: &Address) {
        unimplemented!()
    }

    /// SED - Set Decimal Flag
    ///
    /// Set the decimal mode flag to one.
    fn sed(&mut self, _: &Address) {
        unimplemented!()
    }

    /// SEI - Set Interrupt Disable
    ///
    /// Set the interrupt disable flag to one.
    fn sei(&mut self, _: &Address) {
        unimplemented!()
    }

    /// STA - Store Accumulator
    ///
    /// Stores the contents of the accumulator into memory.
    fn sta(&mut self, _: &Address) {
        unimplemented!()
    }

    /// STX - Store X Register
    ///
    /// Stores the contents of the X register into memory.
    fn stx(&mut self, _: &Address) {
        unimplemented!()
    }

    /// STY - Store Y Register
    ///
    /// Stores the contents of the Y register into memory.
    fn sty(&mut self, _: &Address) {
        unimplemented!()
    }

    /// TAX - Transfer Accumulator to X
    ///
    /// Copies the current contents of the accumulator into the X
    /// register and sets the zero and negative flags as appropriate.
    fn tax(&mut self, _: &Address) {
        unimplemented!()
    }

    /// TAY - Transfer Accumulator to Y
    ///
    /// Copies the current contents of the accumulator into the Y
    /// register and sets the zero and negative flags as appropriate.
    fn tay(&mut self, _: &Address) {
        unimplemented!()
    }

    /// TSX - Transfer Stack Pointer to X
    ///
    /// Copies the current contents of the stack register into the X
    /// register and sets the zero and negative flags as appropriate.
    fn tsx(&mut self, _: &Address) {
        unimplemented!()
    }

    /// TXA - Transfer X to Accumulator
    ///
    /// Copies the current contents of the X register into the
    /// accumulator and sets the zero and negative flags as
    /// appropriate.
    fn txa(&mut self, _: &Address) {
        unimplemented!()
    }

    /// TXS - Transfer X to Stack Pointer
    ///
    /// Copies the current contents of the X register into the stack
    /// register.  fn txs(&mut self, _: &Address) {}
    fn tya(&mut self, _: &Address) {
        unimplemented!()
    }
}
