use address::Address;
use types::*;
use ::instruction::Instruction;

pub trait InstructionSet {
    fn adc(&mut self, _: &Address);
    fn and(&mut self, _: &Address);
    fn asl(&mut self, _: &Address);
    fn bcc(&mut self, _: &Address);
    fn bcs(&mut self, _: &Address);
    fn beq(&mut self, _: &Address);
    fn bit(&mut self, _: &Address);
    fn bmi(&mut self, _: &Address);
    fn bne(&mut self, _: &Address);
    fn bpl(&mut self, _: &Address);
    fn brk(&mut self, _: &Address);
    fn bvc(&mut self, _: &Address);
    fn bvs(&mut self, _: &Address);
    fn clc(&mut self, _: &Address);
    fn cld(&mut self, _: &Address);
    fn cli(&mut self, _: &Address);
    fn clv(&mut self, _: &Address);
    fn cmp(&mut self, _: &Address);
    fn cpx(&mut self, _: &Address);
    fn cpy(&mut self, _: &Address);
    fn dec(&mut self, _: &Address);
    fn dex(&mut self, _: &Address);
    fn dey(&mut self, _: &Address);
    fn eor(&mut self, _: &Address);
    fn inc(&mut self, _: &Address);
    fn inx(&mut self, _: &Address);
    fn iny(&mut self, _: &Address);
    fn jmp(&mut self, _: &Address);
    fn jsr(&mut self, _: &Address);
    fn lda(&mut self, _: &Address);
    fn ldx(&mut self, _: &Address);
    fn ldy(&mut self, _: &Address);
    fn lsr(&mut self, _: &Address);
    fn nop(&mut self, _: &Address);
    fn ora(&mut self, _: &Address);
    fn pha(&mut self, _: &Address);
    fn php(&mut self, _: &Address);
    fn pla(&mut self, _: &Address);
    fn plp(&mut self, _: &Address);
    fn rol(&mut self, _: &Address);
    fn ror(&mut self, _: &Address);
    fn rti(&mut self, _: &Address);
    fn rts(&mut self, _: &Address);
    fn sbc(&mut self, _: &Address);
    fn sec(&mut self, _: &Address);
    fn sed(&mut self, _: &Address);
    fn sei(&mut self, _: &Address);
    fn sta(&mut self, _: &Address);
    fn stx(&mut self, _: &Address);
    fn sty(&mut self, _: &Address);
    fn tax(&mut self, _: &Address);
    fn tay(&mut self, _: &Address);
    fn tsx(&mut self, _: &Address);
    fn txs(&mut self, _: &Address);
    fn txa(&mut self, _: &Address);
    fn tya(&mut self, _: &Address);
    fn execute_instruction(&mut self, instruction: &Instruction);
}

use cpu::{Cpu, Flags};
use ::stack::*;
use std::fmt::Display;
impl Cpu {
    /// Increments the program and returns the previous value
    #[inline(always)]
    pub fn compare<T: Ord>(&mut self, a: T, b: T) {
        self.flags.zero     = a.eq(&b);
        self.flags.negative = a.gt(&b);
    }

    // /// All branches are relative mode and have a length of two
    // /// bytes
    // ///
    // /// A branch not taken requires two machine cycles. Add one if the
    // /// branch is taken and add one more if the branch crosses a page
    // /// boundary.
    // #[inline(always)]
    // pub fn branch(&mut self, condition: bool, address: &Address) {
    //     if condition {

    //     }
    // }
}

#[macro_export]
macro_rules! set_flags {
    ($self:ident, $result: expr) => {{
        $self.flags.zero = $result == 0;
        $self.flags.negative = is!($result & 0b10000000);
    }};
}

#[macro_export]
macro_rules! branch {
    ($self:ident, $condition: expr, $address: expr) => {{
        if $condition {
            $self.program_counter = $self.compute_address($address);
        }
    }};
}

impl InstructionSet for Cpu {
    /// ADC - Add with Carry
    ///
    /// Add the contents of a memory location to the accumulator
    /// together with the carry bit. If overflow occurs the carry bit
    /// is set, this enables multiple byte addition to be performed.
    #[inline(always)]
    fn adc(&mut self, address: &Address) {
        let mem = self.load(address) as Word;
        let sum = mem + self.accumulator as Word + self.flags.carry as Word;

        self.flags.carry = is!(sum & 0b10000000);
        self.flags.overflow =
            (((self.accumulator ^ mem as Byte) & 0b01000000) == 0b00000000) &&
            (((self.accumulator ^ sum as Byte) & 0b01000000) == 0b01000000);

        self.accumulator = sum as Byte;
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
    fn bcc(&mut self, address: &Address) {
        branch!(self, !self.flags.carry, address)
    }

    /// BCS - Branch if Carry Set
    ///
    /// If the carry flag is set then add the relative displacement to
    /// the program counter to cause a branch to a new location.
    fn bcs(&mut self, address: &Address) {
        branch!(self, self.flags.carry, address)
    }

    /// BEQ - Branch if Equal
    ///
    /// If the zero flag is set then add the relative displacement to
    /// the program counter to cause a branch to a new location.
    fn beq(&mut self, address: &Address) {
        branch!(self, self.flags.zero, address)
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
    fn bmi(&mut self, address: &Address) {
        branch!(self, self.flags.negative, address)
    }

    /// BNE - Branch if Not Equal
    ///
    /// If the zero flag is clear then add the relative displacement
    /// to the program counter to cause a branch to a new location.
    fn bne(&mut self, address: &Address) {
        branch!(self, !self.flags.zero, address)
    }

    /// BPL - Branch if Positive
    ///
    /// If the negative flag is clear then add the relative
    /// displacement to the program counter to cause a branch to a new
    /// location.
    fn bpl(&mut self, address: &Address) {
        branch!(self, !self.flags.negative, address)
    }

    /// BRK - Force Interrupt
    ///
    /// The BRK instruction forces the generation of an interrupt
    /// request. The program counter and processor status are pushed
    /// on the stack then the IRQ interrupt vector at $FFFE/F is
    /// loaded into the PC and the break flag in the status set to
    /// one.
    fn brk(&mut self, _: &Address) {
        let program_counter = self.program_counter;
        let flags = self.flags.to_byte();

        self.push_word(program_counter);
        self.push_word(program_counter+1);
        self.push(flags);
        self.flags.break_mode = true; // TODO ?
        self.program_counter = self.load_word(&Address::Absolute(0xFFFE));
    }

    /// BVC - Branch if Overflow Clear
    ///
    /// If the overflow flag is clear then add the relative
    /// displacement to the program counter to cause a branch to a new
    /// location.
    fn bvc(&mut self, _: &Address) {
        // branch!(self, !self.flags.overflow)
    }

    /// BVS - Branch if Overflow Set
    ///
    /// If the overflow flag is set then add the relative displacement
    /// to the program counter to cause a branch to a new location.
    fn bvs(&mut self, _: &Address) {
        // branch!(self, self.flags.overflow)
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
        // TODO
    }

    /// INC - Increment Memory
    ///
    /// adds one to the value held at a specified memory location
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
        self.register_x = increment!(self, self.register_x, 1);
    }

    /// INY - Increment Y Register
    ///
    /// Adds one to the Y register setting the zero and negative flags
    /// as appropriate.
    fn iny(&mut self, _: &Address) {
        self.register_y = increment!(self, self.register_y, 1);
    }

    /// JMP - Jump
    ///
    /// Sets the program counter to the address specified by the
    /// operand.
    fn jmp(&mut self, address: &Address) {
        self.program_counter = self.compute_address(address);
    }

    /// JSR - Jump to Subroutine
    ///
    /// The JSR instruction pushes the address (minus one) of the
    /// return point on to the stack and then sets the program counter
    /// to the target memory address.
    fn jsr(&mut self, address: &Address) {
        let return_point = self.program_counter - 1;
        self.push_word(return_point);
        self.program_counter = self.compute_address(address);
    }

    /// LDA - Load Accumulator
    ///
    /// Loads a byte of memory into the accumulator setting the zero
    /// and negative flags as appropriate.
    fn lda(&mut self, address: &Address) {
        self.accumulator = self.load(address);
        set_flags!(self, self.accumulator)
    }

    /// LDX - Load X Register
    ///
    /// Loads a byte of memory into the X register setting the zero
    /// and negative flags as appropriate.
    fn ldx(&mut self, address: &Address) {
        self.register_x = self.load(address);
        set_flags!(self, self.register_x)
    }

    /// LDY - Load Y Register
    ///
    /// Loads a byte of memory into the Y register setting the zero
    /// and negative flags as appropriate.
    fn ldy(&mut self, address: &Address) {
        self.register_y = self.load(address);
        set_flags!(self, self.register_y);
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
        set_flags!(self, result);
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
        set_flags!(self, self.accumulator);
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
        let status = self.flags.to_byte();
        self.push(status);
    }

    /// PLA - Pull Accumulator
    ///
    /// Pulls an 8 bit value from the stack and into the
    /// accumulator. The zero and negative flags are set as
    /// appropriate.
    fn pla(&mut self, _: &Address) {
        self.accumulator = self.pop();
        set_flags!(self, self.accumulator);
    }

    /// PLP - Pull Processor Status
    ///
    /// Pulls an 8 bit value from the stack and into the processor
    /// flags. The flags will take on new states as determined by the
    /// value pulled.
    fn plp(&mut self, _: &Address) {
        self.flags = Flags::from_byte(self.pop());
    }

    /// ROL - Rotate Left
    ///
    /// Move each of the bits in either A or M one place to the
    /// left. Bit 0 is filled with the current value of the carry flag
    /// whilst the old bit 7 becomes the new carry flag value.
    fn rol(&mut self, address: &Address) {
        let value = self.load(address);
        let new_value = (value << 1) | (self.flags.carry as Byte);
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
        let new_value = (value >> 1) | ((self.flags.carry as Byte) << 7);
        self.flags.carry = is!(value & 0b000000001);
        self.store(address, new_value);
    }

    /// RTI - Return from Interrupt
    ///
    /// The RTI instruction is used at the end of an interrupt
    /// processing routine. It pulls the processor flags from the
    /// stack followed by the program counter.
    fn rti(&mut self, _: &Address) {
        // TODO
    }

    /// RTS - Return from Subroutine
    ///
    /// The RTS instruction is used at the end of a subroutine to
    /// return to the calling routine. It pulls the program counter
    /// (minus one) from the stack.
    fn rts(&mut self, _: &Address) {
        let return_point = self.pop_word();
        self.program_counter = return_point + 1;
    }

    /// SBC - Subtract with Carry
    ///
    /// This instruction subtracts the contents of a memory location
    /// to the accumulator together with the not of the carry bit. If
    /// overflow occurs the carry bit is clear, this enables multiple
    /// byte subtraction to be performed.
    fn sbc(&mut self, address: &Address) {
        let mem = self.load(address) as Word;
        let dif = self.accumulator as Word - mem - self.flags.carry as Word;

        self.flags.carry = is!(dif & 0b10000000);
        self.flags.overflow =
            (((self.accumulator ^ dif as Byte) & 0b01000000) == 0b00000000) &&
            (((self.accumulator ^ mem as Byte) & 0b01000000) == 0b01000000);

        self.accumulator = dif as Byte;
        self.update_flags()
    }

    /// SEC - Set Carry Flag
    ///
    /// Set the carry flag to one.
    fn sec(&mut self, _: &Address) {
        self.flags.carry = true;
    }

    /// SED - Set Decimal Flag
    ///
    /// Set the decimal mode flag to one.
    fn sed(&mut self, _: &Address) {
        self.flags.decimal_mode = true;
    }

    /// SEI - Set Interrupt Disable
    ///
    /// Set the interrupt disable flag to one.
    fn sei(&mut self, _: &Address) {
        self.flags.interrupt_disable = true;
    }

    /// STA - Store Accumulator
    ///
    /// Stores the contents of the accumulator into memory.
    fn sta(&mut self, address: &Address) {
        let value = self.accumulator;
        self.store(address, value)
    }

    /// STX - Store X Register
    ///
    /// Stores the contents of the X register into memory.
    fn stx(&mut self, address: &Address) {
        let value = self.register_x;
        self.store(address, value)
    }

    /// STY - Store Y Register
    ///
    /// Stores the contents of the Y register into memory.
    fn sty(&mut self, address: &Address) {
        let value = self.register_y;
        self.store(address, value);
    }

    /// TAX - Transfer Accumulator to X
    ///
    /// Copies the current contents of the accumulator into the X
    /// register and sets the zero and negative flags as appropriate.
    fn tax(&mut self, _: &Address) {
        self.register_x = self.accumulator;
        self.update_flags();
    }

    /// TAY - Transfer Accumulator to Y
    ///
    /// Copies the current contents of the accumulator into the Y
    /// register and sets the zero and negative flags as appropriate.
    fn tay(&mut self, _: &Address) {
        let value = self.stack_pointer;
        self.register_y = value;
        self.update_flags_with(value);
    }

    /// TSX - Transfer Stack Pointer to X
    ///
    /// Copies the current contents of the stack register into the X
    /// register and sets the zero and negative flags as appropriate.
    fn tsx(&mut self, _: &Address) {
        let value = self.stack_pointer;
        self.register_x = value;
        self.update_flags_with(value);
    }

    /// TXS - Transfer X to Stack Pointer
    ///
    /// Copies the current contents of the X register into the stack pointer
    /// register and sets the zero and negative flags as appropriate.
    fn txs(&mut self, _: &Address) {
        let value = self.register_x;
        self.stack_pointer = value;
        self.update_flags_with(value);
    }

    /// TXA - Transfer X to Accumulator
    ///
    /// Copies the current contents of the X register into the
    /// accumulator and sets the zero and negative flags as
    /// appropriate.
    fn txa(&mut self, _: &Address) {
        self.accumulator = self.register_x;
        self.update_flags();
    }

    /// TXS - Transfer X to Stack Pointer
    ///
    /// Copies the current contents of the X register into the stack
    /// register.
    fn tya(&mut self, _: &Address) {
        let value = self.register_x;
        self.stack_pointer = self.register_x;
    }

    /// Executes provided instruction
    fn execute_instruction(&mut self, instruction: &Instruction) {
        trace!("Executing instruction {:?}", instruction);
        match *instruction {
            Instruction::ADC(ref address) => self.adc(address),
            Instruction::AND(ref address) => self.and(address),
            Instruction::ASL(ref address) => self.asl(address),
            Instruction::BCC(ref address) => self.bcc(address),
            Instruction::BCS(ref address) => self.bcs(address),
            Instruction::BEQ(ref address) => self.beq(address),
            Instruction::BIT(ref address) => self.bit(address),
            Instruction::BMI(ref address) => self.bmi(address),
            Instruction::BNE(ref address) => self.bne(address),
            Instruction::BPL(ref address) => self.bpl(address),
            Instruction::BRK(ref address) => self.brk(address),
            Instruction::BVC(ref address) => self.bvc(address),
            Instruction::BVS(ref address) => self.bvs(address),
            Instruction::CLC(ref address) => self.clc(address),
            Instruction::CLD(ref address) => self.cld(address),
            Instruction::CLI(ref address) => self.cli(address),
            Instruction::CLV(ref address) => self.clv(address),
            Instruction::CMP(ref address) => self.cmp(address),
            Instruction::CPX(ref address) => self.cpx(address),
            Instruction::CPY(ref address) => self.cpy(address),
            Instruction::DEC(ref address) => self.dec(address),
            Instruction::DEX(ref address) => self.dex(address),
            Instruction::DEY(ref address) => self.dey(address),
            Instruction::EOR(ref address) => self.eor(address),
            Instruction::INC(ref address) => self.inc(address),
            Instruction::INX(ref address) => self.inx(address),
            Instruction::INY(ref address) => self.iny(address),
            Instruction::JMP(ref address) => self.jmp(address),
            Instruction::JSR(ref address) => self.jsr(address),
            Instruction::LDA(ref address) => self.lda(address),
            Instruction::LDX(ref address) => self.ldx(address),
            Instruction::LDY(ref address) => self.ldy(address),
            Instruction::LSR(ref address) => self.lsr(address),
            Instruction::NOP(ref address) => self.nop(address),
            Instruction::ORA(ref address) => self.ora(address),
            Instruction::PHA(ref address) => self.pha(address),
            Instruction::PHP(ref address) => self.php(address),
            Instruction::PLA(ref address) => self.pla(address),
            Instruction::PLP(ref address) => self.plp(address),
            Instruction::ROL(ref address) => self.rol(address),
            Instruction::ROR(ref address) => self.ror(address),
            Instruction::RTI(ref address) => self.rti(address),
            Instruction::RTS(ref address) => self.rts(address),
            Instruction::SBC(ref address) => self.sbc(address),
            Instruction::SEC(ref address) => self.sec(address),
            Instruction::SED(ref address) => self.sed(address),
            Instruction::SEI(ref address) => self.sei(address),
            Instruction::STA(ref address) => self.sta(address),
            Instruction::STX(ref address) => self.stx(address),
            Instruction::STY(ref address) => self.sty(address),
            Instruction::TAX(ref address) => self.tax(address),
            Instruction::TAY(ref address) => self.tay(address),
            Instruction::TSX(ref address) => self.tsx(address),
            Instruction::TXA(ref address) => self.txa(address),
            Instruction::TXS(ref address) => self.txs(address),
            Instruction::TYA(ref address) => self.tya(address),
        }
        trace!("Executed instruction {:?}", instruction);
    }
}
