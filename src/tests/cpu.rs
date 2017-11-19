use ::*;
use ::tests::*;


#[cfg(test)]
fn simple_cpu(accumulator: Byte, mem: Byte) -> Cpu<MappedMemory> {
    CpuBuilder::default()
        .accumulator(accumulator)
        .store(&Address::Absolute(0), mem)
        .build()
}


#[cfg(test)]
mod test_adc {
    use super::*;

    fn adc(accumulator: Byte, mem: Byte) -> Cpu<MappedMemory> {
        let mut cpu = CpuBuilder::default().
            accumulator(accumulator).
            store(&Address::Absolute(0), mem).
            build();
        cpu.adc(&Address::Absolute(0));
        cpu
    }

    #[test]
    fn test_no_overflow() {
        let cpu = adc(2, 3);
        assert_eq!(cpu.accumulator, 5);
    }

    #[test]
    fn test_differnt_signs() {
        let cpu = adc(4, (-1 as i8) as u8);
        assert_eq!(cpu.accumulator, 3);
    }

    #[test]
    fn test_carry_positive() {
        let cpu = adc(0b01111111, 0b00000001);
        assert_eq!(cpu.accumulator, 0b10000000);
        assert_eq!(cpu.flags.carry, true);
    }

    #[test]
    fn test_carry_negative() {
        let cpu = adc(0b10000000, 0b11111111);
        assert_eq!(cpu.accumulator, 0b01111111);
        assert_eq!(cpu.flags.carry, false);
    }

    #[test]
    fn test_overflow() {
        let cpu = adc(0b1111111, 0b00000001);
        assert_eq!(cpu.accumulator, 0b10000000);
        assert_eq!(cpu.flags.carry, true);
    }

    #[test]
    fn test_underflow() {
        let cpu = adc(4, 0b11111011);
        assert_eq!(cpu.accumulator as i8, -1);
    }
}

#[cfg(test)]
mod test_update_flags {
    use super::*;
    #[test]
    fn test_negative_negative() {
        let mut cpu: Cpu<MappedMemory> = Cpu::default();
        cpu.accumulator = 0b11111111;
        cpu.update_flags();
        assert_eq!(cpu.flags.negative, true);
    }

    #[test]
    fn test_negative_positive() {
        let mut cpu: Cpu<MappedMemory> = Cpu::default();
        cpu.accumulator = 0b01111111;
        cpu.update_flags();
        assert_eq!(cpu.flags.negative, false);
    }

    #[test]
    fn test_zero_zero() {
        let mut cpu: Cpu<MappedMemory> = Cpu::default();
        cpu.accumulator = 0b00000000;
        cpu.update_flags();
        assert_eq!(cpu.flags.zero, true);
    }

    #[test]
    fn test_zero_nonzero() {
        let mut cpu: Cpu<MappedMemory> = Cpu::default();
        cpu.accumulator = 0b00000001;
        cpu.update_flags();
        assert_eq!(cpu.flags.zero, false);
    }
}

#[cfg(test)]
mod test_and {
    use super::*;

    fn and(accumulator: Byte, mem: Byte) -> Cpu<MappedMemory> {
        let mut cpu = CpuBuilder::default()
            .accumulator(accumulator)
            .store(&Address::Absolute(0), mem)
            .build();
        cpu.and(&Address::Absolute(0));
        cpu
    }

    #[test]
    fn test_and() {
        let cpu = and(0b01001100, 0b01100101);
        assert_eq!(cpu.accumulator, 0b01000100);
    }
}


#[cfg(test)]
mod test_rol {
    use super::*;

    #[test]
    fn test_rol() {
        let mut cpu = CpuBuilder::default().accumulator(0b10110010).build();
        cpu.rol(&Address::Accumulator);
        assert_eq!(cpu.accumulator, 0b01100100);
    }

    #[test]
    fn test_rol_carry() {
        let mut cpu = CpuBuilder::default().accumulator(0b10110010).carry(true).build();
        cpu.rol(&Address::Accumulator);
        assert_eq!(cpu.accumulator, 0b01100101);
    }
}


#[cfg(test)]
mod test_ror {
    use super::*;

    #[test]
    fn test_ror() {
        let mut cpu = CpuBuilder::default().accumulator(0b10110010).build();
        cpu.ror(&Address::Accumulator);
        assert_eq!(cpu.accumulator, 0b01011001);
    }

    #[test]
    fn test_ror_carry() {
        let mut cpu = CpuBuilder::default().accumulator(0b10110010).carry(true).build();
        cpu.ror(&Address::Accumulator);
        assert_eq!(cpu.accumulator, 0b11011001);
    }
}
