use ::*;
use ::tests::*;

#[cfg(test)]
mod test_adc {
    use super::*;

    fn adc(accumulator: Word, mem: Word) -> Cpu<LinearMemory> {
        let mut cpu: Cpu<LinearMemory> = Cpu::default();
        cpu.accumulator = accumulator;
        cpu.memory.inner[0] = mem;
        cpu.adc(Address::Absolute(0));
        cpu
    }

    #[test]
    fn test_adc() {
        let cpu = adc(2, 3);
        assert_eq!(cpu.accumulator, 5);
    }

    #[test]
    fn test_adc_carry_positive() {
        let cpu = adc(0b1111111, 0b00000001);
        assert_eq!(cpu.accumulator, 0b10000000);
        assert_eq!(cpu.flags.carry, true);
    }

    #[test]
    fn test_adc_carry_negative() {
        let cpu = adc(0b10000000, 0b11111111);
        assert_eq!(cpu.accumulator, 0b01111111);
        assert_eq!(cpu.flags.carry, true);
    }

    #[test]
    fn test_adc_overflow_positive() {
        let cpu = adc(0b1111111, 0b00000001);
        assert_eq!(cpu.accumulator, 0b10000000);
        assert_eq!(cpu.flags.carry, true);
    }
}
