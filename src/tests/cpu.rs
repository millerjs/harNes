use ::*;
use ::tests::*;


#[test]
fn test_adc() {
    let mut cpu: Cpu<LinearMemory> = Cpu::default();
    cpu.memory.inner[0] = 2;
    cpu.accumulator = 3;
    cpu.adc(Address::Absolute(0));
    assert_eq!(cpu.accumulator, 5);
}

#[test]
fn test_adc_overflow() {
    let mut cpu: Cpu<LinearMemory> = Cpu::default();
    cpu.memory.inner[0] = 1;
    cpu.accumulator = 255;
    cpu.adc(Address::Absolute(0));
    assert_eq!(cpu.accumulator, 0);
    assert_eq!(cpu.flags.carry, true);
}
