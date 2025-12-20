use itertools::{Itertools, izip};

use crate::hardware::{Hardware, cpu::Status, rom::Rom};

#[test]
fn nestest_ok() {
    let rom = Rom::parse(include_bytes!("../../rom/nestest.nes")).expect("should parse test rom");
    let hw = Hardware::assemble(rom);
    hw.power_on();

    // Some initial manipulation here...
    {
        let mut cpu = hw.cpu.lock().unwrap();
        // It is $C004 that is written in $FFFC, but we need to start from $C000 directly if we are not
        // supporting PPU.
        cpu.pc = 0xC000;

        // I don't know why
        cpu.status.insert(Status::INTERRUPT_DISABLE);

        // I don't know why
        cpu.sp = 0xfd;
    }

    let correct_log = include_str!("../../rom/nestest_no_cycle.log")
        .lines()
        .collect_vec();

    let mut actual_log = vec![];
    while !hw.cpu.lock().unwrap().is_halted() && actual_log.len() < correct_log.len() {
        actual_log.push(hw.cpu.lock().unwrap().dump_state());
        hw.cpu.lock().unwrap().step();
    }

    for (i, (expected, actual)) in izip!(correct_log, actual_log).enumerate() {
        assert_eq!(expected, actual, "mismatch at line {i}",);
    }
}
