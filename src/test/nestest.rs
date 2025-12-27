use itertools::Itertools;

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

    let mut line_no = 0;
    while !hw.cpu.lock().unwrap().is_halted() && line_no < correct_log.len() {
        let actual = hw.cpu.lock().unwrap().dump_state();
        let expected = correct_log[line_no];
        assert_eq!(expected, actual, "mismatch at line {}", line_no + 1);
        hw.cpu.lock().unwrap().step();
        line_no += 1;
    }
}
