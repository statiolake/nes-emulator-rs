use itertools::Itertools;

use crate::{
    hardware::{Hardware, cpu::Status, rom::Rom},
    rt::Runtime,
};

#[test]
fn nestest_ok() {
    let rom = Rom::parse(include_bytes!("../../rom/nestest.nes")).expect("should parse test rom");
    let hw = Hardware::assemble();
    hw.insert(rom);

    // Some initial manipulation here...
    {
        // It is $C004 that is written in $FFFC, but we need to start from $C000 directly if we are not
        // supporting PPU.
        hw.cpu.state.pc.set(0xC000);

        // I don't know why
        let status = hw.cpu.state.status.get();
        hw.cpu.state.status.set(status | Status::INTERRUPT_DISABLE);

        // I don't know why
        hw.cpu.state.sp.set(0xfd);
    }

    let mut rt = Runtime::new();
    rt.run(hw.to_schedule());

    let expected_log = include_str!("../../rom/nestest_no_cycle.log")
        .lines()
        .collect_vec();
    let actual_log = hw.cpu_debug_rx.into_iter().collect_vec();

    for (line_no, (expected, actual)) in expected_log.iter().zip(actual_log.iter()).enumerate() {
        assert_eq!(expected, actual, "mismatch at line {}", line_no + 1);
    }
}
