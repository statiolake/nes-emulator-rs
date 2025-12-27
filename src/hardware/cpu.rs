use std::fmt;

use itertools::Itertools as _;
use log::warn;

use crate::hardware::bus::Bus;

bitflags::bitflags! {
    #[derive(Debug, Clone, Copy, Eq, PartialEq)]
    pub struct Status: u8 {
        const CARRY = 0b0000_0001;
        const ZERO = 0b0000_0010;
        const INTERRUPT_DISABLE = 0b0000_0100;
         // Decimal mode is actually not supported on NES but you can freely set and remove the
         // flag by instructions.
        const DECIMAL_MODE = 0b0000_1000;
        // B Flag is set when status is pushed by PHP or BRK instructions and not set when pushed
        // by interrupts.
        const B_FLAG = 0b0001_0000;
        const RESERVED = 0b0010_0000;
        const OVERFLOW = 0b0100_0000;
        const NEGATIVE = 0b1000_0000;
    }
}

impl fmt::Display for Status {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut maybe_set = |flag: Status, ch: char| -> fmt::Result {
            if self.contains(flag) {
                write!(f, "{}", ch)?;
            } else {
                write!(f, "-")?;
            }

            Ok(())
        };

        maybe_set(Status::CARRY, 'C')?;
        maybe_set(Status::ZERO, 'Z')?;
        maybe_set(Status::INTERRUPT_DISABLE, 'I')?;
        // maybe_set(Status::DECIMAL_MODE, 'D')?;
        maybe_set(Status::B_FLAG, 'B')?;
        maybe_set(Status::OVERFLOW, 'V')?;
        maybe_set(Status::NEGATIVE, 'N')?;

        Ok(())
    }
}

const SIGN_BIT: u8 = 0b1000_0000;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum AddressingMode {
    /// #$01
    Immediate,

    /// $01
    ZeroPage,

    /// $01,X
    ZeroPageX,

    /// $01,Y
    ZeroPageY,

    /// $0102
    Absolute,

    /// $0102,X
    AbsoluteX,

    /// $0102,Y
    AbsoluteY,

    /// branch addresses, etc
    Relative,

    /// ($0102)
    Indirect,

    /// ($01, X)
    IndexedIndirect,

    /// ($01), Y
    IndirectIndexed,

    /// register A
    Accumulator,

    /// instructions that do not use addressing modes
    Implied,
}

impl AddressingMode {
    #[allow(clippy::len_without_is_empty)]
    pub fn len(self) -> usize {
        use AddressingMode::*;
        match self {
            Immediate | ZeroPage | ZeroPageX | ZeroPageY | Relative | IndexedIndirect
            | IndirectIndexed => 1,
            Absolute | AbsoluteX | AbsoluteY | Indirect => 2,
            Accumulator | Implied => 0,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Address {
    Mem(u16),
    Accum,
}

impl Address {
    fn read_from(self, cpu: &mut Cpu) -> u8 {
        match self {
            Address::Mem(addr) => cpu.bus.read(addr),
            Address::Accum => cpu.reg_a,
        }
    }

    fn write_to(self, cpu: &mut Cpu, value: u8) {
        match self {
            Address::Mem(addr) => cpu.bus.write(addr, value),
            Address::Accum => cpu.reg_a = value,
        }
    }

    fn expect_mem(self) -> u16 {
        match self {
            Address::Mem(addr) => addr,
            Address::Accum => panic!("expect_mem() called on Accum"),
        }
    }
}

#[derive(Debug)]
pub struct Opcode {
    pub code: u8,
    pub name: &'static str,
    pub is_official: bool,
    pub mode: AddressingMode,
    pub cycles: u8,
    pub handler: fn(&mut Cpu, &'static Opcode),
}

impl Opcode {
    pub const fn new(
        code: u8,
        name: &'static str,
        mode: AddressingMode,
        cycles: u8,
        handler: fn(&mut Cpu, &'static Opcode),
    ) -> Self {
        Opcode {
            code,
            name,
            is_official: true,
            mode,
            cycles,
            handler,
        }
    }

    pub const fn new_unofficial(
        code: u8,
        name: &'static str,
        mode: AddressingMode,
        cycles: u8,
        handler: fn(&mut Cpu, &'static Opcode),
    ) -> Self {
        Opcode {
            code,
            name,
            is_official: false,
            mode,
            cycles,
            handler,
        }
    }

    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        self.mode.len() + 1
    }
}

pub const CPU_OPCODES: &[Opcode] = &[
    Opcode::new(0x61, "ADC", AddressingMode::IndexedIndirect, 6, Cpu::adc),
    Opcode::new(0x65, "ADC", AddressingMode::ZeroPage, 3, Cpu::adc),
    Opcode::new(0x69, "ADC", AddressingMode::Immediate, 2, Cpu::adc),
    Opcode::new(0x6D, "ADC", AddressingMode::Absolute, 4, Cpu::adc),
    Opcode::new(0x71, "ADC", AddressingMode::IndirectIndexed, 5, Cpu::adc),
    Opcode::new(0x75, "ADC", AddressingMode::ZeroPageX, 4, Cpu::adc),
    Opcode::new(0x79, "ADC", AddressingMode::AbsoluteY, 4, Cpu::adc),
    Opcode::new(0x7D, "ADC", AddressingMode::AbsoluteX, 4, Cpu::adc),
    Opcode::new(0x21, "AND", AddressingMode::IndexedIndirect, 6, Cpu::and),
    Opcode::new(0x25, "AND", AddressingMode::ZeroPage, 3, Cpu::and),
    Opcode::new(0x29, "AND", AddressingMode::Immediate, 2, Cpu::and),
    Opcode::new(0x2D, "AND", AddressingMode::Absolute, 4, Cpu::and),
    Opcode::new(0x31, "AND", AddressingMode::IndirectIndexed, 5, Cpu::and),
    Opcode::new(0x35, "AND", AddressingMode::ZeroPageX, 4, Cpu::and),
    Opcode::new(0x39, "AND", AddressingMode::AbsoluteY, 4, Cpu::and),
    Opcode::new(0x3D, "AND", AddressingMode::AbsoluteX, 4, Cpu::and),
    Opcode::new(0x06, "ASL", AddressingMode::ZeroPage, 5, Cpu::asl),
    Opcode::new(0x0A, "ASL", AddressingMode::Accumulator, 2, Cpu::asl),
    Opcode::new(0x0E, "ASL", AddressingMode::Absolute, 6, Cpu::asl),
    Opcode::new(0x16, "ASL", AddressingMode::ZeroPageX, 6, Cpu::asl),
    Opcode::new(0x1E, "ASL", AddressingMode::AbsoluteX, 7, Cpu::asl),
    Opcode::new(0x90, "BCC", AddressingMode::Relative, 2, Cpu::bcc),
    Opcode::new(0xB0, "BCS", AddressingMode::Relative, 2, Cpu::bcs),
    Opcode::new(0xF0, "BEQ", AddressingMode::Relative, 2, Cpu::beq),
    Opcode::new(0x24, "BIT", AddressingMode::ZeroPage, 3, Cpu::bit),
    Opcode::new(0x2C, "BIT", AddressingMode::Absolute, 4, Cpu::bit),
    Opcode::new(0x30, "BMI", AddressingMode::Relative, 2, Cpu::bmi),
    Opcode::new(0xD0, "BNE", AddressingMode::Relative, 2, Cpu::bne),
    Opcode::new(0x10, "BPL", AddressingMode::Relative, 2, Cpu::bpl),
    Opcode::new(0x00, "BRK", AddressingMode::Implied, 7, Cpu::brk),
    Opcode::new(0x50, "BVC", AddressingMode::Relative, 2, Cpu::bvc),
    Opcode::new(0x70, "BVS", AddressingMode::Relative, 2, Cpu::bvs),
    Opcode::new(0x18, "CLC", AddressingMode::Implied, 2, Cpu::clc),
    Opcode::new(0xD8, "CLD", AddressingMode::Implied, 2, Cpu::cld),
    Opcode::new(0x58, "CLI", AddressingMode::Implied, 2, Cpu::cli),
    Opcode::new(0xB8, "CLV", AddressingMode::Implied, 2, Cpu::clv),
    Opcode::new(0xC1, "CMP", AddressingMode::IndexedIndirect, 6, Cpu::cmp),
    Opcode::new(0xC5, "CMP", AddressingMode::ZeroPage, 3, Cpu::cmp),
    Opcode::new(0xC9, "CMP", AddressingMode::Immediate, 2, Cpu::cmp),
    Opcode::new(0xCD, "CMP", AddressingMode::Absolute, 4, Cpu::cmp),
    Opcode::new(0xD1, "CMP", AddressingMode::IndirectIndexed, 5, Cpu::cmp),
    Opcode::new(0xD5, "CMP", AddressingMode::ZeroPageX, 4, Cpu::cmp),
    Opcode::new(0xD9, "CMP", AddressingMode::AbsoluteY, 4, Cpu::cmp),
    Opcode::new(0xDD, "CMP", AddressingMode::AbsoluteX, 4, Cpu::cmp),
    Opcode::new(0xE0, "CPX", AddressingMode::Immediate, 2, Cpu::cpx),
    Opcode::new(0xE4, "CPX", AddressingMode::ZeroPage, 3, Cpu::cpx),
    Opcode::new(0xEC, "CPX", AddressingMode::Absolute, 4, Cpu::cpx),
    Opcode::new(0xC0, "CPY", AddressingMode::Immediate, 2, Cpu::cpy),
    Opcode::new(0xC4, "CPY", AddressingMode::ZeroPage, 3, Cpu::cpy),
    Opcode::new(0xCC, "CPY", AddressingMode::Absolute, 4, Cpu::cpy),
    Opcode::new(0xC6, "DEC", AddressingMode::ZeroPage, 5, Cpu::dec),
    Opcode::new(0xCE, "DEC", AddressingMode::Absolute, 6, Cpu::dec),
    Opcode::new(0xD6, "DEC", AddressingMode::ZeroPageX, 6, Cpu::dec),
    Opcode::new(0xDE, "DEC", AddressingMode::AbsoluteX, 7, Cpu::dec),
    Opcode::new(0xCA, "DEX", AddressingMode::Implied, 2, Cpu::dex),
    Opcode::new(0x88, "DEY", AddressingMode::Implied, 2, Cpu::dey),
    Opcode::new(0x41, "EOR", AddressingMode::IndexedIndirect, 6, Cpu::eor),
    Opcode::new(0x45, "EOR", AddressingMode::ZeroPage, 3, Cpu::eor),
    Opcode::new(0x49, "EOR", AddressingMode::Immediate, 2, Cpu::eor),
    Opcode::new(0x4D, "EOR", AddressingMode::Absolute, 4, Cpu::eor),
    Opcode::new(0x51, "EOR", AddressingMode::IndirectIndexed, 5, Cpu::eor),
    Opcode::new(0x55, "EOR", AddressingMode::ZeroPageX, 4, Cpu::eor),
    Opcode::new(0x59, "EOR", AddressingMode::AbsoluteY, 4, Cpu::eor),
    Opcode::new(0x5D, "EOR", AddressingMode::AbsoluteX, 4, Cpu::eor),
    Opcode::new(0xE6, "INC", AddressingMode::ZeroPage, 5, Cpu::inc),
    Opcode::new(0xEE, "INC", AddressingMode::Absolute, 6, Cpu::inc),
    Opcode::new(0xF6, "INC", AddressingMode::ZeroPageX, 6, Cpu::inc),
    Opcode::new(0xFE, "INC", AddressingMode::AbsoluteX, 7, Cpu::inc),
    Opcode::new(0xE8, "INX", AddressingMode::Implied, 2, Cpu::inx),
    Opcode::new(0xC8, "INY", AddressingMode::Implied, 2, Cpu::iny),
    Opcode::new(0x4C, "JMP", AddressingMode::Absolute, 3, Cpu::jmp),
    Opcode::new(0x6C, "JMP", AddressingMode::Indirect, 5, Cpu::jmp),
    Opcode::new(0x20, "JSR", AddressingMode::Absolute, 6, Cpu::jsr),
    Opcode::new(0xA1, "LDA", AddressingMode::IndexedIndirect, 6, Cpu::lda),
    Opcode::new(0xA5, "LDA", AddressingMode::ZeroPage, 3, Cpu::lda),
    Opcode::new(0xA9, "LDA", AddressingMode::Immediate, 2, Cpu::lda),
    Opcode::new(0xAD, "LDA", AddressingMode::Absolute, 4, Cpu::lda),
    Opcode::new(0xB1, "LDA", AddressingMode::IndirectIndexed, 5, Cpu::lda),
    Opcode::new(0xB5, "LDA", AddressingMode::ZeroPageX, 4, Cpu::lda),
    Opcode::new(0xB9, "LDA", AddressingMode::AbsoluteY, 4, Cpu::lda),
    Opcode::new(0xBD, "LDA", AddressingMode::AbsoluteX, 4, Cpu::lda),
    Opcode::new(0xA2, "LDX", AddressingMode::Immediate, 2, Cpu::ldx),
    Opcode::new(0xA6, "LDX", AddressingMode::ZeroPage, 3, Cpu::ldx),
    Opcode::new(0xAE, "LDX", AddressingMode::Absolute, 4, Cpu::ldx),
    Opcode::new(0xB6, "LDX", AddressingMode::ZeroPageY, 4, Cpu::ldx),
    Opcode::new(0xBE, "LDX", AddressingMode::AbsoluteY, 4, Cpu::ldx),
    Opcode::new(0xA0, "LDY", AddressingMode::Immediate, 2, Cpu::ldy),
    Opcode::new(0xA4, "LDY", AddressingMode::ZeroPage, 3, Cpu::ldy),
    Opcode::new(0xAC, "LDY", AddressingMode::Absolute, 4, Cpu::ldy),
    Opcode::new(0xB4, "LDY", AddressingMode::ZeroPageX, 4, Cpu::ldy),
    Opcode::new(0xBC, "LDY", AddressingMode::AbsoluteX, 4, Cpu::ldy),
    Opcode::new(0x46, "LSR", AddressingMode::ZeroPage, 5, Cpu::lsr),
    Opcode::new(0x4A, "LSR", AddressingMode::Accumulator, 2, Cpu::lsr),
    Opcode::new(0x4E, "LSR", AddressingMode::Absolute, 6, Cpu::lsr),
    Opcode::new(0x56, "LSR", AddressingMode::ZeroPageX, 6, Cpu::lsr),
    Opcode::new(0x5E, "LSR", AddressingMode::AbsoluteX, 7, Cpu::lsr),
    Opcode::new(0xEA, "NOP", AddressingMode::Implied, 2, Cpu::nop),
    Opcode::new(0x01, "ORA", AddressingMode::IndexedIndirect, 6, Cpu::ora),
    Opcode::new(0x05, "ORA", AddressingMode::ZeroPage, 3, Cpu::ora),
    Opcode::new(0x09, "ORA", AddressingMode::Immediate, 2, Cpu::ora),
    Opcode::new(0x0D, "ORA", AddressingMode::Absolute, 4, Cpu::ora),
    Opcode::new(0x11, "ORA", AddressingMode::IndirectIndexed, 5, Cpu::ora),
    Opcode::new(0x15, "ORA", AddressingMode::ZeroPageX, 4, Cpu::ora),
    Opcode::new(0x19, "ORA", AddressingMode::AbsoluteY, 4, Cpu::ora),
    Opcode::new(0x1D, "ORA", AddressingMode::AbsoluteX, 4, Cpu::ora),
    Opcode::new(0x48, "PHA", AddressingMode::Implied, 3, Cpu::pha),
    Opcode::new(0x08, "PHP", AddressingMode::Implied, 3, Cpu::php),
    Opcode::new(0x68, "PLA", AddressingMode::Implied, 4, Cpu::pla),
    Opcode::new(0x28, "PLP", AddressingMode::Implied, 4, Cpu::plp),
    Opcode::new(0x26, "ROL", AddressingMode::ZeroPage, 5, Cpu::rol),
    Opcode::new(0x2A, "ROL", AddressingMode::Accumulator, 2, Cpu::rol),
    Opcode::new(0x2E, "ROL", AddressingMode::Absolute, 6, Cpu::rol),
    Opcode::new(0x36, "ROL", AddressingMode::ZeroPageX, 6, Cpu::rol),
    Opcode::new(0x3E, "ROL", AddressingMode::AbsoluteX, 7, Cpu::rol),
    Opcode::new(0x66, "ROR", AddressingMode::ZeroPage, 5, Cpu::ror),
    Opcode::new(0x6A, "ROR", AddressingMode::Accumulator, 2, Cpu::ror),
    Opcode::new(0x6E, "ROR", AddressingMode::Absolute, 6, Cpu::ror),
    Opcode::new(0x76, "ROR", AddressingMode::ZeroPageX, 6, Cpu::ror),
    Opcode::new(0x7E, "ROR", AddressingMode::AbsoluteX, 7, Cpu::ror),
    Opcode::new(0x40, "RTI", AddressingMode::Implied, 6, Cpu::rti),
    Opcode::new(0x60, "RTS", AddressingMode::Implied, 6, Cpu::rts),
    Opcode::new(0xE1, "SBC", AddressingMode::IndexedIndirect, 6, Cpu::sbc),
    Opcode::new(0xE5, "SBC", AddressingMode::ZeroPage, 3, Cpu::sbc),
    Opcode::new(0xE9, "SBC", AddressingMode::Immediate, 2, Cpu::sbc),
    Opcode::new(0xED, "SBC", AddressingMode::Absolute, 4, Cpu::sbc),
    Opcode::new(0xF1, "SBC", AddressingMode::IndirectIndexed, 5, Cpu::sbc),
    Opcode::new(0xF5, "SBC", AddressingMode::ZeroPageX, 4, Cpu::sbc),
    Opcode::new(0xF9, "SBC", AddressingMode::AbsoluteY, 4, Cpu::sbc),
    Opcode::new(0xFD, "SBC", AddressingMode::AbsoluteX, 4, Cpu::sbc),
    Opcode::new(0x38, "SEC", AddressingMode::Implied, 2, Cpu::sec),
    Opcode::new(0xF8, "SED", AddressingMode::Implied, 2, Cpu::sed),
    Opcode::new(0x78, "SEI", AddressingMode::Implied, 2, Cpu::sei),
    Opcode::new(0x81, "STA", AddressingMode::IndexedIndirect, 6, Cpu::sta),
    Opcode::new(0x85, "STA", AddressingMode::ZeroPage, 3, Cpu::sta),
    Opcode::new(0x8D, "STA", AddressingMode::Absolute, 4, Cpu::sta),
    Opcode::new(0x91, "STA", AddressingMode::IndirectIndexed, 6, Cpu::sta),
    Opcode::new(0x95, "STA", AddressingMode::ZeroPageX, 4, Cpu::sta),
    Opcode::new(0x99, "STA", AddressingMode::AbsoluteY, 5, Cpu::sta),
    Opcode::new(0x9D, "STA", AddressingMode::AbsoluteX, 5, Cpu::sta),
    Opcode::new(0x86, "STX", AddressingMode::ZeroPage, 3, Cpu::stx),
    Opcode::new(0x8E, "STX", AddressingMode::Absolute, 4, Cpu::stx),
    Opcode::new(0x96, "STX", AddressingMode::ZeroPageY, 4, Cpu::stx),
    Opcode::new(0x84, "STY", AddressingMode::ZeroPage, 3, Cpu::sty),
    Opcode::new(0x8C, "STY", AddressingMode::Absolute, 4, Cpu::sty),
    Opcode::new(0x94, "STY", AddressingMode::ZeroPageX, 4, Cpu::sty),
    Opcode::new(0xAA, "TAX", AddressingMode::Implied, 2, Cpu::tax),
    Opcode::new(0xA8, "TAY", AddressingMode::Implied, 2, Cpu::tay),
    Opcode::new(0xBA, "TSX", AddressingMode::Implied, 2, Cpu::tsx),
    Opcode::new(0x8A, "TXA", AddressingMode::Implied, 2, Cpu::txa),
    Opcode::new(0x9A, "TXS", AddressingMode::Implied, 2, Cpu::txs),
    Opcode::new(0x98, "TYA", AddressingMode::Implied, 2, Cpu::tya),
    // Invalid opcode for testing
    Opcode::new(0xFF, "IVD", AddressingMode::Implied, 2, Cpu::ivd),
    // Unofficial opcodes
    Opcode::new_unofficial(0x02, "KIL", AddressingMode::Implied, 0, Cpu::kil),
    Opcode::new_unofficial(0x03, "SLO", AddressingMode::IndexedIndirect, 8, Cpu::slo),
    Opcode::new_unofficial(0x04, "NOP", AddressingMode::ZeroPage, 3, Cpu::dop),
    Opcode::new_unofficial(0x07, "SLO", AddressingMode::ZeroPage, 5, Cpu::slo),
    Opcode::new_unofficial(0x0B, "AAC", AddressingMode::Immediate, 2, Cpu::aac),
    Opcode::new_unofficial(0x0C, "NOP", AddressingMode::Absolute, 4, Cpu::top),
    Opcode::new_unofficial(0x0F, "SLO", AddressingMode::Absolute, 6, Cpu::slo),
    Opcode::new_unofficial(0x12, "KIL", AddressingMode::Implied, 0, Cpu::kil),
    Opcode::new_unofficial(0x13, "SLO", AddressingMode::IndirectIndexed, 8, Cpu::slo),
    Opcode::new_unofficial(0x14, "NOP", AddressingMode::ZeroPageX, 4, Cpu::dop),
    Opcode::new_unofficial(0x17, "SLO", AddressingMode::ZeroPageX, 6, Cpu::slo),
    Opcode::new_unofficial(0x1A, "NOP", AddressingMode::Implied, 2, Cpu::nop),
    Opcode::new_unofficial(0x1B, "SLO", AddressingMode::AbsoluteY, 7, Cpu::slo),
    Opcode::new_unofficial(0x1C, "NOP", AddressingMode::AbsoluteX, 4, Cpu::top),
    Opcode::new_unofficial(0x1F, "SLO", AddressingMode::AbsoluteX, 7, Cpu::slo),
    Opcode::new_unofficial(0x22, "KIL", AddressingMode::Implied, 0, Cpu::kil),
    Opcode::new_unofficial(0x23, "RLA", AddressingMode::IndexedIndirect, 8, Cpu::rla),
    Opcode::new_unofficial(0x27, "RLA", AddressingMode::ZeroPage, 5, Cpu::rla),
    Opcode::new_unofficial(0x2B, "AAC", AddressingMode::Immediate, 2, Cpu::aac),
    Opcode::new_unofficial(0x2F, "RLA", AddressingMode::Absolute, 6, Cpu::rla),
    Opcode::new_unofficial(0x32, "KIL", AddressingMode::Implied, 0, Cpu::kil),
    Opcode::new_unofficial(0x33, "RLA", AddressingMode::IndirectIndexed, 8, Cpu::rla),
    Opcode::new_unofficial(0x34, "NOP", AddressingMode::ZeroPageX, 4, Cpu::dop),
    Opcode::new_unofficial(0x37, "RLA", AddressingMode::ZeroPageX, 6, Cpu::rla),
    Opcode::new_unofficial(0x3A, "NOP", AddressingMode::Implied, 2, Cpu::nop),
    Opcode::new_unofficial(0x3B, "RLA", AddressingMode::AbsoluteY, 7, Cpu::rla),
    Opcode::new_unofficial(0x3C, "NOP", AddressingMode::AbsoluteX, 4, Cpu::top),
    Opcode::new_unofficial(0x3F, "RLA", AddressingMode::AbsoluteX, 7, Cpu::rla),
    Opcode::new_unofficial(0x42, "KIL", AddressingMode::Implied, 0, Cpu::kil),
    Opcode::new_unofficial(0x43, "SRE", AddressingMode::IndexedIndirect, 8, Cpu::sre),
    Opcode::new_unofficial(0x44, "NOP", AddressingMode::ZeroPage, 3, Cpu::dop),
    Opcode::new_unofficial(0x47, "SRE", AddressingMode::ZeroPage, 5, Cpu::sre),
    Opcode::new_unofficial(0x4B, "ASR", AddressingMode::Immediate, 2, Cpu::asr),
    Opcode::new_unofficial(0x4F, "SRE", AddressingMode::Absolute, 6, Cpu::sre),
    Opcode::new_unofficial(0x52, "KIL", AddressingMode::Implied, 0, Cpu::kil),
    Opcode::new_unofficial(0x53, "SRE", AddressingMode::IndirectIndexed, 8, Cpu::sre),
    Opcode::new_unofficial(0x54, "NOP", AddressingMode::ZeroPageX, 4, Cpu::dop),
    Opcode::new_unofficial(0x57, "SRE", AddressingMode::ZeroPageX, 6, Cpu::sre),
    Opcode::new_unofficial(0x5A, "NOP", AddressingMode::Implied, 2, Cpu::nop),
    Opcode::new_unofficial(0x5B, "SRE", AddressingMode::AbsoluteY, 7, Cpu::sre),
    Opcode::new_unofficial(0x5C, "NOP", AddressingMode::AbsoluteX, 4, Cpu::top),
    Opcode::new_unofficial(0x5F, "SRE", AddressingMode::AbsoluteX, 7, Cpu::sre),
    Opcode::new_unofficial(0x62, "KIL", AddressingMode::Implied, 0, Cpu::kil),
    Opcode::new_unofficial(0x63, "RRA", AddressingMode::IndexedIndirect, 8, Cpu::rra),
    Opcode::new_unofficial(0x64, "NOP", AddressingMode::ZeroPage, 3, Cpu::dop),
    Opcode::new_unofficial(0x67, "RRA", AddressingMode::ZeroPage, 5, Cpu::rra),
    Opcode::new_unofficial(0x6B, "ARR", AddressingMode::Immediate, 2, Cpu::arr),
    Opcode::new_unofficial(0x6F, "RRA", AddressingMode::Absolute, 6, Cpu::rra),
    Opcode::new_unofficial(0x72, "KIL", AddressingMode::Implied, 0, Cpu::kil),
    Opcode::new_unofficial(0x73, "RRA", AddressingMode::IndirectIndexed, 8, Cpu::rra),
    Opcode::new_unofficial(0x74, "NOP", AddressingMode::ZeroPageX, 4, Cpu::dop),
    Opcode::new_unofficial(0x77, "RRA", AddressingMode::ZeroPageX, 6, Cpu::rra),
    Opcode::new_unofficial(0x7A, "NOP", AddressingMode::Implied, 2, Cpu::nop),
    Opcode::new_unofficial(0x7B, "RRA", AddressingMode::AbsoluteY, 7, Cpu::rra),
    Opcode::new_unofficial(0x7C, "NOP", AddressingMode::AbsoluteX, 4, Cpu::top),
    Opcode::new_unofficial(0x7F, "RRA", AddressingMode::AbsoluteX, 7, Cpu::rra),
    Opcode::new_unofficial(0x80, "NOP", AddressingMode::Immediate, 2, Cpu::dop),
    Opcode::new_unofficial(0x82, "NOP", AddressingMode::Immediate, 2, Cpu::dop),
    Opcode::new_unofficial(0x83, "SAX", AddressingMode::IndexedIndirect, 6, Cpu::sax),
    Opcode::new_unofficial(0x87, "SAX", AddressingMode::ZeroPage, 3, Cpu::sax),
    Opcode::new_unofficial(0x89, "NOP", AddressingMode::Immediate, 2, Cpu::dop),
    Opcode::new_unofficial(0x8B, "XAA", AddressingMode::Immediate, 2, Cpu::xaa),
    Opcode::new_unofficial(0x8F, "SAX", AddressingMode::Absolute, 4, Cpu::sax),
    Opcode::new_unofficial(0x92, "KIL", AddressingMode::Implied, 0, Cpu::kil),
    Opcode::new_unofficial(0x93, "AXA", AddressingMode::IndirectIndexed, 6, Cpu::axa),
    Opcode::new_unofficial(0x97, "SAX", AddressingMode::ZeroPageY, 4, Cpu::sax),
    Opcode::new_unofficial(0x9B, "XAS", AddressingMode::AbsoluteY, 5, Cpu::xas),
    Opcode::new_unofficial(0x9C, "SYA", AddressingMode::AbsoluteX, 5, Cpu::sya),
    Opcode::new_unofficial(0x9E, "SXA", AddressingMode::AbsoluteY, 5, Cpu::sxa),
    Opcode::new_unofficial(0x9F, "AXA", AddressingMode::AbsoluteY, 5, Cpu::axa),
    Opcode::new_unofficial(0xA3, "LAX", AddressingMode::IndexedIndirect, 6, Cpu::lax),
    Opcode::new_unofficial(0xA7, "LAX", AddressingMode::ZeroPage, 3, Cpu::lax),
    Opcode::new_unofficial(0xAB, "ATX", AddressingMode::Immediate, 2, Cpu::atx),
    Opcode::new_unofficial(0xAF, "LAX", AddressingMode::Absolute, 4, Cpu::lax),
    Opcode::new_unofficial(0xB2, "KIL", AddressingMode::Implied, 0, Cpu::kil),
    Opcode::new_unofficial(0xB3, "LAX", AddressingMode::IndirectIndexed, 5, Cpu::lax),
    Opcode::new_unofficial(0xB7, "LAX", AddressingMode::ZeroPageY, 4, Cpu::lax),
    Opcode::new_unofficial(0xBB, "LAR", AddressingMode::AbsoluteY, 4, Cpu::lar),
    Opcode::new_unofficial(0xBF, "LAX", AddressingMode::AbsoluteY, 4, Cpu::lax),
    Opcode::new_unofficial(0xC2, "NOP", AddressingMode::Immediate, 2, Cpu::dop),
    Opcode::new_unofficial(0xC3, "DCP", AddressingMode::IndexedIndirect, 8, Cpu::dcp),
    Opcode::new_unofficial(0xC7, "DCP", AddressingMode::ZeroPage, 5, Cpu::dcp),
    Opcode::new_unofficial(0xCB, "AXS", AddressingMode::Immediate, 2, Cpu::axs),
    Opcode::new_unofficial(0xCF, "DCP", AddressingMode::Absolute, 6, Cpu::dcp),
    Opcode::new_unofficial(0xD2, "KIL", AddressingMode::Implied, 0, Cpu::kil),
    Opcode::new_unofficial(0xD3, "DCP", AddressingMode::IndirectIndexed, 8, Cpu::dcp),
    Opcode::new_unofficial(0xD4, "NOP", AddressingMode::ZeroPageX, 4, Cpu::dop),
    Opcode::new_unofficial(0xD7, "DCP", AddressingMode::ZeroPageX, 6, Cpu::dcp),
    Opcode::new_unofficial(0xDA, "NOP", AddressingMode::Implied, 2, Cpu::nop),
    Opcode::new_unofficial(0xDB, "DCP", AddressingMode::AbsoluteY, 7, Cpu::dcp),
    Opcode::new_unofficial(0xDC, "NOP", AddressingMode::AbsoluteX, 4, Cpu::top),
    Opcode::new_unofficial(0xDF, "DCP", AddressingMode::AbsoluteX, 7, Cpu::dcp),
    Opcode::new_unofficial(0xE2, "NOP", AddressingMode::Immediate, 2, Cpu::dop),
    Opcode::new_unofficial(0xE3, "ISB", AddressingMode::IndexedIndirect, 8, Cpu::isb),
    Opcode::new_unofficial(0xE7, "ISB", AddressingMode::ZeroPage, 5, Cpu::isb),
    Opcode::new_unofficial(0xEB, "SBC", AddressingMode::Immediate, 2, Cpu::sbc),
    Opcode::new_unofficial(0xEF, "ISB", AddressingMode::Absolute, 6, Cpu::isb),
    Opcode::new_unofficial(0xF2, "KIL", AddressingMode::Implied, 0, Cpu::kil),
    Opcode::new_unofficial(0xF3, "ISB", AddressingMode::IndirectIndexed, 8, Cpu::isb),
    Opcode::new_unofficial(0xF4, "NOP", AddressingMode::ZeroPageX, 4, Cpu::dop),
    Opcode::new_unofficial(0xF7, "ISB", AddressingMode::ZeroPageX, 6, Cpu::isb),
    Opcode::new_unofficial(0xFA, "NOP", AddressingMode::Implied, 2, Cpu::nop),
    Opcode::new_unofficial(0xFB, "ISB", AddressingMode::AbsoluteY, 7, Cpu::isb),
    Opcode::new_unofficial(0xFC, "NOP", AddressingMode::AbsoluteX, 4, Cpu::top),
    Opcode::new_unofficial(0xFF, "ISB", AddressingMode::AbsoluteX, 7, Cpu::isb),
];

pub struct Disassembled {
    /// Whether the instruction is an official opcode
    pub is_official: bool,

    /// An assembly-like notation of the instruction, e.g. "ORA ($33),Y"
    pub repr: String,

    /// A hint to visualize the indirect addressing resolution
    pub addr_value_hint: Option<String>,
}

pub fn disassemble(cpu: &mut Cpu, instr: &[u8]) -> Disassembled {
    return inner(cpu, instr).unwrap_or_else(|| Disassembled {
        is_official: true,
        repr: "???".to_string(),
        addr_value_hint: None,
    });

    fn inner(cpu: &mut Cpu, instr: &[u8]) -> Option<Disassembled> {
        use AddressingMode::*;

        let op = cpu.op_table[instr[0] as usize]?;
        let op_name = op.name;
        let is_official = op.is_official;

        let first = instr.get(1).copied();
        let second = instr.get(2).copied();

        Some(match op.mode {
            Immediate => {
                let first = first?;
                Disassembled {
                    is_official,
                    repr: format!("{op_name} #${first:02X}"),
                    addr_value_hint: None,
                }
            }
            ZeroPage => {
                let first = first?;
                let value = cpu.bus.read(u16::from(first));
                Disassembled {
                    is_official,
                    repr: format!("{op_name} ${first:02X}"),
                    addr_value_hint: Some(format!("= {:02X}", value)),
                }
            }
            ZeroPageX => {
                let first = first?;
                let addr = first.wrapping_add(cpu.reg_x);
                let value = cpu.bus.read(u16::from(addr));
                Disassembled {
                    is_official,
                    repr: format!("{op_name} ${first:02X},X"),
                    addr_value_hint: Some(format!("@ {addr:02X} = {value:02X}",)),
                }
            }
            ZeroPageY => {
                let first = first?;
                let addr = first.wrapping_add(cpu.reg_y);
                let value = cpu.bus.read(u16::from(addr));
                Disassembled {
                    is_official,
                    repr: format!("{op_name} ${first:02X},Y"),
                    addr_value_hint: Some(format!("@ {addr:02X} = {value:02X}",)),
                }
            }
            Absolute => {
                let first = first?;
                let second = second?;
                let addr = u16::from_le_bytes([first, second]);
                let value = cpu.bus.read(addr);
                let addr_value_hint = match op_name {
                    "JMP" | "JSR" => None,
                    _ => Some(format!("= {value:02X}")),
                };
                Disassembled {
                    is_official,
                    repr: format!("{op_name} ${addr:04X}"),
                    addr_value_hint,
                }
            }
            AbsoluteX => {
                let first = first?;
                let second = second?;
                let base_addr = u16::from_le_bytes([first, second]);
                let addr = base_addr.wrapping_add(u16::from(cpu.reg_x));
                let value = cpu.bus.read(addr);
                Disassembled {
                    is_official,
                    repr: format!("{op_name} ${base_addr:04X},X"),
                    addr_value_hint: Some(format!("@ {addr:04X} = {value:02X}")),
                }
            }
            AbsoluteY => {
                let first = first?;
                let second = second?;
                let base_addr = u16::from_le_bytes([first, second]);
                let addr = base_addr.wrapping_add(u16::from(cpu.reg_y));
                let value = cpu.bus.read(addr);
                Disassembled {
                    is_official,
                    repr: format!("{op_name} ${base_addr:04X},Y"),
                    addr_value_hint: Some(format!("@ {addr:04X} = {value:02X}")),
                }
            }
            Relative => {
                let first = first?;
                let offset = first as i8;
                // need to advance PC by 2 (the length of this instruction)
                let addr = cpu
                    .pc
                    .wrapping_add(2)
                    .wrapping_add_signed(i16::from(offset));
                Disassembled {
                    is_official,
                    repr: format!("{op_name} ${addr:04X}"),
                    addr_value_hint: None,
                }
            }
            Indirect => {
                let first = first?;
                let second = second?;
                let ptr_addr = u16::from_le_bytes([first, second]);

                // Emulate 6502 page boundary hardware bug
                // On page boundary, the high byte does not wrap to the next page
                // So, if the addr is $01FF, the hi byte is read from $0100 instead of $0200
                let [lo_addr, hi_addr] = ptr_addr.to_le_bytes();
                let lo = cpu.bus.read(u16::from_le_bytes([lo_addr, hi_addr]));
                let hi = cpu
                    .bus
                    .read(u16::from_le_bytes([lo_addr.wrapping_add(1), hi_addr]));

                let addr = u16::from_le_bytes([lo, hi]);
                Disassembled {
                    is_official,
                    repr: format!("{op_name} (${ptr_addr:04X})"),
                    addr_value_hint: Some(format!("= {addr:04X}")),
                }
            }
            IndexedIndirect => {
                let first = first?;
                let offsetted = first.wrapping_add(cpu.reg_x);
                // IndexedIndirect always reads from zero page
                let lo = cpu.bus.read(u16::from(offsetted));
                let hi = cpu.bus.read(u16::from(offsetted.wrapping_add(1)));
                let addr = u16::from_le_bytes([lo, hi]);
                let value = cpu.bus.read(addr);
                Disassembled {
                    is_official,
                    repr: format!("{op_name} (${:02X},X)", first),
                    addr_value_hint: Some(format!("@ {offsetted:02X} = {addr:04X} = {value:02X}")),
                }
            }
            IndirectIndexed => {
                let first = first?;
                // IndirectIndexed always reads from zero page
                let lo = cpu.bus.read(u16::from(first));
                let hi = cpu.bus.read(u16::from(first.wrapping_add(1)));
                let base_addr = u16::from_le_bytes([lo, hi]);
                let addr = base_addr.wrapping_add(u16::from(cpu.reg_y));
                let value = cpu.bus.read(addr);
                Disassembled {
                    is_official,
                    repr: format!("{op_name} (${:02X}),Y", first),
                    addr_value_hint: Some(format!("= {base_addr:04X} @ {addr:04X} = {value:02X}",)),
                }
            }
            Accumulator => Disassembled {
                is_official,
                repr: format!("{op_name} A"),
                addr_value_hint: None,
            },
            Implied => Disassembled {
                is_official,
                repr: op_name.to_string(),
                addr_value_hint: None,
            },
        })
    }
}

pub struct Cpu {
    pub op_table: Vec<Option<&'static Opcode>>,

    pub halted: bool,

    pub reg_a: u8,
    pub reg_x: u8,
    pub reg_y: u8,
    pub status: Status,
    pub pc: u16,
    pub sp: u8,

    pub bus: Bus,
}

impl Cpu {
    pub fn new(bus: Bus) -> Self {
        let mut op_table = vec![None; 256];

        for op in CPU_OPCODES {
            op_table[op.code as usize] = Some(op);
        }

        Cpu {
            op_table,

            halted: false,

            reg_a: 0,
            reg_x: 0,
            reg_y: 0,
            status: Status::empty(),
            pc: 0,
            sp: 0,

            bus,
        }
    }

    pub fn reset(&mut self) {
        self.halted = false;
        self.reg_a = 0;
        self.reg_x = 0;
        self.reg_y = 0;
        self.status = Status::RESERVED;
        self.pc = self.bus.read_u16(0xfffc);
        self.sp = 0xff;
    }

    pub fn run(&mut self) {
        while !self.is_halted() {
            self.step();
        }
    }

    pub fn step(&mut self) {
        // self.debug_dump_state();
        let op_code = self.read_pc_next();
        let Some(op) = self.op_table[op_code as usize] else {
            warn!("Invalid opcode {op_code:x} at PC {:#06x}", self.pc - 1);
            self.halted = true;
            return;
        };

        (op.handler)(self, op);
    }

    pub fn is_halted(&self) -> bool {
        self.halted
    }

    pub fn dump_state(&mut self) -> String {
        let op_code = self.bus.read(self.pc);
        let maybe_op = self.op_table[op_code as usize];
        let instr_len = if let Some(op) = maybe_op { op.len() } else { 1 };
        let instr = (0..instr_len)
            .map(|i| self.bus.read(self.pc + i as u16))
            .collect::<Vec<u8>>();
        let dis = disassemble(self, &instr);

        let pc = self.pc;
        let instr = (0..3)
            .map(|i| {
                if i < instr_len {
                    format!("{:02X}", self.bus.read(pc + i as u16))
                } else {
                    "  ".to_string()
                }
            })
            .join(" ");
        let ext_mark = if dis.is_official { " " } else { "*" };
        let disassembled = format!("{} {}", dis.repr, dis.addr_value_hint.unwrap_or_default());
        let reg_a = self.reg_a;
        let reg_x = self.reg_x;
        let reg_y = self.reg_y;
        let p = self.status.bits();
        let sp = self.sp;

        format!(
            "{pc:04X}  {instr} {ext_mark}{disassembled:31} A:{reg_a:02X} X:{reg_x:02X} Y:{reg_y:02X} P:{p:02X} SP:{sp:02X}",
        )
    }

    fn pc_next(&mut self) -> u16 {
        let pc = self.pc;
        self.pc += 1;
        pc
    }

    fn read_pc_next(&mut self) -> u8 {
        let pc = self.pc_next();

        self.bus.read(pc)
    }

    fn read_pc_u16_next(&mut self) -> u16 {
        // Increment PC twice to read two bytes
        let pc = self.pc_next();
        let _ = self.pc_next();

        self.bus.read_u16(pc)
    }

    fn stack_push(&mut self, value: u8) {
        let sp_addr = u16::from_be_bytes([0x01, self.sp]);
        self.bus.write(sp_addr, value);
        self.sp = self.sp.wrapping_sub(1);
    }

    fn stack_pop(&mut self) -> u8 {
        if self.sp == 0xff {
            panic!("Stack underflow");
        }

        self.sp = self.sp.wrapping_add(1);
        let sp_addr = u16::from_be_bytes([0x01, self.sp]);
        self.bus.read(sp_addr)
    }

    fn stack_push_u16(&mut self, value: u16) {
        let [lo, hi] = value.to_le_bytes();
        self.stack_push(hi);
        self.stack_push(lo);
    }

    fn stack_pop_u16(&mut self) -> u16 {
        let lo = self.stack_pop();
        let hi = self.stack_pop();
        u16::from_le_bytes([lo, hi])
    }

    fn operand_addr_next(&mut self, mode: AddressingMode) -> Address {
        use AddressingMode::*;

        match mode {
            Immediate => Address::Mem(self.pc_next()),
            ZeroPage => Address::Mem(u16::from(self.read_pc_next())),
            ZeroPageX => {
                let addr = self.read_pc_next();
                Address::Mem(u16::from(addr.wrapping_add(self.reg_x)))
            }
            ZeroPageY => {
                let addr = self.read_pc_next();
                Address::Mem(u16::from(addr.wrapping_add(self.reg_y)))
            }
            Absolute => Address::Mem(self.read_pc_u16_next()),
            AbsoluteX => {
                let addr = self.read_pc_u16_next();
                Address::Mem(addr.wrapping_add(u16::from(self.reg_x)))
            }
            AbsoluteY => {
                let addr = self.read_pc_u16_next();
                Address::Mem(addr.wrapping_add(u16::from(self.reg_y)))
            }
            Relative => {
                // this relative offset is signed
                let offset = self.read_pc_next() as i8;
                Address::Mem(self.pc.wrapping_add_signed(i16::from(offset)))
            }
            Indirect => {
                let addr = self.read_pc_u16_next();

                // Emulate 6502 page boundary hardware bug
                // On page boundary, the high byte does not wrap to the next page
                // So, if the addr is $01FF, the hi byte is read from $0100 instead of $0200
                let [lo_addr, hi_addr] = addr.to_le_bytes();
                let lo = self.bus.read(u16::from_le_bytes([lo_addr, hi_addr]));
                let hi = self
                    .bus
                    .read(u16::from_le_bytes([lo_addr.wrapping_add(1), hi_addr]));

                Address::Mem(u16::from_le_bytes([lo, hi]))
            }
            IndexedIndirect => {
                let base = self.read_pc_next();
                let offsetted = base.wrapping_add(self.reg_x);
                // IndexedIndirect always read address from zero page
                let lo = self.bus.read(u16::from(offsetted));
                let hi = self.bus.read(u16::from(offsetted.wrapping_add(1)));
                let addr = u16::from_le_bytes([lo, hi]);
                Address::Mem(addr)
            }
            IndirectIndexed => {
                let base = self.read_pc_next();
                // IndirectIndexed always read address from zero page
                let lo = self.bus.read(u16::from(base));
                let hi = self.bus.read(u16::from(base.wrapping_add(1)));
                let addr = u16::from_le_bytes([lo, hi]);

                Address::Mem(addr.wrapping_add(u16::from(self.reg_y)))
            }
            Accumulator => Address::Accum,
            Implied => {
                panic!("Implied addressing mode does not have an operand address")
            }
        }
    }

    fn adc(&mut self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode);
        let value = addr.read_from(self);
        self.adc_impl(Address::Accum, value, true);
    }

    fn and(&mut self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode);
        let value = addr.read_from(self);
        self.and_impl(Address::Accum, value);
    }

    fn asl(&mut self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode);
        self.asl_impl(addr);
    }

    fn bcc(&mut self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode).expect_mem();
        if !self.status.contains(Status::CARRY) {
            self.pc = addr;
        }
    }

    fn bcs(&mut self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode).expect_mem();
        if self.status.contains(Status::CARRY) {
            self.pc = addr;
        }
    }

    fn beq(&mut self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode).expect_mem();
        if self.status.contains(Status::ZERO) {
            self.pc = addr;
        }
    }

    fn bit(&mut self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode);
        let value = addr.read_from(self);
        let mask = self.reg_a;
        let result = value & mask;

        self.status.set(Status::ZERO, result == 0);
        self.status.set(Status::OVERFLOW, value & 0b0100_0000 != 0);
        self.status.set(Status::NEGATIVE, value & SIGN_BIT != 0);
    }

    fn bmi(&mut self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode).expect_mem();
        if self.status.contains(Status::NEGATIVE) {
            self.pc = addr;
        }
    }

    fn bne(&mut self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode).expect_mem();
        if !self.status.contains(Status::ZERO) {
            self.pc = addr;
        }
    }

    fn bpl(&mut self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode).expect_mem();
        if !self.status.contains(Status::NEGATIVE) {
            self.pc = addr;
        }
    }

    fn brk(&mut self, _op: &'static Opcode) {
        self.halted = true;
    }

    fn bvc(&mut self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode).expect_mem();
        if !self.status.contains(Status::OVERFLOW) {
            self.pc = addr;
        }
    }

    fn bvs(&mut self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode).expect_mem();
        if self.status.contains(Status::OVERFLOW) {
            self.pc = addr;
        }
    }

    fn clc(&mut self, _op: &'static Opcode) {
        self.status.remove(Status::CARRY);
    }

    fn cld(&mut self, _op: &'static Opcode) {
        // Decimal mode is not supported but we can set the flag
        self.status.remove(Status::DECIMAL_MODE);
    }

    fn cli(&mut self, _op: &'static Opcode) {
        self.status.remove(Status::INTERRUPT_DISABLE);
    }

    fn clv(&mut self, _op: &'static Opcode) {
        self.status.remove(Status::OVERFLOW);
    }

    fn cmp(&mut self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode);
        let value = addr.read_from(self);
        self.cmp_impl(Address::Accum, value);
    }

    fn cpx(&mut self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode);
        let value = addr.read_from(self);

        self.status.set(Status::CARRY, self.reg_x >= value);
        self.status.set(Status::ZERO, self.reg_x == value);
        self.status.set(
            Status::NEGATIVE,
            (self.reg_x.wrapping_sub(value)) & SIGN_BIT != 0,
        );
    }

    fn cpy(&mut self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode);
        let value = addr.read_from(self);

        self.status.set(Status::CARRY, self.reg_y >= value);
        self.status.set(Status::ZERO, self.reg_y == value);
        self.status.set(
            Status::NEGATIVE,
            (self.reg_y.wrapping_sub(value)) & SIGN_BIT != 0,
        );
    }

    fn dec(&mut self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode);
        self.sbc_impl(addr, 1, false);
    }

    fn dex(&mut self, _op: &'static Opcode) {
        self.reg_x = self.reg_x.wrapping_sub(1);

        self.status.set(Status::ZERO, self.reg_x == 0);
        self.status
            .set(Status::NEGATIVE, self.reg_x & SIGN_BIT != 0);
    }

    fn dey(&mut self, _op: &'static Opcode) {
        self.reg_y = self.reg_y.wrapping_sub(1);

        self.status.set(Status::ZERO, self.reg_y == 0);
        self.status
            .set(Status::NEGATIVE, self.reg_y & SIGN_BIT != 0);
    }

    fn eor(&mut self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode);
        let value = addr.read_from(self);

        let result = self.reg_a ^ value;
        self.reg_a = result;

        self.status.set(Status::ZERO, result == 0);
        self.status.set(Status::NEGATIVE, result & SIGN_BIT != 0);
    }

    fn inc(&mut self, _op: &'static Opcode) {
        let addr = self.operand_addr_next(_op.mode);
        let value = addr.read_from(self);
        let result = value.wrapping_add(1);
        addr.write_to(self, result);

        self.status.set(Status::ZERO, result == 0);
        self.status.set(Status::NEGATIVE, result & SIGN_BIT != 0);
    }

    fn inx(&mut self, _op: &'static Opcode) {
        self.reg_x = self.reg_x.wrapping_add(1);

        self.status.set(Status::ZERO, self.reg_x == 0);
        self.status
            .set(Status::NEGATIVE, self.reg_x & SIGN_BIT != 0);
    }

    fn iny(&mut self, _op: &'static Opcode) {
        self.reg_y = self.reg_y.wrapping_add(1);

        self.status.set(Status::ZERO, self.reg_y == 0);
        self.status
            .set(Status::NEGATIVE, self.reg_y & SIGN_BIT != 0);
    }

    fn jmp(&mut self, _op: &'static Opcode) {
        let addr = self.operand_addr_next(_op.mode).expect_mem();
        self.pc = addr;
    }

    fn jsr(&mut self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode).expect_mem();

        self.stack_push_u16(self.pc.wrapping_sub(1));

        self.pc = addr;
    }

    fn lda(&mut self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode);
        let value = addr.read_from(self);
        self.reg_a = value;

        self.status.set(Status::ZERO, value == 0);
        self.status.set(Status::NEGATIVE, value & SIGN_BIT != 0);
    }

    fn ldx(&mut self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode);
        let value = addr.read_from(self);
        self.reg_x = value;

        self.status.set(Status::ZERO, value == 0);
        self.status.set(Status::NEGATIVE, value & SIGN_BIT != 0);
    }

    fn ldy(&mut self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode);
        let value = addr.read_from(self);
        self.reg_y = value;

        self.status.set(Status::ZERO, value == 0);
        self.status.set(Status::NEGATIVE, value & SIGN_BIT != 0);
    }

    fn lsr(&mut self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode);
        let value = addr.read_from(self);

        let carry = value & 0b0000_0001 != 0;
        let result = value >> 1;
        addr.write_to(self, result);

        self.status.set(Status::CARRY, carry);
        self.status.set(Status::ZERO, result == 0);
        self.status.set(Status::NEGATIVE, result & SIGN_BIT != 0);
    }

    fn nop(&mut self, _op: &'static Opcode) {}

    fn ora(&mut self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode);
        let value = addr.read_from(self);
        self.or_impl(Address::Accum, value);
    }

    fn pha(&mut self, _op: &'static Opcode) {
        let value = self.reg_a;
        self.stack_push(value);
    }

    fn php(&mut self, _op: &'static Opcode) {
        // Set the B flag when pushing to stack by instruction
        let value = (self.status | Status::B_FLAG).bits();
        self.stack_push(value);
    }

    fn pla(&mut self, _op: &'static Opcode) {
        let value = self.stack_pop();
        self.reg_a = value;

        self.status.set(Status::ZERO, value == 0);
        self.status.set(Status::NEGATIVE, value & SIGN_BIT != 0);
    }

    fn plp(&mut self, _op: &'static Opcode) {
        let value = self.stack_pop();
        // when pulling from stack, B flag is ignored, and RESERVED flag is always set.
        self.status = Status::from_bits_truncate(value) & !Status::B_FLAG | Status::RESERVED;
    }

    fn rol(&mut self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode);
        let value = addr.read_from(self);
        self.rol_impl(addr, value);
    }

    fn ror(&mut self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode);
        let value = addr.read_from(self);
        self.ror_impl(addr, value);
    }

    fn rti(&mut self, _op: &'static Opcode) {
        // when pulling from stack, B flag is not set and RESERVED flag is always set.
        self.status =
            Status::from_bits_truncate(self.stack_pop()) & !Status::B_FLAG | Status::RESERVED;
        self.pc = self.stack_pop_u16();
    }

    fn rts(&mut self, _op: &'static Opcode) {
        self.pc = self.stack_pop_u16().wrapping_add(1);
    }

    fn sbc(&mut self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode);
        let value = addr.read_from(self);
        self.sbc_impl(Address::Accum, value, true);
    }

    fn sec(&mut self, _op: &'static Opcode) {
        self.status.insert(Status::CARRY);
    }

    fn sed(&mut self, _op: &'static Opcode) {
        // Decimal mode is not supported but we can set the flag
        self.status.insert(Status::DECIMAL_MODE);
    }

    fn sei(&mut self, _op: &'static Opcode) {
        self.status.insert(Status::INTERRUPT_DISABLE);
    }

    fn sta(&mut self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode);
        addr.write_to(self, self.reg_a);
    }

    fn stx(&mut self, _op: &'static Opcode) {
        let addr = self.operand_addr_next(_op.mode);
        addr.write_to(self, self.reg_x);
    }

    fn sty(&mut self, _op: &'static Opcode) {
        let addr = self.operand_addr_next(_op.mode);
        addr.write_to(self, self.reg_y);
    }

    fn tax(&mut self, _op: &'static Opcode) {
        let result = self.reg_a;
        self.reg_x = result;

        self.status.set(Status::ZERO, result == 0);
        self.status.set(Status::NEGATIVE, result & SIGN_BIT != 0);
    }

    fn tay(&mut self, _op: &'static Opcode) {
        let result = self.reg_a;
        self.reg_y = result;

        self.status.set(Status::ZERO, result == 0);
        self.status.set(Status::NEGATIVE, result & SIGN_BIT != 0);
    }

    fn tsx(&mut self, _op: &'static Opcode) {
        let result = self.sp;
        self.reg_x = result;

        self.status.set(Status::ZERO, result == 0);
        self.status.set(Status::NEGATIVE, result & SIGN_BIT != 0);
    }

    fn txa(&mut self, _op: &'static Opcode) {
        let result = self.reg_x;
        self.reg_a = result;

        self.status.set(Status::ZERO, result == 0);
        self.status.set(Status::NEGATIVE, result & SIGN_BIT != 0);
    }

    fn txs(&mut self, _op: &'static Opcode) {
        self.sp = self.reg_x;
    }

    fn tya(&mut self, _op: &'static Opcode) {
        let result = self.reg_y;
        self.reg_a = result;

        self.status.set(Status::ZERO, result == 0);
        self.status.set(Status::NEGATIVE, result & SIGN_BIT != 0);
    }

    fn ivd(&mut self, _op: &'static Opcode) {
        panic!("Invalid opcode encountered");
    }

    // unofficial opcodes

    fn aac(&mut self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode);
        let value = addr.read_from(self);
        let res = self.and_impl(Address::Accum, value);

        self.status.set(Status::CARRY, res & SIGN_BIT != 0);
    }

    fn sax(&mut self, op: &'static Opcode) {
        let res = self.reg_a & self.reg_x;
        let addr = self.operand_addr_next(op.mode);
        addr.write_to(self, res);

        // FIXME: According to nestest.log, this instruction does not affect any flags (really?)
        // self.status.set(Status::ZERO, res == 0);
        // self.status.set(Status::NEGATIVE, res & SIGN_BIT != 0);
    }

    fn arr(&mut self, _op: &'static Opcode) {
        todo!()
    }

    fn asr(&mut self, _op: &'static Opcode) {
        todo!()
    }

    fn atx(&mut self, _op: &'static Opcode) {
        todo!()
    }

    fn axa(&mut self, _op: &'static Opcode) {
        todo!()
    }

    fn axs(&mut self, _op: &'static Opcode) {
        todo!()
    }

    fn dcp(&mut self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode);
        self.sbc_impl(addr, 1, false);
        let value = addr.read_from(self);
        self.cmp_impl(Address::Accum, value);
    }

    fn dop(&mut self, op: &'static Opcode) {
        // Double NOP
        // We need to advance PC even if it is NOP because there might be operands
        let _ = self.operand_addr_next(op.mode);
    }

    fn isb(&mut self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode);
        self.adc_impl(addr, 1, false);
        let value = addr.read_from(self);
        self.sbc_impl(Address::Accum, value, true);
    }

    fn kil(&mut self, _op: &'static Opcode) {
        todo!()
    }

    fn lar(&mut self, _op: &'static Opcode) {
        todo!()
    }

    fn lax(&mut self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode);
        let value = addr.read_from(self);
        self.reg_a = value;
        self.reg_x = value;

        self.status.set(Status::ZERO, value == 0);
        self.status.set(Status::NEGATIVE, value & SIGN_BIT != 0);
    }

    fn rla(&mut self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode);
        let value = addr.read_from(self);
        let res = self.rol_impl(addr, value);
        self.and_impl(Address::Accum, res);
    }

    fn rra(&mut self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode);
        let value = addr.read_from(self);
        self.ror_impl(addr, value);
        self.adc_impl(Address::Accum, value, true);
    }

    fn slo(&mut self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode);
        let res = self.asl_impl(addr);
        self.or_impl(Address::Accum, res);
    }

    fn sre(&mut self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode);
        let value = addr.read_from(self);
        let carry = value & 0b0000_0001 != 0;
        let result = value >> 1;
        addr.write_to(self, result);

        // EOR (XOR) with accumulator
        let xored = self.reg_a ^ result;
        self.reg_a = xored;

        self.status.set(Status::CARRY, carry);
        self.status.set(Status::ZERO, xored == 0);
        self.status.set(Status::NEGATIVE, xored & SIGN_BIT != 0);
    }

    fn sxa(&mut self, _op: &'static Opcode) {
        todo!()
    }

    fn sya(&mut self, _op: &'static Opcode) {
        todo!()
    }

    fn top(&mut self, op: &'static Opcode) {
        // Triple NOP
        // We need to advance PC even if it is NOP because there might be operands
        let _ = self.operand_addr_next(op.mode);
    }

    fn xaa(&mut self, _op: &'static Opcode) {
        todo!()
    }

    fn xas(&mut self, _op: &'static Opcode) {
        todo!()
    }

    // impls

    fn and_impl(&mut self, res_addr: Address, value: u8) -> u8 {
        let res = self.reg_a & value;
        res_addr.write_to(self, res);

        self.status.set(Status::ZERO, res == 0);
        self.status.set(Status::NEGATIVE, res & SIGN_BIT != 0);

        res
    }

    fn or_impl(&mut self, res_addr: Address, value: u8) -> u8 {
        let res = self.reg_a | value;
        res_addr.write_to(self, res);

        self.status.set(Status::ZERO, res == 0);
        self.status.set(Status::NEGATIVE, res & SIGN_BIT != 0);

        res
    }

    fn cmp_impl(&mut self, target_addr: Address, value: u8) {
        let target = target_addr.read_from(self);

        self.status.set(Status::CARRY, target >= value);
        self.status.set(Status::ZERO, target == value);
        self.status.set(
            Status::NEGATIVE,
            (target.wrapping_sub(value)) & SIGN_BIT != 0,
        );
    }

    fn asl_impl(&mut self, res_addr: Address) -> u8 {
        let value = res_addr.read_from(self);
        let carry = value & 0b1000_0000 != 0;
        let res = value.wrapping_shl(1);
        res_addr.write_to(self, res);

        self.status.set(Status::CARRY, carry);
        self.status.set(Status::ZERO, res == 0);
        self.status.set(Status::NEGATIVE, res & SIGN_BIT != 0);

        res
    }

    fn adc_impl(&mut self, res_addr: Address, value: u8, respect_carry: bool) {
        let carry = if respect_carry && self.status.contains(Status::CARRY) {
            1
        } else {
            0
        };

        let orig_value = res_addr.read_from(self);
        let res = orig_value.wrapping_add(value).wrapping_add(carry);
        let res_signed = (orig_value as i8)
            .wrapping_add(value as i8)
            .wrapping_add(carry as i8);

        let res_ext = u16::from(orig_value) + u16::from(value) + u16::from(carry);
        let res_ext_signed =
            i16::from(orig_value as i8) + i16::from(value as i8) + i16::from(carry);

        res_addr.write_to(self, res);

        self.status.set(Status::ZERO, res == 0);
        self.status.set(Status::NEGATIVE, res_signed < 0);
        if respect_carry {
            self.status.set(Status::CARRY, res_ext > u16::from(u8::MAX));
            self.status.set(
                Status::OVERFLOW,
                i16::from(res_signed.signum()) * res_ext_signed.signum() < 0,
            );
        }
    }

    fn sbc_impl(&mut self, res_addr: Address, value: u8, respect_carry: bool) {
        if respect_carry {
            // We need to add 1 to the value before negation, because:
            // sbc(reg_a, value)
            // = reg_a - value - (1 - carry)
            // = reg_a - (value + 1) + carry
            // = adc(reg_a, -(value + 1))
            self.adc_impl(res_addr, negate(value.wrapping_add(1)), true);
        } else {
            // No need to worry about the carry if we are not respecting it
            self.adc_impl(res_addr, negate(value), false);
        }
    }

    fn rol_impl(&mut self, res_addr: Address, value: u8) -> u8 {
        let next_carry = value & 0b1000_0000 != 0;
        let mut res = value << 1;
        if self.status.contains(Status::CARRY) {
            res |= 0b0000_0001;
        }

        res_addr.write_to(self, res);

        self.status.set(Status::CARRY, next_carry);
        self.status.set(Status::NEGATIVE, res & SIGN_BIT != 0);

        res
    }

    fn ror_impl(&mut self, res_addr: Address, value: u8) -> u8 {
        let next_carry = value & 0b0000_0001 != 0;
        let mut res = value >> 1;
        if self.status.contains(Status::CARRY) {
            res |= 0b1000_0000;
        }

        res_addr.write_to(self, res);

        self.status.set(Status::CARRY, next_carry);
        self.status.set(Status::NEGATIVE, res & SIGN_BIT != 0);

        res
    }
}

fn negate(value: u8) -> u8 {
    // Two's complement negation
    (!value).wrapping_add(1)
}

#[cfg(test)]
mod test {
    use crate::hardware::ram::Ram;

    use super::*;

    fn create_bus(program: &[u8]) -> Bus {
        // Memory that holds the testing program
        let mut mem_prg = Ram::new();
        mem_prg.load(0x0000, program);

        // Memory that is sometimes used to test jump instructions
        // (Some tests use 0x8000 as the target address for jumps)
        let mem_zp = Ram::new();

        let mut start_address = Ram::new();
        start_address.write_u16(0x0000, 0x8000);

        let mut bus = Bus::new();
        bus.connect(0x0000..=0x07ff, mem_zp);
        bus.connect(0x8000..=0x87ff, mem_prg);
        bus.connect(0xfffc..=0xffff, start_address);

        bus
    }

    #[test]
    fn test_0xa9_lda_immediate_load_data() {
        let bus = create_bus(&[0xa9, 0x05, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.reg_a, 0x05);
        assert!(!cpu.status.contains(Status::ZERO));
        assert!(!cpu.status.contains(Status::NEGATIVE));
    }

    #[test]
    fn test_0xa9_lda_zero_flag() {
        let bus = create_bus(&[0xa9, 0x00, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.run();

        assert!(cpu.status.contains(Status::ZERO));
    }

    #[test]
    fn test_0xaa_tax_move_a_to_x() {
        let bus = create_bus(&[0xaa, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_a = 10;
        cpu.run();

        assert_eq!(cpu.reg_x, 10)
    }

    #[test]
    fn test_5_ops_working_together() {
        let bus = create_bus(&[0xa9, 0xc0, 0xaa, 0xe8, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_a = 0xc0;
        cpu.run();

        assert_eq!(cpu.reg_x, 0xc1)
    }

    #[test]
    fn test_inx_overflow() {
        let bus = create_bus(&[0xe8, 0xe8, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_x = 0xff;
        cpu.run();

        assert_eq!(cpu.reg_x, 1)
    }

    #[test]
    fn test_lda_from_memory() {
        let bus = create_bus(&[0xa5, 0x10, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.bus.write(0x10, 0x55);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.reg_a, 0x55);
    }

    // ===== ADC (Add with Carry) Tests =====

    #[test]
    fn test_0x69_adc_immediate() {
        let bus = create_bus(&[0x69, 0x50, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_a = 0x30;
        cpu.run();

        assert_eq!(cpu.reg_a, 0x80);
    }

    #[test]
    fn test_0x69_adc_immediate_with_zero_result() {
        let bus = create_bus(&[0x69, 0x00, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_a = 0x00;
        cpu.run();

        assert!(cpu.status.contains(Status::ZERO));
    }

    #[test]
    fn test_0x65_adc_zero_page() {
        let bus = create_bus(&[0x65, 0x10, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.bus.write(0x10, 0x25);
        cpu.reset();
        cpu.reg_a = 0x25;
        cpu.run();

        assert_eq!(cpu.reg_a, 0x4a);
    }

    #[test]
    fn test_0x6d_adc_absolute() {
        let bus = create_bus(&[0x6d, 0x20, 0x80, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.bus.write(0x8020, 0x40);
        cpu.reset();
        cpu.reg_a = 0x10;
        cpu.run();

        assert_eq!(cpu.reg_a, 0x50);
    }

    #[test]
    fn test_0x69_adc_carry_flag() {
        // Carry flag is set when addition overflows (result > 255)
        // 0xFF (255) + 0x02 (2) = 0x101 (257 unsigned) -> Carry set, result 0x01
        let bus = create_bus(&[0x69, 0x02, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_a = 0xff;
        cpu.run();

        assert_eq!(cpu.reg_a, 0x01);
        assert!(cpu.status.contains(Status::CARRY));
    }

    #[test]
    fn test_0x69_adc_no_carry_flag() {
        // Carry flag is not set when addition doesn't overflow (result <= 255)
        // 0x50 (80) + 0x50 (80) = 0xA0 (160) -> No carry
        let bus = create_bus(&[0x69, 0x50, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_a = 0x50;
        cpu.run();

        assert_eq!(cpu.reg_a, 0xa0);
        assert!(!cpu.status.contains(Status::CARRY));
    }

    #[test]
    fn test_0x69_adc_overflow_positive_plus_positive() {
        // Overflow occurs when adding two positive numbers results in a negative number
        // 0x50 (80 as i8) + 0x40 (64 as i8) = 0x90 (-112 as i8) -> Overflow
        let bus = create_bus(&[0x69, 0x40, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_a = 0x50;
        cpu.run();

        assert_eq!(cpu.reg_a, 0x90);
        assert!(cpu.status.contains(Status::OVERFLOW));
    }

    #[test]
    fn test_0x69_adc_overflow_negative_plus_negative() {
        // Overflow occurs when adding two negative numbers results in a positive number
        // 0xB0 (-80 as i8) + 0xC0 (-64 as i8) = 0x70 (112 as i8) -> Overflow
        let bus = create_bus(&[0x69, 0xc0, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_a = 0xb0;
        cpu.run();

        assert_eq!(cpu.reg_a, 0x70);
        assert!(cpu.status.contains(Status::OVERFLOW));
    }

    #[test]
    fn test_0x69_adc_no_overflow_positive_plus_negative() {
        // No overflow when adding positive and negative numbers
        // 0x50 (80 as i8) + 0xD0 (-48 as i8) = 0x20 (32 as i8)
        let bus = create_bus(&[0x69, 0xd0, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_a = 0x50;
        cpu.run();

        assert_eq!(cpu.reg_a, 0x20);
        assert!(!cpu.status.contains(Status::OVERFLOW));
    }

    #[test]
    fn test_0x69_adc_no_overflow_positive_plus_positive_no_sign_change() {
        // No overflow when adding positive numbers that stay positive
        // 0x30 (48 as i8) + 0x20 (32 as i8) = 0x50 (80 as i8) - both positive, stays positive
        let bus = create_bus(&[0x69, 0x20, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_a = 0x30;
        cpu.run();

        assert_eq!(cpu.reg_a, 0x50);
        assert!(!cpu.status.contains(Status::OVERFLOW));
    }

    // ===== AND (Logical AND) Tests =====

    #[test]
    fn test_0x29_and_immediate() {
        let bus = create_bus(&[0x29, 0x0f, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_a = 0xf0;
        cpu.run();

        assert_eq!(cpu.reg_a, 0x00);
        assert!(cpu.status.contains(Status::ZERO));
    }

    #[test]
    fn test_0x25_and_zero_page() {
        let bus = create_bus(&[0x25, 0x10, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.bus.write(0x10, 0x0f);
        cpu.reset();
        cpu.reg_a = 0xf5;
        cpu.run();

        assert_eq!(cpu.reg_a, 0x05);
    }

    #[test]
    fn test_0x2d_and_absolute() {
        let bus = create_bus(&[0x2d, 0x20, 0x80, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.bus.write(0x8020, 0xff);
        cpu.reset();
        cpu.reg_a = 0x55;
        cpu.run();

        assert_eq!(cpu.reg_a, 0x55);
    }

    // ===== ASL (Arithmetic Shift Left) Tests =====

    #[test]
    fn test_0x0a_asl_accumulator() {
        let bus = create_bus(&[0x0a, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_a = 0x02;
        cpu.run();

        assert_eq!(cpu.reg_a, 0x04);
    }

    #[test]
    fn test_0x06_asl_zero_page() {
        let bus = create_bus(&[0x06, 0x10, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.bus.write(0x10, 0x40);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.bus.read(0x10), 0x80);
    }

    #[test]
    fn test_0x0e_asl_absolute() {
        let bus = create_bus(&[0x0e, 0x20, 0x80, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.bus.write(0x8020, 0x01);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.bus.read(0x8020), 0x02);
    }

    // ===== Branch Instructions Tests =====

    // BEQ (Branch if Equal - ZERO flag set)
    #[test]
    fn test_0xf0_beq_branch_taken() {
        let bus = create_bus(&[0xf0, 0x02, 0xff, 0xff, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.status.insert(Status::ZERO);
        cpu.run();

        // If branch is not taken by bug, CPU will panic due to unknown opcode 0xff
    }

    #[test]
    fn test_0xf0_beq_branch_not_taken() {
        let bus = create_bus(&[0xf0, 0x02, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.status = Status::empty();
        cpu.run();

        assert!(!cpu.status.contains(Status::ZERO));
    }

    // BNE (Branch if Not Equal - ZERO flag clear)
    #[test]
    fn test_0xd0_bne_branch_taken() {
        let bus = create_bus(&[0xd0, 0x02, 0xff, 0xff, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.status = Status::empty();
        cpu.run();

        // If branch is not taken by bug, CPU will panic due to unknown opcode 0xff
    }

    #[test]
    fn test_0xd0_bne_branch_not_taken() {
        let bus = create_bus(&[0xd0, 0x02, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.status.insert(Status::ZERO);
        cpu.run();

        assert!(cpu.status.contains(Status::ZERO));
    }

    // BCC (Branch if Carry Clear - CARRY flag clear)
    #[test]
    fn test_0x90_bcc_branch_taken() {
        let bus = create_bus(&[0x90, 0x02, 0xff, 0xff, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.status = Status::empty();
        cpu.run();

        // If branch is not taken by bug, CPU will panic due to unknown opcode 0xff
    }

    #[test]
    fn test_0x90_bcc_branch_not_taken() {
        let bus = create_bus(&[0x90, 0x02, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.status.insert(Status::CARRY);
        cpu.run();

        assert!(cpu.status.contains(Status::CARRY));
    }

    // BCS (Branch if Carry Set - CARRY flag set)
    #[test]
    fn test_0xb0_bcs_branch_taken() {
        let bus = create_bus(&[0xb0, 0x02, 0xff, 0xff, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.status.insert(Status::CARRY);
        cpu.run();

        // If branch is not taken by bug, CPU will panic due to unknown opcode 0xff
    }

    #[test]
    fn test_0xb0_bcs_branch_not_taken() {
        let bus = create_bus(&[0xb0, 0x02, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.status = Status::empty();
        cpu.run();
    }

    // BMI (Branch if Minus - NEGATIVE flag set)
    #[test]
    fn test_0x30_bmi_branch_taken() {
        let bus = create_bus(&[0x30, 0x02, 0xff, 0xff, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.status.insert(Status::NEGATIVE);
        cpu.run();

        // If branch is not taken by bug, CPU will panic due to unknown opcode 0xff
    }

    #[test]
    fn test_0x30_bmi_branch_not_taken() {
        let bus = create_bus(&[0x30, 0x02, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.status = Status::empty();
        cpu.run();
    }

    // BPL (Branch if Plus - NEGATIVE flag clear)
    #[test]
    fn test_0x10_bpl_branch_taken() {
        let bus = create_bus(&[0x10, 0x02, 0xff, 0xff, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.status = Status::empty();
        cpu.run();

        // If branch is not taken by bug, CPU will panic due to unknown opcode 0xff
    }

    #[test]
    fn test_0x10_bpl_branch_not_taken() {
        let bus = create_bus(&[0x10, 0x02, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.status.insert(Status::NEGATIVE);
        cpu.run();

        assert!(cpu.status.contains(Status::NEGATIVE));
    }

    // BVC (Branch if Overflow Clear - OVERFLOW flag clear)
    #[test]
    fn test_0x50_bvc_branch_taken() {
        let bus = create_bus(&[0x50, 0x02, 0xff, 0xff, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.status = Status::empty();
        cpu.run();

        // If branch is not taken by bug, CPU will panic due to unknown opcode 0xff
    }

    #[test]
    fn test_0x50_bvc_branch_not_taken() {
        let bus = create_bus(&[0x50, 0x02, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.status.insert(Status::OVERFLOW);
        cpu.run();

        assert!(cpu.status.contains(Status::OVERFLOW));
    }

    // BVS (Branch if Overflow Set - OVERFLOW flag set)
    #[test]
    fn test_0x70_bvs_branch_taken() {
        let bus = create_bus(&[0x70, 0x02, 0xff, 0xff, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.status.insert(Status::OVERFLOW);
        cpu.run();

        // If branch is not taken by bug, CPU will panic due to unknown opcode 0xff
    }

    #[test]
    fn test_0x70_bvs_branch_not_taken() {
        let bus = create_bus(&[0x70, 0x02, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.status = Status::empty();
        cpu.run();
    }

    // ===== BIT Test =====

    #[test]
    fn test_0x24_bit_zero_page() {
        let bus = create_bus(&[0x24, 0x10, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.bus.write(0x10, 0xc0);
        cpu.reset();
        cpu.reg_a = 0x3f;
        cpu.run();

        assert!(cpu.status.contains(Status::ZERO));
        assert!(cpu.status.contains(Status::NEGATIVE));
    }

    #[test]
    fn test_0x2c_bit_absolute() {
        let bus = create_bus(&[0x2c, 0x20, 0x80, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.bus.write(0x8020, 0x80);
        cpu.reset();
        cpu.reg_a = 0x01;
        cpu.run();

        assert!(cpu.status.contains(Status::ZERO));
        assert!(cpu.status.contains(Status::NEGATIVE));
    }

    // ===== BRK Test =====

    #[test]
    fn test_0x00_brk() {
        let bus = create_bus(&[0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.run();
    }

    // ===== CMP (Compare) Tests =====

    #[test]
    fn test_0xc9_cmp_immediate_equal() {
        let bus = create_bus(&[0xc9, 0x50, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_a = 0x50;
        cpu.run();

        assert!(cpu.status.contains(Status::CARRY));
        assert!(cpu.status.contains(Status::ZERO));
        assert!(!cpu.status.contains(Status::NEGATIVE));
    }

    #[test]
    fn test_0xc5_cmp_zero_page() {
        let bus = create_bus(&[0xc5, 0x10, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.bus.write(0x10, 0x30);
        cpu.reset();
        cpu.reg_a = 0x40;
        cpu.run();
    }

    #[test]
    fn test_0xcd_cmp_absolute() {
        let bus = create_bus(&[0xcd, 0x20, 0x80, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.bus.write(0x8020, 0x80);
        cpu.reset();
        cpu.reg_a = 0x80;
        cpu.run();

        assert!(cpu.status.contains(Status::ZERO));
    }

    // ===== CPX (Compare X) Tests =====

    #[test]
    fn test_0xe0_cpx_immediate_equal() {
        let bus = create_bus(&[0xe0, 0x40, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_x = 0x40;
        cpu.run();

        assert!(cpu.status.contains(Status::ZERO));
    }

    #[test]
    fn test_0xe4_cpx_zero_page() {
        let bus = create_bus(&[0xe4, 0x10, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.bus.write(0x10, 0x50);
        cpu.reset();
        cpu.reg_x = 0x50;
        cpu.run();

        assert!(cpu.status.contains(Status::ZERO));
    }

    #[test]
    fn test_0xec_cpx_absolute() {
        let bus = create_bus(&[0xec, 0x20, 0x80, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.bus.write(0x8020, 0x60);
        cpu.reset();
        cpu.reg_x = 0x60;
        cpu.run();

        assert!(cpu.status.contains(Status::ZERO));
    }

    // ===== CPY (Compare Y) Tests =====

    #[test]
    fn test_0xc0_cpy_immediate_equal() {
        let bus = create_bus(&[0xc0, 0x30, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_y = 0x30;
        cpu.run();

        assert!(cpu.status.contains(Status::ZERO));
    }

    #[test]
    fn test_0xc4_cpy_zero_page() {
        let bus = create_bus(&[0xc4, 0x10, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.bus.write(0x10, 0x70);
        cpu.reset();
        cpu.reg_y = 0x70;
        cpu.run();

        assert!(cpu.status.contains(Status::ZERO));
    }

    #[test]
    fn test_0xcc_cpy_absolute() {
        let bus = create_bus(&[0xcc, 0x20, 0x80, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.bus.write(0x8020, 0x90);
        cpu.reset();
        cpu.reg_y = 0x90;
        cpu.run();

        assert!(cpu.status.contains(Status::ZERO));
    }

    // ===== DEC (Decrement) Tests =====

    #[test]
    fn test_0xc6_dec_zero_page() {
        let bus = create_bus(&[0xc6, 0x10, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.bus.write(0x10, 0x10);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.bus.read(0x10), 0x0f);
    }

    #[test]
    fn test_0xce_dec_absolute() {
        let bus = create_bus(&[0xce, 0x20, 0x80, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.bus.write(0x8020, 0x01);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.bus.read(0x8020), 0x00);
        assert!(cpu.status.contains(Status::ZERO));
    }

    #[test]
    fn test_0xc6_dec_zero_page_underflow() {
        let bus = create_bus(&[0xc6, 0x10, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.bus.write(0x10, 0x00);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.bus.read(0x10), 0xff);
        assert!(cpu.status.contains(Status::NEGATIVE));
    }

    // ===== DEX (Decrement X) Tests =====

    #[test]
    fn test_0xca_dex() {
        let bus = create_bus(&[0xca, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_x = 0x10;
        cpu.run();

        assert_eq!(cpu.reg_x, 0x0f);
    }

    #[test]
    fn test_0xca_dex_underflow() {
        let bus = create_bus(&[0xca, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_x = 0x00;
        cpu.run();

        assert_eq!(cpu.reg_x, 0xff);
        assert!(cpu.status.contains(Status::NEGATIVE));
    }

    // ===== DEY (Decrement Y) Tests =====

    #[test]
    fn test_0x88_dey() {
        let bus = create_bus(&[0x88, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_y = 0x20;
        cpu.run();

        assert_eq!(cpu.reg_y, 0x1f);
    }

    #[test]
    fn test_0x88_dey_underflow() {
        let bus = create_bus(&[0x88, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_y = 0x00;
        cpu.run();

        assert_eq!(cpu.reg_y, 0xff);
        assert!(cpu.status.contains(Status::NEGATIVE));
    }

    // ===== EOR (Exclusive OR) Tests =====

    #[test]
    fn test_0x49_eor_immediate() {
        let bus = create_bus(&[0x49, 0x0f, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_a = 0xf0;
        cpu.run();

        assert_eq!(cpu.reg_a, 0xff);
    }

    #[test]
    fn test_0x45_eor_zero_page() {
        let bus = create_bus(&[0x45, 0x10, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.bus.write(0x10, 0x55);
        cpu.reset();
        cpu.reg_a = 0xaa;
        cpu.run();

        assert_eq!(cpu.reg_a, 0xff);
    }

    #[test]
    fn test_0x4d_eor_absolute() {
        let bus = create_bus(&[0x4d, 0x20, 0x80, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.bus.write(0x8020, 0xff);
        cpu.reset();
        cpu.reg_a = 0x00;
        cpu.run();

        assert_eq!(cpu.reg_a, 0xff);
    }

    // ===== INC (Increment) Tests =====

    #[test]
    fn test_0xe6_inc_zero_page() {
        let bus = create_bus(&[0xe6, 0x10, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.bus.write(0x10, 0x0f);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.bus.read(0x10), 0x10);
    }

    #[test]
    fn test_0xee_inc_absolute() {
        let bus = create_bus(&[0xee, 0x20, 0x80, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.bus.write(0x8020, 0xff);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.bus.read(0x8020), 0x00);
        assert!(cpu.status.contains(Status::ZERO));
    }

    #[test]
    fn test_0xe6_inc_zero_page_to_negative() {
        let bus = create_bus(&[0xe6, 0x10, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.bus.write(0x10, 0x7f);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.bus.read(0x10), 0x80);
        assert!(cpu.status.contains(Status::NEGATIVE));
    }

    // ===== INX (Increment X) Tests =====

    #[test]
    fn test_0xe8_inx() {
        let bus = create_bus(&[0xe8, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_x = 0x20;
        cpu.run();

        assert_eq!(cpu.reg_x, 0x21);
    }

    #[test]
    fn test_0xe8_inx_to_negative() {
        let bus = create_bus(&[0xe8, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_x = 0x7f;
        cpu.run();

        assert_eq!(cpu.reg_x, 0x80);
        assert!(cpu.status.contains(Status::NEGATIVE));
    }

    // ===== INY (Increment Y) Tests =====

    #[test]
    fn test_0xc8_iny() {
        let bus = create_bus(&[0xc8, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_y = 0x30;
        cpu.run();

        assert_eq!(cpu.reg_y, 0x31);
    }

    #[test]
    fn test_0xc8_iny_to_negative() {
        let bus = create_bus(&[0xc8, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_y = 0x7f;
        cpu.run();

        assert_eq!(cpu.reg_y, 0x80);
        assert!(cpu.status.contains(Status::NEGATIVE));
    }

    // ===== JMP (Jump) Tests =====

    #[test]
    fn test_0x4c_jmp_absolute() {
        let bus = create_bus(&[0x4c, 0x20, 0x80, 0xff]);
        let mut cpu = Cpu::new(bus);
        cpu.bus.write(0x8020, 0x00);
        cpu.reset();
        cpu.run();
    }

    #[test]
    fn test_0x6c_jmp_indirect() {
        let bus = create_bus(&[0x6c, 0x10, 0xff, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.bus.write_u16(0x10, 0x8002);
        cpu.reset();
        cpu.run();
    }

    // ===== JSR (Jump to Subroutine) Tests =====

    #[test]
    fn test_0x20_jsr() {
        let bus = create_bus(&[0x20, 0x20, 0x80, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.run();

        // return_addr should be the last byte of the JSR instruction
        let return_addr = cpu.stack_pop_u16();
        assert_eq!(return_addr, 0x8002);
    }

    // ===== LDA (Load Accumulator) Tests =====

    #[test]
    fn test_0xa5_lda_zero_page() {
        let bus = create_bus(&[0xa5, 0x10, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.bus.write(0x10, 0x42);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.reg_a, 0x42);
    }

    #[test]
    fn test_0xad_lda_absolute() {
        let bus = create_bus(&[0xad, 0x20, 0x80, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.bus.write(0x8020, 0x55);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.reg_a, 0x55);
    }

    #[test]
    fn test_0xb5_lda_zero_page_x() {
        let bus = create_bus(&[0xb5, 0x10, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.bus.write(0x15, 0x77);
        cpu.reset();
        cpu.reg_x = 0x05;
        cpu.run();

        assert_eq!(cpu.reg_a, 0x77);
    }

    #[test]
    fn test_0xbd_lda_absolute_x() {
        let bus = create_bus(&[0xbd, 0x20, 0x80, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.bus.write(0x8025, 0xaa);
        cpu.reset();
        cpu.reg_x = 0x05;
        cpu.run();

        assert_eq!(cpu.reg_a, 0xaa);
    }

    #[test]
    fn test_0xb9_lda_absolute_y() {
        let bus = create_bus(&[0xb9, 0x20, 0x80, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.bus.write(0x8030, 0xbb);
        cpu.reset();
        cpu.reg_y = 0x10;
        cpu.run();

        assert_eq!(cpu.reg_a, 0xbb);
    }

    // ===== LDX (Load X) Tests =====

    #[test]
    fn test_0xa2_ldx_immediate() {
        let bus = create_bus(&[0xa2, 0x44, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.reg_x, 0x44);
    }

    #[test]
    fn test_0xa6_ldx_zero_page() {
        let bus = create_bus(&[0xa6, 0x10, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.bus.write(0x10, 0x66);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.reg_x, 0x66);
    }

    #[test]
    fn test_0xae_ldx_absolute() {
        let bus = create_bus(&[0xae, 0x20, 0x80, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.bus.write(0x8020, 0x88);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.reg_x, 0x88);
    }

    #[test]
    fn test_0xb6_ldx_zero_page_y() {
        let bus = create_bus(&[0xb6, 0x10, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.bus.write(0x15, 0xcc);
        cpu.reset();
        cpu.reg_y = 0x05;
        cpu.run();

        assert_eq!(cpu.reg_x, 0xcc);
    }

    #[test]
    fn test_0xbe_ldx_absolute_y() {
        let bus = create_bus(&[0xbe, 0x20, 0x80, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.bus.write(0x8025, 0xdd);
        cpu.reset();
        cpu.reg_y = 0x05;
        cpu.run();

        assert_eq!(cpu.reg_x, 0xdd);
    }

    // ===== LDY (Load Y) Tests =====

    #[test]
    fn test_0xa0_ldy_immediate() {
        let bus = create_bus(&[0xa0, 0x33, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.reg_y, 0x33);
    }

    #[test]
    fn test_0xa4_ldy_zero_page() {
        let bus = create_bus(&[0xa4, 0x10, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.bus.write(0x10, 0x55);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.reg_y, 0x55);
    }

    #[test]
    fn test_0xac_ldy_absolute() {
        let bus = create_bus(&[0xac, 0x20, 0x80, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.bus.write(0x8020, 0x77);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.reg_y, 0x77);
    }

    #[test]
    fn test_0xb4_ldy_zero_page_x() {
        let bus = create_bus(&[0xb4, 0x10, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.bus.write(0x15, 0x99);
        cpu.reset();
        cpu.reg_x = 0x05;
        cpu.run();

        assert_eq!(cpu.reg_y, 0x99);
    }

    #[test]
    fn test_0xbc_ldy_absolute_x() {
        let bus = create_bus(&[0xbc, 0x20, 0x80, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.bus.write(0x8030, 0xee);
        cpu.reset();
        cpu.reg_x = 0x10;
        cpu.run();

        assert_eq!(cpu.reg_y, 0xee);
    }

    // ===== LSR (Logical Shift Right) Tests =====

    #[test]
    fn test_0x4a_lsr_accumulator() {
        let bus = create_bus(&[0x4a, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_a = 0x04;
        cpu.run();

        assert_eq!(cpu.reg_a, 0x02);
    }

    #[test]
    fn test_0x46_lsr_zero_page() {
        let bus = create_bus(&[0x46, 0x10, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.bus.write(0x10, 0x80);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.bus.read(0x10), 0x40);
    }

    #[test]
    fn test_0x4e_lsr_absolute() {
        let bus = create_bus(&[0x4e, 0x20, 0x80, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.bus.write(0x8020, 0x02);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.bus.read(0x8020), 0x01);
    }

    // ===== NOP (No Operation) Tests =====

    #[test]
    fn test_0xea_nop() {
        let bus = create_bus(&[0xea, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        let reg_a_before = cpu.reg_a;
        cpu.run();

        assert_eq!(cpu.reg_a, reg_a_before);
    }

    // ===== ORA (Logical OR) Tests =====

    #[test]
    fn test_0x09_ora_immediate() {
        let bus = create_bus(&[0x09, 0x0f, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_a = 0xf0;
        cpu.run();

        assert_eq!(cpu.reg_a, 0xff);
    }

    #[test]
    fn test_0x05_ora_zero_page() {
        let bus = create_bus(&[0x05, 0x10, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.bus.write(0x10, 0x0f);
        cpu.reset();
        cpu.reg_a = 0xf0;
        cpu.run();

        assert_eq!(cpu.reg_a, 0xff);
    }

    #[test]
    fn test_0x0d_ora_absolute() {
        let bus = create_bus(&[0x0d, 0x20, 0x80, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.bus.write(0x8020, 0x55);
        cpu.reset();
        cpu.reg_a = 0xaa;
        cpu.run();

        assert_eq!(cpu.reg_a, 0xff);
    }

    // ===== PHA (Push Accumulator) Tests =====

    #[test]
    fn test_0x48_pha() {
        let bus = create_bus(&[0x48, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_a = 0x42;
        cpu.run();
    }

    // ===== PHP (Push Processor Status) Tests =====

    #[test]
    fn test_0x08_php() {
        let bus = create_bus(&[0x08, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.status = Status::ZERO | Status::NEGATIVE;
        cpu.run();
    }

    // ===== PLA (Pull Accumulator) Tests =====

    #[test]
    fn test_0x68_pla() {
        let bus = create_bus(&[0x68, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.bus.write(0x01ff, 0x42);
        cpu.reset();
        cpu.sp = 0xfe;
        cpu.run();

        assert_eq!(cpu.reg_a, 0x42);
    }

    // ===== PLP (Pull Processor Status) Tests =====

    #[test]
    fn test_0x28_plp() {
        let bus = create_bus(&[0x28, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.bus
            .write(0x01ff, (Status::INTERRUPT_DISABLE | Status::B_FLAG).bits());
        cpu.reset();
        cpu.sp = 0xfe;
        cpu.run();

        // B Flag should be ignored when pulling status
        // RESERVED flag
        assert_eq!(cpu.status, Status::INTERRUPT_DISABLE | Status::RESERVED);
    }

    // ===== ROL (Rotate Left) Tests =====

    #[test]
    fn test_0x2a_rol_accumulator() {
        let bus = create_bus(&[0x2a, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_a = 0x40;
        cpu.run();
    }

    #[test]
    fn test_0x26_rol_zero_page() {
        let bus = create_bus(&[0x26, 0x10, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.bus.write(0x10, 0x40);
        cpu.reset();
        cpu.run();
    }

    #[test]
    fn test_0x2e_rol_absolute() {
        let bus = create_bus(&[0x2e, 0x20, 0x80, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.bus.write(0x8020, 0x40);
        cpu.reset();
        cpu.run();
    }

    // ===== ROR (Rotate Right) Tests =====

    #[test]
    fn test_0x6a_ror_accumulator() {
        let bus = create_bus(&[0x6a, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_a = 0x02;
        cpu.run();
    }

    #[test]
    fn test_0x66_ror_zero_page() {
        let bus = create_bus(&[0x66, 0x10, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.bus.write(0x10, 0x02);
        cpu.reset();
        cpu.run();
    }

    #[test]
    fn test_0x6e_ror_absolute() {
        let bus = create_bus(&[0x6e, 0x20, 0x80, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.bus.write(0x8020, 0x02);
        cpu.reset();
        cpu.run();
    }

    // ===== RTI (Return from Interrupt) Tests =====

    #[test]
    fn test_0x40_rti() {
        // RTI pops status and PC from stack
        // Load program with RTI instruction at 0x8000
        let bus = create_bus(&[0x40, 0xff, 0xff, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();

        // Set up stack with expected return address and status
        // RTI will pop in reverse order: first status, then PC (lo then hi)
        let return_addr = 0x8003;
        let expected_status = Status::ZERO | Status::NEGATIVE;

        // Push values onto stack (push_u16 pushes hi then lo, so stack will be: hi, lo)
        cpu.bus.write_u16(0x01fe, return_addr);
        cpu.bus.write(0x01fd, expected_status.bits());
        cpu.sp = 0xfc;

        cpu.run();

        // Verify RTI restored the status and PC correctly
        assert_eq!(cpu.status, expected_status | Status::RESERVED);
        assert_eq!(cpu.pc, return_addr + 1); // CPU stops at 0x8003 BRK so PC should be 0x8004
    }

    // ===== RTS (Return from Subroutine) Tests =====

    #[test]
    fn test_0x60_rts() {
        // RTS pops PC from stack
        // Load program with RTS instruction at 0x80bus00
        let bus = create_bus(&[0x60, 0xff, 0xff, 0xff, 0xff, 0x00, 0xff]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();

        // Set up stack with return address (minus one)
        cpu.bus.write_u16(0x01fe, 0x8004);
        cpu.sp = 0xfd;

        cpu.run();

        // Verify RTS restored the PC correctly (and then executed BRK)
        assert_eq!(cpu.pc, 0x8006);
    }

    // ===== SBC (Subtract with Carry) Tests =====

    #[test]
    fn test_0xe9_sbc_immediate() {
        let bus = create_bus(&[0xe9, 0x30, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_a = 0x50;
        cpu.run();
    }

    #[test]
    fn test_0xe5_sbc_zero_page() {
        let bus = create_bus(&[0xe5, 0x10, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.bus.write(0x10, 0x20);
        cpu.reset();
        cpu.reg_a = 0x50;
        cpu.run();
    }

    #[test]
    fn test_0xed_sbc_absolute() {
        let bus = create_bus(&[0xed, 0x20, 0x80, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.bus.write(0x8020, 0x30);
        cpu.reset();
        cpu.reg_a = 0x60;
        cpu.run();
    }

    #[test]
    fn test_0xe9_sbc_carry_flag_borrow() {
        // In 6502, SBC uses carry as borrow. Carry clear means borrow occurred.
        // 0x50 (80) - 0xFF (255) requires borrow, so carry is cleared
        let bus = create_bus(&[0xe9, 0xff, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_a = 0x50;
        cpu.run();

        // Result: 0x50 - 0xFF - 1 (borrow) = 0x50
        assert!(!cpu.status.contains(Status::CARRY));
    }

    #[test]
    fn test_0xe9_sbc_carry_flag_no_borrow() {
        // Carry is set when subtraction doesn't require borrow
        // 0x50 (80) - 0x30 (48) = 0x20 (32) -> No borrow
        let bus = create_bus(&[0xe9, 0x30, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_a = 0x50;
        cpu.status.insert(Status::CARRY); // Set carry before operation to prevent borrow
        cpu.run();

        assert_eq!(cpu.reg_a, 0x20);
        assert!(cpu.status.contains(Status::CARRY));
    }

    #[test]
    fn test_0xe9_sbc_overflow_positive_minus_negative() {
        // Overflow occurs when subtracting a negative number from a positive number results in negative
        // 0x50 (80 as i8) - 0xC0 (-64 as i8) = 0x50 - (-64) = 0x90 (-112 as i8) -> Overflow
        let bus = create_bus(&[0xe9, 0xc0, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_a = 0x50;
        cpu.status.insert(Status::CARRY); // Set carry to avoid borrow
        cpu.run();

        assert_eq!(cpu.reg_a, 0x90);
        assert!(cpu.status.contains(Status::OVERFLOW));
    }

    #[test]
    fn test_0xe9_sbc_overflow_negative_minus_positive() {
        // Overflow occurs when subtracting a positive number from a negative number results in positive
        // 0xC0 (-64 as i8) - 0x50 (80 as i8) = 0xC0 - 80 = 0x70 (112 as i8) -> Overflow
        let bus = create_bus(&[0xe9, 0x50, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_a = 0xc0;
        cpu.status.insert(Status::CARRY); // Set carry to avoid borrow
        cpu.run();

        assert_eq!(cpu.reg_a, 0x70);
        assert!(cpu.status.contains(Status::OVERFLOW));
    }

    #[test]
    fn test_0xe9_sbc_no_overflow_positive_minus_positive() {
        // No overflow when subtracting positive from positive (both positive)
        // 0x50 (80 as i8) - 0x20 (32 as i8) = 0x30 (48 as i8)
        let bus = create_bus(&[0xe9, 0x20, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_a = 0x50;
        cpu.status.insert(Status::CARRY); // Set carry to avoid borrow
        cpu.run();

        assert_eq!(cpu.reg_a, 0x30);
        assert!(!cpu.status.contains(Status::OVERFLOW));
    }

    #[test]
    fn test_0xe9_sbc_no_overflow_negative_minus_negative() {
        // No overflow when subtracting negative from negative
        // 0xD0 (-48 as i8) - 0xA0 (-96 as i8) = 0x30 (48 as i8)
        let bus = create_bus(&[0xe9, 0xa0, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_a = 0xd0;
        cpu.status.insert(Status::CARRY); // Set carry to avoid borrow
        cpu.run();

        assert_eq!(cpu.reg_a, 0x30);
        assert!(!cpu.status.contains(Status::OVERFLOW));
    }

    // ===== SEC (Set Carry) Tests =====

    #[test]
    fn test_0x38_sec() {
        let bus = create_bus(&[0x38, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.run();
    }

    // ===== SED (Set Decimal) Tests =====

    #[test]
    fn test_0xf8_sed() {
        let bus = create_bus(&[0xf8, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.run();
    }

    // ===== SEI (Set Interrupt Disable) Tests =====

    #[test]
    fn test_0x78_sei() {
        let bus = create_bus(&[0x78, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.run();
    }

    // ===== CLC (Clear Carry) Tests =====

    #[test]
    fn test_0x18_clc() {
        let bus = create_bus(&[0x18, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.run();
    }

    // ===== CLD (Clear Decimal) Tests =====

    #[test]
    fn test_0xd8_cld() {
        let bus = create_bus(&[0xd8, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.run();
    }

    // ===== CLI (Clear Interrupt Disable) Tests =====

    #[test]
    fn test_0x58_cli() {
        let bus = create_bus(&[0x58, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.run();
    }

    // ===== CLV (Clear Overflow) Tests =====

    #[test]
    fn test_0xb8_clv() {
        let bus = create_bus(&[0xb8, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.run();
    }

    // ===== STA (Store Accumulator) Tests =====

    #[test]
    fn test_0x85_sta_zero_page() {
        let bus = create_bus(&[0x85, 0x10, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_a = 0x42;
        cpu.run();

        assert_eq!(cpu.bus.read(0x10), 0x42);
    }

    #[test]
    fn test_0x8d_sta_absolute() {
        let bus = create_bus(&[0x8d, 0x20, 0x80, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_a = 0x55;
        cpu.run();

        assert_eq!(cpu.bus.read(0x8020), 0x55);
    }

    #[test]
    fn test_0x95_sta_zero_page_x() {
        let bus = create_bus(&[0x95, 0x10, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_a = 0x77;
        cpu.reg_x = 0x05;
        cpu.run();

        assert_eq!(cpu.bus.read(0x15), 0x77);
    }

    #[test]
    fn test_0x9d_sta_absolute_x() {
        let bus = create_bus(&[0x9d, 0x20, 0x80, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_a = 0xaa;
        cpu.reg_x = 0x05;
        cpu.run();

        assert_eq!(cpu.bus.read(0x8025), 0xaa);
    }

    #[test]
    fn test_0x99_sta_absolute_y() {
        let bus = create_bus(&[0x99, 0x20, 0x80, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_a = 0xbb;
        cpu.reg_y = 0x10;
        cpu.run();

        assert_eq!(cpu.bus.read(0x8030), 0xbb);
    }

    // ===== STX (Store X) Tests =====

    #[test]
    fn test_0x86_stx_zero_page() {
        let bus = create_bus(&[0x86, 0x10, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_x = 0x44;
        cpu.run();

        assert_eq!(cpu.bus.read(0x10), 0x44);
    }

    #[test]
    fn test_0x8e_stx_absolute() {
        let bus = create_bus(&[0x8e, 0x20, 0x80, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_x = 0x66;
        cpu.run();

        assert_eq!(cpu.bus.read(0x8020), 0x66);
    }

    #[test]
    fn test_0x96_stx_zero_page_y() {
        let bus = create_bus(&[0x96, 0x10, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_x = 0x88;
        cpu.reg_y = 0x05;
        cpu.run();

        assert_eq!(cpu.bus.read(0x15), 0x88);
    }

    // ===== STY (Store Y) Tests =====

    #[test]
    fn test_0x84_sty_zero_page() {
        let bus = create_bus(&[0x84, 0x10, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_y = 0x33;
        cpu.run();

        assert_eq!(cpu.bus.read(0x10), 0x33);
    }

    #[test]
    fn test_0x8c_sty_absolute() {
        let bus = create_bus(&[0x8c, 0x20, 0x80, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_y = 0x55;
        cpu.run();

        assert_eq!(cpu.bus.read(0x8020), 0x55);
    }

    #[test]
    fn test_0x94_sty_zero_page_x() {
        let bus = create_bus(&[0x94, 0x10, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_y = 0x77;
        cpu.reg_x = 0x05;
        cpu.run();

        assert_eq!(cpu.bus.read(0x15), 0x77);
    }

    // ===== TAX (Transfer A to X) Tests =====

    #[test]
    fn test_0xaa_tax_non_zero() {
        let bus = create_bus(&[0xaa, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_a = 0x42;
        cpu.run();

        assert_eq!(cpu.reg_x, 0x42);
        assert!(!cpu.status.contains(Status::ZERO));
    }

    #[test]
    fn test_0xaa_tax_zero() {
        let bus = create_bus(&[0xaa, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_a = 0x00;
        cpu.run();

        assert_eq!(cpu.reg_x, 0x00);
        assert!(cpu.status.contains(Status::ZERO));
    }

    #[test]
    fn test_0xaa_tax_negative() {
        let bus = create_bus(&[0xaa, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_a = 0x80;
        cpu.run();

        assert_eq!(cpu.reg_x, 0x80);
        assert!(cpu.status.contains(Status::NEGATIVE));
    }

    // ===== TAY (Transfer A to Y) Tests =====

    #[test]
    fn test_0xa8_tay() {
        let bus = create_bus(&[0xa8, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_a = 0x50;
        cpu.run();

        assert_eq!(cpu.reg_y, 0x50);
    }

    #[test]
    fn test_0xa8_tay_zero() {
        let bus = create_bus(&[0xa8, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_a = 0x00;
        cpu.run();

        assert_eq!(cpu.reg_y, 0x00);
        assert!(cpu.status.contains(Status::ZERO));
    }

    // ===== TSX (Transfer Stack Pointer to X) Tests =====

    #[test]
    fn test_0xba_tsx() {
        let bus = create_bus(&[0xba, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.run();
    }

    // ===== TXA (Transfer X to A) Tests =====

    #[test]
    fn test_0x8a_txa() {
        let bus = create_bus(&[0x8a, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_x = 0x60;
        cpu.run();

        assert_eq!(cpu.reg_a, 0x60);
    }

    #[test]
    fn test_0x8a_txa_zero() {
        let bus = create_bus(&[0x8a, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_x = 0x00;
        cpu.run();

        assert_eq!(cpu.reg_a, 0x00);
        assert!(cpu.status.contains(Status::ZERO));
    }

    // ===== TXS (Transfer X to Stack Pointer) Tests =====

    #[test]
    fn test_0x9a_txs() {
        let bus = create_bus(&[0x9a, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_x = 0x70;
        cpu.run();
    }

    // ===== TYA (Transfer Y to A) Tests =====

    #[test]
    fn test_0x98_tya() {
        let bus = create_bus(&[0x98, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_y = 0x80;
        cpu.run();

        assert_eq!(cpu.reg_a, 0x80);
        assert!(cpu.status.contains(Status::NEGATIVE));
    }

    #[test]
    fn test_0x98_tya_zero() {
        let bus = create_bus(&[0x98, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_y = 0x00;
        cpu.run();

        assert_eq!(cpu.reg_a, 0x00);
        assert!(cpu.status.contains(Status::ZERO));
    }

    // ===== Indexed Indirect Addressing Mode Tests =====

    #[test]
    fn test_0xa1_lda_indexed_indirect() {
        let bus = create_bus(&[0xa1, 0x10, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.bus.write_u16(0x15, 0x8020);
        cpu.bus.write(0x8020, 0x42);
        cpu.reset();
        cpu.reg_x = 0x05;
        cpu.run();

        assert_eq!(cpu.reg_a, 0x42);
    }

    #[test]
    fn test_0xb1_lda_indirect_indexed() {
        let bus = create_bus(&[0xb1, 0x10, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.bus.write_u16(0x10, 0x8020);
        cpu.bus.write(0x8025, 0x55);
        cpu.reset();
        cpu.reg_y = 0x05;
        cpu.run();

        assert_eq!(cpu.reg_a, 0x55);
    }

    #[test]
    fn test_0x81_sta_indexed_indirect() {
        let bus = create_bus(&[0x81, 0x10, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.bus.write_u16(0x15, 0x8020);
        cpu.reset();
        cpu.reg_a = 0x99;
        cpu.reg_x = 0x05;
        cpu.run();

        assert_eq!(cpu.bus.read(0x8020), 0x99);
    }

    #[test]
    fn test_0x91_sta_indirect_indexed() {
        let bus = create_bus(&[0x91, 0x10, 0x00]);
        let mut cpu = Cpu::new(bus);
        cpu.bus.write_u16(0x10, 0x8020);
        cpu.reset();
        cpu.reg_a = 0xcc;
        cpu.reg_y = 0x05;
        cpu.run();

        assert_eq!(cpu.bus.read(0x8025), 0xcc);
    }

    // ===== Disassemble Tests by Addressing Mode =====

    #[test]
    fn disassemble_implied() {
        let bus = create_bus(&[]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();

        // Example: NOP (nestest.log: "NOP")
        let result = disassemble(&mut cpu, &[0xea]); // NOP
        assert_eq!(result.repr, "NOP");
        assert_eq!(result.addr_value_hint, None);

        // Example: SEC (nestest.log: "SEC")
        let result = disassemble(&mut cpu, &[0x38]); // SEC
        assert_eq!(result.repr, "SEC");
        assert_eq!(result.addr_value_hint, None);

        // Example: CLC (nestest.log: "CLC")
        let result = disassemble(&mut cpu, &[0x18]); // CLC
        assert_eq!(result.repr, "CLC");
        assert_eq!(result.addr_value_hint, None);
    }

    #[test]
    fn disassemble_accumulator() {
        let bus = create_bus(&[]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();

        // Example: ASL A (nestest.log: "ASL A")
        let result = disassemble(&mut cpu, &[0x0a]); // ASL A
        assert_eq!(result.repr, "ASL A");
        assert_eq!(result.addr_value_hint, None);

        // Example: LSR A (nestest.log: "LSR A")
        let result = disassemble(&mut cpu, &[0x4a]); // LSR A
        assert_eq!(result.repr, "LSR A");
        assert_eq!(result.addr_value_hint, None);

        // Example: ROR A (nestest.log: "ROR A")
        let result = disassemble(&mut cpu, &[0x6a]); // ROR A
        assert_eq!(result.repr, "ROR A");
        assert_eq!(result.addr_value_hint, None);
    }

    #[test]
    fn disassemble_immediate() {
        let bus = create_bus(&[]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();

        // Example: LDA #$00 (nestest.log: "LDA #$00")
        let result = disassemble(&mut cpu, &[0xa9, 0x00]); // LDA #$00
        assert_eq!(result.repr, "LDA #$00");
        assert_eq!(result.addr_value_hint, None);

        // Example: LDA #$40
        let result = disassemble(&mut cpu, &[0xa9, 0x40]); // LDA #$40
        assert_eq!(result.repr, "LDA #$40");
        assert_eq!(result.addr_value_hint, None);

        // Example: LDA #$FF
        let result = disassemble(&mut cpu, &[0xa9, 0xff]); // LDA #$FF
        assert_eq!(result.repr, "LDA #$FF");
        assert_eq!(result.addr_value_hint, None);

        // Example: AND #$EF
        let result = disassemble(&mut cpu, &[0x29, 0xef]); // AND #$EF
        assert_eq!(result.repr, "AND #$EF");
        assert_eq!(result.addr_value_hint, None);
    }

    #[test]
    fn disassemble_zero_page() {
        let bus = create_bus(&[]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.bus.write(0x00, 0x00);
        cpu.bus.write(0x01, 0xff);
        cpu.bus.write(0x10, 0x00);

        // Example: LDA $00 (nestest.log: "LDA $00 = 00")
        let result = disassemble(&mut cpu, &[0xa5, 0x00]); // LDA $00
        assert_eq!(result.repr, "LDA $00");
        assert_eq!(result.addr_value_hint, Some("= 00".to_string()));

        // Example: STA $01 = FF
        let result = disassemble(&mut cpu, &[0x85, 0x01]); // STA $01
        assert_eq!(result.repr, "STA $01");
        assert_eq!(result.addr_value_hint, Some("= FF".to_string()));

        // Example: BIT $01 = FF
        let result = disassemble(&mut cpu, &[0x24, 0x10]); // BIT $10
        assert_eq!(result.repr, "BIT $10");
        assert_eq!(result.addr_value_hint, Some("= 00".to_string()));
    }

    #[test]
    fn disassemble_zero_page_x() {
        let bus = create_bus(&[]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_x = 0x10;
        cpu.bus.write(0x15, 0xaa);

        // Example: STY $33,X @ 33 = AA (nestest.log format)
        let result = disassemble(&mut cpu, &[0xb5, 0x05]); // LDA $05,X
        assert_eq!(result.repr, "LDA $05,X");
        assert_eq!(result.addr_value_hint, Some("@ 15 = AA".to_string()));

        cpu.reg_x = 0x00;
        let result = disassemble(&mut cpu, &[0x86, 0x00]); // STX $00
        assert_eq!(result.repr, "STX $00");
        assert_eq!(result.addr_value_hint, Some("= 00".to_string()));
    }

    #[test]
    fn disassemble_zero_page_y() {
        let bus = create_bus(&[]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_y = 0x10;
        cpu.bus.write(0x15, 0xbb);

        // Example: LDX $00,Y @ 78 = 33 (nestest.log format)
        let result = disassemble(&mut cpu, &[0xb6, 0x05]); // LDX $05,Y
        assert_eq!(result.repr, "LDX $05,Y");
        assert_eq!(result.addr_value_hint, Some("@ 15 = BB".to_string()));

        cpu.reg_y = 0x00;
        cpu.bus.write(0x01, 0xff);
        let result = disassemble(&mut cpu, &[0x96, 0x01]); // STX $01,Y
        assert_eq!(result.repr, "STX $01,Y");
        assert_eq!(result.addr_value_hint, Some("@ 01 = FF".to_string()));
    }

    #[test]
    fn disassemble_absolute() {
        let bus = create_bus(&[]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.bus.write(0x8020, 0x42);

        // Example: JMP $8020 = 42 (4-digit hex address)
        let result = disassemble(&mut cpu, &[0x4c, 0x20, 0x80]); // JMP $8020
        assert_eq!(result.repr, "JMP $8020");
        assert_eq!(result.addr_value_hint, None);

        let result = disassemble(&mut cpu, &[0xad, 0x20, 0x80]); // LDA $8020
        assert_eq!(result.repr, "LDA $8020");
        assert_eq!(result.addr_value_hint, Some("= 42".to_string()));
    }

    #[test]
    fn disassemble_absolute_x() {
        let bus = create_bus(&[]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_x = 0x10;
        cpu.bus.write(0x0633, 0x99);

        // Example: LDY $33,X @ 33 = AA (4-digit result address)
        let result = disassemble(&mut cpu, &[0xbc, 0x23, 0x06]); // LDY $0623,X
        assert_eq!(result.repr, "LDY $0623,X");
        assert_eq!(result.addr_value_hint, Some("@ 0633 = 99".to_string()));
    }

    #[test]
    fn disassemble_absolute_y() {
        let bus = create_bus(&[]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_y = 0x10;
        cpu.bus.write(0x0610, 0x77);

        // Example: LDX $0600,Y @ 0610 = 77
        let result = disassemble(&mut cpu, &[0xbe, 0x00, 0x06]); // LDX $0600,Y
        assert_eq!(result.repr, "LDX $0600,Y");
        assert_eq!(result.addr_value_hint, Some("@ 0610 = 77".to_string()));
    }

    #[test]
    fn disassemble_relative() {
        let bus = create_bus(&[]);
        let mut cpu = Cpu::new(bus);
        cpu.pc = 0x8000;

        // Example: BCS $8005
        let result = disassemble(&mut cpu, &[0xb0, 0x03]);
        assert_eq!(result.repr, "BCS $8005");
        assert_eq!(result.addr_value_hint, None);

        // Negative offset test
        cpu.pc = 0x8010;
        cpu.bus.write(0x800C, 0xaa);
        let result = disassemble(&mut cpu, &[0xf0, 0xfc]);
        assert_eq!(result.repr, "BEQ $800E");
        assert_eq!(result.addr_value_hint, None);
    }

    #[test]
    fn disassemble_indirect() {
        let bus = create_bus(&[]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.bus.write_u16(0x0200, 0xdb7e);

        // Example: JMP ($0200) = DB7E (nestest.log format - 4 digit result)
        let result = disassemble(&mut cpu, &[0x6c, 0x00, 0x02]); // JMP ($0200)
        assert_eq!(result.repr, "JMP ($0200)");
        assert_eq!(result.addr_value_hint, Some("= DB7E".to_string()));
    }

    #[test]
    fn disassemble_indexed_indirect() {
        let bus = create_bus(&[]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_x = 0x00;
        cpu.bus.write_u16(0x80, 0x0200);
        cpu.bus.write(0x0200, 0x5a);

        // Example: LDA ($80,X) @ 80 = 0200 = 5A (nestest.log format)
        let result = disassemble(&mut cpu, &[0xa1, 0x80]); // LDA ($80,X)
        assert_eq!(result.repr, "LDA ($80,X)");
        assert_eq!(result.addr_value_hint, Some("@ 80 = 0200 = 5A".to_string()));

        // Test with X offset
        cpu.reg_x = 0x02;
        cpu.bus.write_u16(0x82, 0x0300);
        cpu.bus.write(0x0300, 0x5b);
        let result = disassemble(&mut cpu, &[0xa1, 0x80]); // LDA ($80,X) with X=2
        assert_eq!(result.repr, "LDA ($80,X)");
        assert_eq!(result.addr_value_hint, Some("@ 82 = 0300 = 5B".to_string()));
    }

    #[test]
    fn disassemble_indirect_indexed() {
        let bus = create_bus(&[]);
        let mut cpu = Cpu::new(bus);
        cpu.reset();
        cpu.reg_y = 0x00;
        cpu.bus.write_u16(0x89, 0x0300);
        cpu.bus.write(0x0300, 0x89);

        // Example: LDA ($89),Y = 0300 @ 0300 = 89 (nestest.log format)
        let result = disassemble(&mut cpu, &[0xb1, 0x89]); // LDA ($89),Y
        assert_eq!(result.repr, "LDA ($89),Y");
        assert_eq!(
            result.addr_value_hint,
            Some("= 0300 @ 0300 = 89".to_string())
        );

        // Test with Y offset
        cpu.reg_y = 0x34;
        cpu.bus.write_u16(0x97, 0xffff);
        cpu.bus.write(0x0033, 0xa3);
        let result = disassemble(&mut cpu, &[0xb1, 0x97]); // LDA ($97),Y with Y=0x34
        assert_eq!(result.repr, "LDA ($97),Y");
        assert_eq!(
            result.addr_value_hint,
            Some("= FFFF @ 0033 = A3".to_string())
        );
    }

    // #[test]
    // fn test_dump_state_format_normal() {
    //     let bus = create_bus(&[0xa2, 0x01, 0xca, 0x88, 0x00]);
    //     let mut cpu = Cpu::new(bus);
    //     cpu.reset();
    //     cpu.reg_a = 1;
    //     cpu.reg_x = 2;
    //     cpu.reg_y = 3;
    //
    //     let mut log = vec![];
    //
    //     while !cpu.is_halted() {
    //         log.push(cpu.dump_state());
    //         cpu.step();
    //     }
    //
    //     assert_eq!(
    //         "8000  A2 01     LDX #$01                        A:01 X:02 Y:03 P:24 SP:FD",
    //         log[0]
    //     );
    //     assert_eq!(
    //         "8002  CA        DEX                             A:01 X:01 Y:03 P:24 SP:FD",
    //         log[1]
    //     );
    //     assert_eq!(
    //         "8003  88        DEY                             A:01 X:00 Y:03 P:26 SP:FD",
    //         log[2]
    //     );
    //     assert_eq!(
    //         "8004  00        BRK                             A:01 X:00 Y:03 P:26 SP:FD",
    //         log[3]
    //     );
    // }
}
