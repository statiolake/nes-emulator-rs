bitflags::bitflags! {
    pub struct Status: u8 {
        const CARRY = 0b0000_0001;
        const ZERO = 0b0000_0010;
        const INTERRUPT_DISABLE = 0b0000_0100;
        // const DECIMAL_MODE = 0b0000_1000; // not supported on NES
        const BREAK_COMMAND = 0b0001_0000;
        const OVERFLOW = 0b0010_0000;
        const NEGATIVE = 0b0100_0000;
    }
}

const SIGN_BIT: u8 = 0b1000_0000;

pub struct Memory {
    pub data: [u8; 0xffff],
}

impl Memory {
    pub fn new() -> Self {
        Memory { data: [0; 0xffff] }
    }

    pub fn load(&mut self, start_address: u16, program: &[u8]) {
        self.data[start_address as usize..(start_address as usize + program.len())]
            .copy_from_slice(program);
    }

    pub fn read(&mut self, address: u16) -> u8 {
        self.data[address as usize]
    }

    pub fn read_u16(&mut self, address: u16) -> u16 {
        let lo = self.read(address);
        let hi = self.read(address + 1);
        u16::from_le_bytes([lo, hi])
    }

    pub fn write(&mut self, address: u16, value: u8) {
        self.data[address as usize] = value;
    }

    pub fn write_u16(&mut self, address: u16, value: u16) {
        let bytes = value.to_le_bytes();
        self.write(address, bytes[0]);
        self.write(address + 1, bytes[1]);
    }
}

impl Default for Memory {
    fn default() -> Self {
        Self::new()
    }
}

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

    /// ($01)
    Indirect,

    /// ($01, X)
    IndexedIndirect,

    /// ($01), Y
    IndirectIndexed,

    /// A register or for instructions that do not use addressing modes
    None,
}

pub struct Op {
    pub code: u8,
    pub name: &'static str,
    pub mode: AddressingMode,
    pub cycles: u8,
    pub handler: fn(&mut Cpu, &'static Op),
}

impl Op {
    pub const fn new(
        code: u8,
        name: &'static str,
        mode: AddressingMode,
        cycles: u8,
        handler: fn(&mut Cpu, &'static Op),
    ) -> Self {
        Op {
            code,
            name,
            mode,
            cycles,
            handler,
        }
    }
}

pub const CPU_OPS: &[Op] = &[
    Op::new(0x61, "ADC", AddressingMode::IndexedIndirect, 6, Cpu::adc),
    Op::new(0x65, "ADC", AddressingMode::ZeroPage, 3, Cpu::adc),
    Op::new(0x69, "ADC", AddressingMode::Immediate, 2, Cpu::adc),
    Op::new(0x6D, "ADC", AddressingMode::Absolute, 4, Cpu::adc),
    Op::new(0x71, "ADC", AddressingMode::IndirectIndexed, 5, Cpu::adc),
    Op::new(0x75, "ADC", AddressingMode::ZeroPageX, 4, Cpu::adc),
    Op::new(0x79, "ADC", AddressingMode::AbsoluteY, 4, Cpu::adc),
    Op::new(0x7D, "ADC", AddressingMode::AbsoluteX, 4, Cpu::adc),
    Op::new(0x21, "AND", AddressingMode::IndexedIndirect, 6, Cpu::and),
    Op::new(0x25, "AND", AddressingMode::ZeroPage, 3, Cpu::and),
    Op::new(0x29, "AND", AddressingMode::Immediate, 2, Cpu::and),
    Op::new(0x2D, "AND", AddressingMode::Absolute, 4, Cpu::and),
    Op::new(0x31, "AND", AddressingMode::IndirectIndexed, 5, Cpu::and),
    Op::new(0x35, "AND", AddressingMode::ZeroPageX, 4, Cpu::and),
    Op::new(0x39, "AND", AddressingMode::AbsoluteY, 4, Cpu::and),
    Op::new(0x3D, "AND", AddressingMode::AbsoluteX, 4, Cpu::and),
    Op::new(0x06, "ASL", AddressingMode::ZeroPage, 5, Cpu::asl),
    Op::new(0x0A, "ASL", AddressingMode::None, 2, Cpu::asl),
    Op::new(0x0E, "ASL", AddressingMode::Absolute, 6, Cpu::asl),
    Op::new(0x16, "ASL", AddressingMode::ZeroPageX, 6, Cpu::asl),
    Op::new(0x1E, "ASL", AddressingMode::AbsoluteX, 7, Cpu::asl),
    Op::new(0x90, "BCC", AddressingMode::Relative, 2, Cpu::bcc),
    Op::new(0xB0, "BCS", AddressingMode::Relative, 2, Cpu::bcs),
    Op::new(0xF0, "BEQ", AddressingMode::Relative, 2, Cpu::beq),
    Op::new(0x24, "BIT", AddressingMode::ZeroPage, 3, Cpu::bit),
    Op::new(0x2C, "BIT", AddressingMode::Absolute, 4, Cpu::bit),
    Op::new(0x30, "BMI", AddressingMode::Relative, 2, Cpu::bmi),
    Op::new(0xD0, "BNE", AddressingMode::Relative, 2, Cpu::bne),
    Op::new(0x10, "BPL", AddressingMode::Relative, 2, Cpu::bpl),
    Op::new(0x00, "BRK", AddressingMode::None, 7, Cpu::brk),
    Op::new(0x50, "BVC", AddressingMode::Relative, 2, Cpu::bvc),
    Op::new(0x70, "BVS", AddressingMode::Relative, 2, Cpu::bvs),
    Op::new(0x18, "CLC", AddressingMode::None, 2, Cpu::clc),
    Op::new(0xD8, "CLD", AddressingMode::None, 2, Cpu::cld),
    Op::new(0x58, "CLI", AddressingMode::None, 2, Cpu::cli),
    Op::new(0xB8, "CLV", AddressingMode::None, 2, Cpu::clv),
    Op::new(0xC1, "CMP", AddressingMode::IndexedIndirect, 6, Cpu::cmp),
    Op::new(0xC5, "CMP", AddressingMode::ZeroPage, 3, Cpu::cmp),
    Op::new(0xC9, "CMP", AddressingMode::Immediate, 2, Cpu::cmp),
    Op::new(0xCD, "CMP", AddressingMode::Absolute, 4, Cpu::cmp),
    Op::new(0xD1, "CMP", AddressingMode::IndirectIndexed, 5, Cpu::cmp),
    Op::new(0xD5, "CMP", AddressingMode::ZeroPageX, 4, Cpu::cmp),
    Op::new(0xD9, "CMP", AddressingMode::AbsoluteY, 4, Cpu::cmp),
    Op::new(0xDD, "CMP", AddressingMode::AbsoluteX, 4, Cpu::cmp),
    Op::new(0xE0, "CPX", AddressingMode::Immediate, 2, Cpu::cpx),
    Op::new(0xE4, "CPX", AddressingMode::ZeroPage, 3, Cpu::cpx),
    Op::new(0xEC, "CPX", AddressingMode::Absolute, 4, Cpu::cpx),
    Op::new(0xC0, "CPY", AddressingMode::Immediate, 2, Cpu::cpy),
    Op::new(0xC4, "CPY", AddressingMode::ZeroPage, 3, Cpu::cpy),
    Op::new(0xCC, "CPY", AddressingMode::Absolute, 4, Cpu::cpy),
    Op::new(0xC6, "DEC", AddressingMode::ZeroPage, 5, Cpu::dec),
    Op::new(0xCE, "DEC", AddressingMode::Absolute, 6, Cpu::dec),
    Op::new(0xD6, "DEC", AddressingMode::ZeroPageX, 6, Cpu::dec),
    Op::new(0xDE, "DEC", AddressingMode::AbsoluteX, 7, Cpu::dec),
    Op::new(0xCA, "DEX", AddressingMode::None, 2, Cpu::dex),
    Op::new(0x88, "DEY", AddressingMode::None, 2, Cpu::dey),
    Op::new(0x41, "EOR", AddressingMode::IndexedIndirect, 6, Cpu::eor),
    Op::new(0x45, "EOR", AddressingMode::ZeroPage, 3, Cpu::eor),
    Op::new(0x49, "EOR", AddressingMode::Immediate, 2, Cpu::eor),
    Op::new(0x4D, "EOR", AddressingMode::Absolute, 4, Cpu::eor),
    Op::new(0x51, "EOR", AddressingMode::IndirectIndexed, 5, Cpu::eor),
    Op::new(0x55, "EOR", AddressingMode::ZeroPageX, 4, Cpu::eor),
    Op::new(0x59, "EOR", AddressingMode::AbsoluteY, 4, Cpu::eor),
    Op::new(0x5D, "EOR", AddressingMode::AbsoluteX, 4, Cpu::eor),
    Op::new(0xE6, "INC", AddressingMode::ZeroPage, 5, Cpu::inc),
    Op::new(0xEE, "INC", AddressingMode::Absolute, 6, Cpu::inc),
    Op::new(0xF6, "INC", AddressingMode::ZeroPageX, 6, Cpu::inc),
    Op::new(0xFE, "INC", AddressingMode::AbsoluteX, 7, Cpu::inc),
    Op::new(0xE8, "INX", AddressingMode::None, 2, Cpu::inx),
    Op::new(0xC8, "INY", AddressingMode::None, 2, Cpu::iny),
    Op::new(0x4C, "JMP", AddressingMode::Absolute, 3, Cpu::jmp),
    Op::new(0x6C, "JMP", AddressingMode::Indirect, 5, Cpu::jmp),
    Op::new(0x20, "JSR", AddressingMode::Absolute, 6, Cpu::jsr),
    Op::new(0xA1, "LDA", AddressingMode::IndexedIndirect, 6, Cpu::lda),
    Op::new(0xA5, "LDA", AddressingMode::ZeroPage, 3, Cpu::lda),
    Op::new(0xA9, "LDA", AddressingMode::Immediate, 2, Cpu::lda),
    Op::new(0xAD, "LDA", AddressingMode::Absolute, 4, Cpu::lda),
    Op::new(0xB1, "LDA", AddressingMode::IndirectIndexed, 5, Cpu::lda),
    Op::new(0xB5, "LDA", AddressingMode::ZeroPageX, 4, Cpu::lda),
    Op::new(0xB9, "LDA", AddressingMode::AbsoluteY, 4, Cpu::lda),
    Op::new(0xBD, "LDA", AddressingMode::AbsoluteX, 4, Cpu::lda),
    Op::new(0xA2, "LDX", AddressingMode::Immediate, 2, Cpu::ldx),
    Op::new(0xA6, "LDX", AddressingMode::ZeroPage, 3, Cpu::ldx),
    Op::new(0xAE, "LDX", AddressingMode::Absolute, 4, Cpu::ldx),
    Op::new(0xB6, "LDX", AddressingMode::ZeroPageY, 4, Cpu::ldx),
    Op::new(0xBE, "LDX", AddressingMode::AbsoluteY, 4, Cpu::ldx),
    Op::new(0xA0, "LDY", AddressingMode::Immediate, 2, Cpu::ldy),
    Op::new(0xA4, "LDY", AddressingMode::ZeroPage, 3, Cpu::ldy),
    Op::new(0xAC, "LDY", AddressingMode::Absolute, 4, Cpu::ldy),
    Op::new(0xB4, "LDY", AddressingMode::ZeroPageX, 4, Cpu::ldy),
    Op::new(0xBC, "LDY", AddressingMode::AbsoluteX, 4, Cpu::ldy),
    Op::new(0x46, "LSR", AddressingMode::ZeroPage, 5, Cpu::lsr),
    Op::new(0x4A, "LSR", AddressingMode::None, 2, Cpu::lsr),
    Op::new(0x4E, "LSR", AddressingMode::Absolute, 6, Cpu::lsr),
    Op::new(0x56, "LSR", AddressingMode::ZeroPageX, 6, Cpu::lsr),
    Op::new(0x5E, "LSR", AddressingMode::AbsoluteX, 7, Cpu::lsr),
    Op::new(0xEA, "NOP", AddressingMode::None, 2, Cpu::nop),
    Op::new(0x01, "ORA", AddressingMode::IndexedIndirect, 6, Cpu::ora),
    Op::new(0x05, "ORA", AddressingMode::ZeroPage, 3, Cpu::ora),
    Op::new(0x09, "ORA", AddressingMode::Immediate, 2, Cpu::ora),
    Op::new(0x0D, "ORA", AddressingMode::Absolute, 4, Cpu::ora),
    Op::new(0x11, "ORA", AddressingMode::IndirectIndexed, 5, Cpu::ora),
    Op::new(0x15, "ORA", AddressingMode::ZeroPageX, 4, Cpu::ora),
    Op::new(0x19, "ORA", AddressingMode::AbsoluteY, 4, Cpu::ora),
    Op::new(0x1D, "ORA", AddressingMode::AbsoluteX, 4, Cpu::ora),
    Op::new(0x48, "PHA", AddressingMode::None, 3, Cpu::pha),
    Op::new(0x08, "PHP", AddressingMode::None, 3, Cpu::php),
    Op::new(0x68, "PLA", AddressingMode::None, 4, Cpu::pla),
    Op::new(0x28, "PLP", AddressingMode::None, 4, Cpu::plp),
    Op::new(0x26, "ROL", AddressingMode::ZeroPage, 5, Cpu::rol),
    Op::new(0x2A, "ROL", AddressingMode::None, 2, Cpu::rol),
    Op::new(0x2E, "ROL", AddressingMode::Absolute, 6, Cpu::rol),
    Op::new(0x36, "ROL", AddressingMode::ZeroPageX, 6, Cpu::rol),
    Op::new(0x3E, "ROL", AddressingMode::AbsoluteX, 7, Cpu::rol),
    Op::new(0x66, "ROR", AddressingMode::ZeroPage, 5, Cpu::ror),
    Op::new(0x6A, "ROR", AddressingMode::None, 2, Cpu::ror),
    Op::new(0x6E, "ROR", AddressingMode::Absolute, 6, Cpu::ror),
    Op::new(0x76, "ROR", AddressingMode::ZeroPageX, 6, Cpu::ror),
    Op::new(0x7E, "ROR", AddressingMode::AbsoluteX, 7, Cpu::ror),
    Op::new(0x40, "RTI", AddressingMode::None, 6, Cpu::rti),
    Op::new(0x60, "RTS", AddressingMode::None, 6, Cpu::rts),
    Op::new(0xE1, "SBC", AddressingMode::IndexedIndirect, 6, Cpu::sbc),
    Op::new(0xE5, "SBC", AddressingMode::ZeroPage, 3, Cpu::sbc),
    Op::new(0xE9, "SBC", AddressingMode::Immediate, 2, Cpu::sbc),
    Op::new(0xED, "SBC", AddressingMode::Absolute, 4, Cpu::sbc),
    Op::new(0xF1, "SBC", AddressingMode::IndirectIndexed, 5, Cpu::sbc),
    Op::new(0xF5, "SBC", AddressingMode::ZeroPageX, 4, Cpu::sbc),
    Op::new(0xF9, "SBC", AddressingMode::AbsoluteY, 4, Cpu::sbc),
    Op::new(0xFD, "SBC", AddressingMode::AbsoluteX, 4, Cpu::sbc),
    Op::new(0x38, "SEC", AddressingMode::None, 2, Cpu::sec),
    Op::new(0xF8, "SED", AddressingMode::None, 2, Cpu::sed),
    Op::new(0x78, "SEI", AddressingMode::None, 2, Cpu::sei),
    Op::new(0x81, "STA", AddressingMode::IndexedIndirect, 6, Cpu::sta),
    Op::new(0x85, "STA", AddressingMode::ZeroPage, 3, Cpu::sta),
    Op::new(0x8D, "STA", AddressingMode::Absolute, 4, Cpu::sta),
    Op::new(0x91, "STA", AddressingMode::IndirectIndexed, 6, Cpu::sta),
    Op::new(0x95, "STA", AddressingMode::ZeroPageX, 4, Cpu::sta),
    Op::new(0x99, "STA", AddressingMode::AbsoluteY, 5, Cpu::sta),
    Op::new(0x9D, "STA", AddressingMode::AbsoluteX, 5, Cpu::sta),
    Op::new(0x86, "STX", AddressingMode::ZeroPage, 3, Cpu::stx),
    Op::new(0x8E, "STX", AddressingMode::Absolute, 4, Cpu::stx),
    Op::new(0x96, "STX", AddressingMode::ZeroPageY, 4, Cpu::stx),
    Op::new(0x84, "STY", AddressingMode::ZeroPage, 3, Cpu::sty),
    Op::new(0x8C, "STY", AddressingMode::Absolute, 4, Cpu::sty),
    Op::new(0x94, "STY", AddressingMode::ZeroPageX, 4, Cpu::sty),
    Op::new(0xAA, "TAX", AddressingMode::None, 2, Cpu::tax),
    Op::new(0xA8, "TAY", AddressingMode::None, 2, Cpu::tay),
    Op::new(0xBA, "TSX", AddressingMode::None, 2, Cpu::tsx),
    Op::new(0x8A, "TXA", AddressingMode::None, 2, Cpu::txa),
    Op::new(0x9A, "TXS", AddressingMode::None, 2, Cpu::txs),
    Op::new(0x98, "TYA", AddressingMode::None, 2, Cpu::tya),
    // Invalid opcode for testing
    Op::new(0xFF, "IVD", AddressingMode::None, 2, Cpu::ivd),
];

pub struct Cpu {
    pub op_table: Vec<Option<&'static Op>>,

    pub reg_a: u8,
    pub reg_x: u8,
    pub reg_y: u8,
    pub status: Status,
    pub pc: u16,

    pub mem: Memory,
}

impl Cpu {
    pub fn new() -> Self {
        let mut op_table = vec![None; 256];

        for op in CPU_OPS {
            op_table[op.code as usize] = Some(op);
        }

        Cpu {
            op_table,

            reg_a: 0,
            reg_x: 0,
            reg_y: 0,
            status: Status::empty(),
            pc: 0,

            mem: Memory::new(),
        }
    }

    pub fn reset(&mut self) {
        self.reg_a = 0;
        self.reg_x = 0;
        self.reg_y = 0;
        self.status = Status::empty();
        self.pc = self.mem.read_u16(0xfffc);
    }

    pub fn load(&mut self, program: &[u8]) {
        let start_address = 0x8000; // Starting address for now
        self.mem.load(start_address, program);
        self.mem.write_u16(0xfffc, start_address);
    }

    pub fn run(&mut self) {
        while !self.status.contains(Status::BREAK_COMMAND) {
            let op_code = self.read_pc_next();
            let Some(op) = self.op_table[op_code as usize] else {
                panic!("Invalid opcode {op_code:x} at PC {:#06x}", self.pc - 1);
            };

            (op.handler)(self, op);
        }
    }

    fn pc_next(&mut self) -> u16 {
        let pc = self.pc;
        self.pc += 1;
        pc
    }

    fn read_pc_next(&mut self) -> u8 {
        let pc = self.pc_next();

        self.mem.read(pc)
    }

    fn read_pc_u16_next(&mut self) -> u16 {
        // Increment PC twice to read two bytes
        let pc = self.pc_next();
        let _ = self.pc_next();

        self.mem.read_u16(pc)
    }

    fn operand_addr_next(&mut self, mode: AddressingMode) -> Option<u16> {
        match mode {
            AddressingMode::Immediate => Some(self.pc_next()),
            AddressingMode::ZeroPage => Some(u16::from(self.read_pc_next())),
            AddressingMode::ZeroPageX => {
                let addr = self.read_pc_next();
                Some(u16::from(addr.wrapping_add(self.reg_x)))
            }
            AddressingMode::ZeroPageY => {
                let addr = self.read_pc_next();
                Some(u16::from(addr.wrapping_add(self.reg_y)))
            }
            AddressingMode::Absolute => Some(self.read_pc_u16_next()),
            AddressingMode::AbsoluteX => {
                let addr = self.read_pc_u16_next();
                Some(addr.wrapping_add(u16::from(self.reg_x)))
            }
            AddressingMode::AbsoluteY => {
                let addr = self.read_pc_u16_next();
                Some(addr.wrapping_add(u16::from(self.reg_y)))
            }
            AddressingMode::Relative => {
                let offset = self.read_pc_next();
                Some(self.pc.wrapping_add(u16::from(offset)))
            }
            AddressingMode::Indirect => {
                let addr = self.read_pc_u16_next();
                Some(self.mem.read_u16(addr))
            }
            AddressingMode::IndexedIndirect => {
                let base = self.read_pc_next();
                let offsetted = base.wrapping_add(self.reg_x);
                Some(self.mem.read_u16(u16::from(offsetted)))
            }
            AddressingMode::IndirectIndexed => {
                let base = self.read_pc_next();
                let addr = self.mem.read_u16(u16::from(base));

                Some(addr.wrapping_add(u16::from(self.reg_y)))
            }
            AddressingMode::None => None,
        }
    }

    fn adc(&mut self, op: &'static Op) {
        let addr = self
            .operand_addr_next(op.mode)
            .expect("ADC requires an address operand");
        let reg_a = self.reg_a;
        let value = self.mem.read(addr);
        let (result, carry) = self.reg_a.overflowing_add(value);
        self.reg_a = result;

        self.status.set(Status::CARRY, carry);
        self.status.set(Status::ZERO, result == 0);
        self.status.set(
            Status::OVERFLOW,
            result & SIGN_BIT != 0 && reg_a.max(value) < SIGN_BIT,
        );
        self.status.set(Status::NEGATIVE, result & SIGN_BIT != 0);
    }

    fn and(&mut self, op: &'static Op) {
        let addr = self
            .operand_addr_next(op.mode)
            .expect("AND requires an address operand");
        let reg_a = self.reg_a;
        let value = self.mem.read(addr);
        let result = reg_a & value;
        self.reg_a = result;

        self.status.set(Status::ZERO, result == 0);
        self.status.set(Status::NEGATIVE, result & SIGN_BIT != 0);
    }

    fn asl(&mut self, op: &'static Op) {
        let addr = self.operand_addr_next(op.mode);
        let value = if let Some(addr) = addr {
            self.mem.read(addr)
        } else {
            self.reg_a
        };

        let (result, carry) = value.overflowing_shl(1);
        if let Some(addr) = addr {
            self.mem.write(addr, result);
        } else {
            self.reg_a = result;
        }

        self.status.set(Status::CARRY, carry);
        self.status.set(Status::ZERO, result == 0);
        self.status.set(Status::NEGATIVE, result & SIGN_BIT != 0);
    }

    fn bcc(&mut self, op: &'static Op) {
        let addr = self
            .operand_addr_next(op.mode)
            .expect("BCC requires an address operand");
        if !self.status.contains(Status::CARRY) {
            self.pc = addr;
        }
    }

    fn bcs(&mut self, op: &'static Op) {
        let addr = self
            .operand_addr_next(op.mode)
            .expect("BCS requires an address operand");
        if self.status.contains(Status::CARRY) {
            self.pc = addr;
        }
    }

    fn beq(&mut self, op: &'static Op) {
        let addr = self
            .operand_addr_next(op.mode)
            .expect("BEQ requires an address operand");
        if self.status.contains(Status::ZERO) {
            self.pc = addr;
        }
    }

    fn bit(&mut self, op: &'static Op) {
        let addr = self
            .operand_addr_next(op.mode)
            .expect("BIT requires an address operand");
        let value = self.mem.read(addr);
        let mask = self.reg_a;
        let result = value & mask;

        self.status.set(Status::ZERO, result == 0);
        self.status.set(Status::OVERFLOW, value & 0b0100_0000 != 0);
        self.status.set(Status::NEGATIVE, value & SIGN_BIT != 0);
    }

    fn bmi(&mut self, op: &'static Op) {
        let addr = self
            .operand_addr_next(op.mode)
            .expect("BMI requires an address operand");
        if self.status.contains(Status::NEGATIVE) {
            self.pc = addr;
        }
    }

    fn bne(&mut self, _op: &'static Op) {
        todo!("op {:?} not yet implemented", _op.name)
    }

    fn bpl(&mut self, _op: &'static Op) {
        todo!("op {:?} not yet implemented", _op.name)
    }

    fn brk(&mut self, _op: &'static Op) {
        self.status.insert(Status::BREAK_COMMAND);
    }

    fn bvc(&mut self, _op: &'static Op) {
        todo!("op {:?} not yet implemented", _op.name)
    }

    fn bvs(&mut self, _op: &'static Op) {
        todo!("op {:?} not yet implemented", _op.name)
    }

    fn clc(&mut self, _op: &'static Op) {
        todo!("op {:?} not yet implemented", _op.name)
    }

    fn cld(&mut self, _op: &'static Op) {
        todo!("op {:?} not yet implemented", _op.name)
    }

    fn cli(&mut self, _op: &'static Op) {
        todo!("op {:?} not yet implemented", _op.name)
    }

    fn clv(&mut self, _op: &'static Op) {
        todo!("op {:?} not yet implemented", _op.name)
    }

    fn cmp(&mut self, _op: &'static Op) {
        todo!("op {:?} not yet implemented", _op.name)
    }

    fn cpx(&mut self, _op: &'static Op) {
        todo!("op {:?} not yet implemented", _op.name)
    }

    fn cpy(&mut self, _op: &'static Op) {
        todo!("op {:?} not yet implemented", _op.name)
    }

    fn dec(&mut self, _op: &'static Op) {
        todo!("op {:?} not yet implemented", _op.name)
    }

    fn dex(&mut self, _op: &'static Op) {
        todo!("op {:?} not yet implemented", _op.name)
    }

    fn dey(&mut self, _op: &'static Op) {
        todo!("op {:?} not yet implemented", _op.name)
    }

    fn eor(&mut self, _op: &'static Op) {
        todo!("op {:?} not yet implemented", _op.name)
    }

    fn inc(&mut self, _op: &'static Op) {
        todo!("op {:?} not yet implemented", _op.name)
    }

    fn inx(&mut self, _op: &'static Op) {
        self.reg_x = self.reg_x.wrapping_add(1);
        self.update_status(self.reg_x);
    }

    fn iny(&mut self, _op: &'static Op) {
        todo!("op {:?} not yet implemented", _op.name)
    }

    fn jmp(&mut self, _op: &'static Op) {
        todo!("op {:?} not yet implemented", _op.name)
    }

    fn jsr(&mut self, _op: &'static Op) {
        todo!("op {:?} not yet implemented", _op.name)
    }

    fn lda(&mut self, op: &'static Op) {
        let addr = self
            .operand_addr_next(op.mode)
            .expect("LDA requires an address operand");
        self.reg_a = self.mem.read(addr);
        self.update_status(self.reg_a);
    }

    fn ldx(&mut self, _op: &'static Op) {
        todo!("op {:?} not yet implemented", _op.name)
    }

    fn ldy(&mut self, _op: &'static Op) {
        todo!("op {:?} not yet implemented", _op.name)
    }

    fn lsr(&mut self, _op: &'static Op) {
        todo!("op {:?} not yet implemented", _op.name)
    }

    fn nop(&mut self, _op: &'static Op) {
        todo!("op {:?} not yet implemented", _op.name)
    }

    fn ora(&mut self, _op: &'static Op) {
        todo!("op {:?} not yet implemented", _op.name)
    }

    fn pha(&mut self, _op: &'static Op) {
        todo!("op {:?} not yet implemented", _op.name)
    }

    fn php(&mut self, _op: &'static Op) {
        todo!("op {:?} not yet implemented", _op.name)
    }

    fn pla(&mut self, _op: &'static Op) {
        todo!("op {:?} not yet implemented", _op.name)
    }

    fn plp(&mut self, _op: &'static Op) {
        todo!("op {:?} not yet implemented", _op.name)
    }

    fn rol(&mut self, _op: &'static Op) {
        todo!("op {:?} not yet implemented", _op.name)
    }

    fn ror(&mut self, _op: &'static Op) {
        todo!("op {:?} not yet implemented", _op.name)
    }

    fn rti(&mut self, _op: &'static Op) {
        todo!("op {:?} not yet implemented", _op.name)
    }

    fn rts(&mut self, _op: &'static Op) {
        todo!("op {:?} not yet implemented", _op.name)
    }

    fn sbc(&mut self, _op: &'static Op) {
        todo!("op {:?} not yet implemented", _op.name)
    }

    fn sec(&mut self, _op: &'static Op) {
        todo!("op {:?} not yet implemented", _op.name)
    }

    fn sed(&mut self, _op: &'static Op) {
        todo!("op {:?} not yet implemented", _op.name)
    }

    fn sei(&mut self, _op: &'static Op) {
        todo!("op {:?} not yet implemented", _op.name)
    }

    fn sta(&mut self, op: &'static Op) {
        let addr = self
            .operand_addr_next(op.mode)
            .expect("STA requires an address operand");
        self.mem.write(addr, self.reg_a);
    }

    fn stx(&mut self, _op: &'static Op) {
        todo!("op {:?} not yet implemented", _op.name)
    }

    fn sty(&mut self, _op: &'static Op) {
        todo!("op {:?} not yet implemented", _op.name)
    }

    fn tax(&mut self, _op: &'static Op) {
        self.reg_x = self.reg_a;
        self.update_status(self.reg_x);
    }

    fn tay(&mut self, _op: &'static Op) {
        todo!("op {:?} not yet implemented", _op.name)
    }

    fn tsx(&mut self, _op: &'static Op) {
        todo!("op {:?} not yet implemented", _op.name)
    }

    fn txa(&mut self, _op: &'static Op) {
        todo!("op {:?} not yet implemented", _op.name)
    }

    fn txs(&mut self, _op: &'static Op) {
        todo!("op {:?} not yet implemented", _op.name)
    }

    fn tya(&mut self, _op: &'static Op) {
        todo!("op {:?} not yet implemented", _op.name)
    }

    fn ivd(&mut self, _op: &'static Op) {
        panic!("Invalid opcode encountered");
    }

    fn update_status(&mut self, result: u8) {
        // zero
        self.status.set(Status::ZERO, result == 0);

        // negative
        self.status.set(Status::NEGATIVE, result & SIGN_BIT != 0);
    }
}

impl Default for Cpu {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_0xa9_lda_immediate_load_data() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xa9, 0x05, 0x00]);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.reg_a, 0x05);
        assert!(!cpu.status.contains(Status::ZERO));
        assert!(!cpu.status.contains(Status::NEGATIVE));
    }

    #[test]
    fn test_0xa9_lda_zero_flag() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xa9, 0x00, 0x00]);
        cpu.reset();
        cpu.run();

        assert!(cpu.status.contains(Status::ZERO));
    }

    #[test]
    fn test_0xaa_tax_move_a_to_x() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xaa, 0x00]);
        cpu.reset();
        cpu.reg_a = 10;
        cpu.run();

        assert_eq!(cpu.reg_x, 10)
    }

    #[test]
    fn test_5_ops_working_together() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xa9, 0xc0, 0xaa, 0xe8, 0x00]);
        cpu.reset();
        cpu.reg_a = 0xc0;
        cpu.run();

        assert_eq!(cpu.reg_x, 0xc1)
    }

    #[test]
    fn test_inx_overflow() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xe8, 0xe8, 0x00]);
        cpu.reset();
        cpu.reg_x = 0xff;
        cpu.run();

        assert_eq!(cpu.reg_x, 1)
    }

    #[test]
    fn test_lda_from_memory() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xa5, 0x10, 0x00]);
        cpu.mem.write(0x10, 0x55);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.reg_a, 0x55);
    }

    // ===== ADC (Add with Carry) Tests =====

    #[test]
    fn test_0x69_adc_immediate() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x69, 0x50, 0x00]);
        cpu.reset();
        cpu.reg_a = 0x30;
        cpu.run();

        assert_eq!(cpu.reg_a, 0x80);
    }

    #[test]
    fn test_0x69_adc_immediate_with_zero_result() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x69, 0x00, 0x00]);
        cpu.reset();
        cpu.reg_a = 0x00;
        cpu.run();

        assert!(cpu.status.contains(Status::ZERO));
    }

    #[test]
    fn test_0x65_adc_zero_page() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x65, 0x10, 0x00]);
        cpu.mem.write(0x10, 0x25);
        cpu.reset();
        cpu.reg_a = 0x25;
        cpu.run();

        assert_eq!(cpu.reg_a, 0x4a);
    }

    #[test]
    fn test_0x6d_adc_absolute() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x6d, 0x20, 0x80, 0x00]);
        cpu.mem.write(0x8020, 0x40);
        cpu.reset();
        cpu.reg_a = 0x10;
        cpu.run();

        assert_eq!(cpu.reg_a, 0x50);
    }

    // ===== AND (Logical AND) Tests =====

    #[test]
    fn test_0x29_and_immediate() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x29, 0x0f, 0x00]);
        cpu.reset();
        cpu.reg_a = 0xf0;
        cpu.run();

        assert_eq!(cpu.reg_a, 0x00);
        assert!(cpu.status.contains(Status::ZERO));
    }

    #[test]
    fn test_0x25_and_zero_page() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x25, 0x10, 0x00]);
        cpu.mem.write(0x10, 0x0f);
        cpu.reset();
        cpu.reg_a = 0xf5;
        cpu.run();

        assert_eq!(cpu.reg_a, 0x05);
    }

    #[test]
    fn test_0x2d_and_absolute() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x2d, 0x20, 0x80, 0x00]);
        cpu.mem.write(0x8020, 0xff);
        cpu.reset();
        cpu.reg_a = 0x55;
        cpu.run();

        assert_eq!(cpu.reg_a, 0x55);
    }

    // ===== ASL (Arithmetic Shift Left) Tests =====

    #[test]
    fn test_0x0a_asl_accumulator() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x0a, 0x00]);
        cpu.reset();
        cpu.reg_a = 0x02;
        cpu.run();

        assert_eq!(cpu.reg_a, 0x04);
    }

    #[test]
    fn test_0x06_asl_zero_page() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x06, 0x10, 0x00]);
        cpu.mem.write(0x10, 0x40);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.mem.read(0x10), 0x80);
    }

    #[test]
    fn test_0x0e_asl_absolute() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x0e, 0x20, 0x80, 0x00]);
        cpu.mem.write(0x8020, 0x01);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.mem.read(0x8020), 0x02);
    }

    // ===== Branch Instructions Tests =====

    // BEQ (Branch if Equal - ZERO flag set)
    #[test]
    fn test_0xf0_beq_branch_taken() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xf0, 0x02, 0xff, 0xff, 0x00]);
        cpu.reset();
        cpu.status.insert(Status::ZERO);
        cpu.run();

        // If branch is not taken by bug, CPU will panic due to unknown opcode 0xff
    }

    #[test]
    fn test_0xf0_beq_branch_not_taken() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xf0, 0x02, 0x00]);
        cpu.reset();
        cpu.status = Status::empty();
        cpu.run();

        assert!(!cpu.status.contains(Status::ZERO));
    }

    // BNE (Branch if Not Equal - ZERO flag clear)
    #[test]
    fn test_0xd0_bne_branch_taken() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xd0, 0x02, 0xff, 0xff, 0x00]);
        cpu.reset();
        cpu.status = Status::empty();
        cpu.run();

        // If branch is not taken by bug, CPU will panic due to unknown opcode 0xff
    }

    #[test]
    fn test_0xd0_bne_branch_not_taken() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xd0, 0x02, 0x00]);
        cpu.reset();
        cpu.status.insert(Status::ZERO);
        cpu.run();

        assert!(cpu.status.contains(Status::ZERO));
    }

    // BCC (Branch if Carry Clear - CARRY flag clear)
    #[test]
    fn test_0x90_bcc_branch_taken() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x90, 0x02, 0xff, 0xff, 0x00]);
        cpu.reset();
        cpu.status = Status::empty();
        cpu.run();

        // If branch is not taken by bug, CPU will panic due to unknown opcode 0xff
    }

    #[test]
    fn test_0x90_bcc_branch_not_taken() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x90, 0x02, 0x00]);
        cpu.reset();
        cpu.status.insert(Status::CARRY);
        cpu.run();

        assert!(cpu.status.contains(Status::CARRY));
    }

    // BCS (Branch if Carry Set - CARRY flag set)
    #[test]
    fn test_0xb0_bcs_branch_taken() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xb0, 0x02, 0xff, 0xff, 0x00]);
        cpu.reset();
        cpu.status.insert(Status::CARRY);
        cpu.run();

        // If branch is not taken by bug, CPU will panic due to unknown opcode 0xff
    }

    #[test]
    fn test_0xb0_bcs_branch_not_taken() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xb0, 0x02, 0x00]);
        cpu.reset();
        cpu.status = Status::empty();
        cpu.run();
    }

    // BMI (Branch if Minus - NEGATIVE flag set)
    #[test]
    fn test_0x30_bmi_branch_taken() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x30, 0x02, 0xff, 0xff, 0x00]);
        cpu.reset();
        cpu.status.insert(Status::NEGATIVE);
        cpu.run();

        // If branch is not taken by bug, CPU will panic due to unknown opcode 0xff
    }

    #[test]
    fn test_0x30_bmi_branch_not_taken() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x30, 0x02, 0x00]);
        cpu.reset();
        cpu.status = Status::empty();
        cpu.run();
    }

    // BPL (Branch if Plus - NEGATIVE flag clear)
    #[test]
    fn test_0x10_bpl_branch_taken() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x10, 0x02, 0xff, 0xff, 0x00]);
        cpu.reset();
        cpu.status = Status::empty();
        cpu.run();

        // If branch is not taken by bug, CPU will panic due to unknown opcode 0xff
    }

    #[test]
    fn test_0x10_bpl_branch_not_taken() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x10, 0x02, 0x00]);
        cpu.reset();
        cpu.status.insert(Status::NEGATIVE);
        cpu.run();

        assert!(cpu.status.contains(Status::NEGATIVE));
    }

    // BVC (Branch if Overflow Clear - OVERFLOW flag clear)
    #[test]
    fn test_0x50_bvc_branch_taken() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x50, 0x02, 0xff, 0xff, 0x00]);
        cpu.reset();
        cpu.status = Status::empty();
        cpu.run();

        // If branch is not taken by bug, CPU will panic due to unknown opcode 0xff
    }

    #[test]
    fn test_0x50_bvc_branch_not_taken() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x50, 0x02, 0x00]);
        cpu.reset();
        cpu.status.insert(Status::OVERFLOW);
        cpu.run();

        assert!(cpu.status.contains(Status::OVERFLOW));
    }

    // BVS (Branch if Overflow Set - OVERFLOW flag set)
    #[test]
    fn test_0x70_bvs_branch_taken() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x70, 0x02, 0xff, 0xff, 0x00]);
        cpu.reset();
        cpu.status.insert(Status::OVERFLOW);
        cpu.run();

        // If branch is not taken by bug, CPU will panic due to unknown opcode 0xff
    }

    #[test]
    fn test_0x70_bvs_branch_not_taken() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x70, 0x02, 0x00]);
        cpu.reset();
        cpu.status = Status::empty();
        cpu.run();
    }

    // ===== BIT Test =====

    #[test]
    fn test_0x24_bit_zero_page() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x24, 0x10, 0x00]);
        cpu.mem.write(0x10, 0xc0);
        cpu.reset();
        cpu.reg_a = 0x3f;
        cpu.run();

        assert!(cpu.status.contains(Status::ZERO));
        assert!(cpu.status.contains(Status::NEGATIVE));
    }

    #[test]
    fn test_0x2c_bit_absolute() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x2c, 0x20, 0x80, 0x00]);
        cpu.mem.write(0x8020, 0x80);
        cpu.reset();
        cpu.reg_a = 0x01;
        cpu.run();

        assert!(cpu.status.contains(Status::ZERO));
        assert!(cpu.status.contains(Status::NEGATIVE));
    }

    // ===== BRK Test =====

    #[test]
    fn test_0x00_brk() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x00]);
        cpu.reset();
        cpu.run();
    }

    // ===== CMP (Compare) Tests =====

    #[test]
    fn test_0xc9_cmp_immediate_equal() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xc9, 0x50, 0x00]);
        cpu.reset();
        cpu.reg_a = 0x50;
        cpu.run();

        assert!(cpu.status.contains(Status::ZERO));
    }

    #[test]
    fn test_0xc5_cmp_zero_page() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xc5, 0x10, 0x00]);
        cpu.mem.write(0x10, 0x30);
        cpu.reset();
        cpu.reg_a = 0x40;
        cpu.run();
    }

    #[test]
    fn test_0xcd_cmp_absolute() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xcd, 0x20, 0x80, 0x00]);
        cpu.mem.write(0x8020, 0x80);
        cpu.reset();
        cpu.reg_a = 0x80;
        cpu.run();

        assert!(cpu.status.contains(Status::ZERO));
    }

    // ===== CPX (Compare X) Tests =====

    #[test]
    fn test_0xe0_cpx_immediate_equal() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xe0, 0x40, 0x00]);
        cpu.reset();
        cpu.reg_x = 0x40;
        cpu.run();

        assert!(cpu.status.contains(Status::ZERO));
    }

    #[test]
    fn test_0xe4_cpx_zero_page() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xe4, 0x10, 0x00]);
        cpu.mem.write(0x10, 0x50);
        cpu.reset();
        cpu.reg_x = 0x50;
        cpu.run();

        assert!(cpu.status.contains(Status::ZERO));
    }

    #[test]
    fn test_0xec_cpx_absolute() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xec, 0x20, 0x80, 0x00]);
        cpu.mem.write(0x8020, 0x60);
        cpu.reset();
        cpu.reg_x = 0x60;
        cpu.run();

        assert!(cpu.status.contains(Status::ZERO));
    }

    // ===== CPY (Compare Y) Tests =====

    #[test]
    fn test_0xc0_cpy_immediate_equal() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xc0, 0x30, 0x00]);
        cpu.reset();
        cpu.reg_y = 0x30;
        cpu.run();

        assert!(cpu.status.contains(Status::ZERO));
    }

    #[test]
    fn test_0xc4_cpy_zero_page() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xc4, 0x10, 0x00]);
        cpu.mem.write(0x10, 0x70);
        cpu.reset();
        cpu.reg_y = 0x70;
        cpu.run();

        assert!(cpu.status.contains(Status::ZERO));
    }

    #[test]
    fn test_0xcc_cpy_absolute() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xcc, 0x20, 0x80, 0x00]);
        cpu.mem.write(0x8020, 0x90);
        cpu.reset();
        cpu.reg_y = 0x90;
        cpu.run();

        assert!(cpu.status.contains(Status::ZERO));
    }

    // ===== DEC (Decrement) Tests =====

    #[test]
    fn test_0xc6_dec_zero_page() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xc6, 0x10, 0x00]);
        cpu.mem.write(0x10, 0x10);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.mem.read(0x10), 0x0f);
    }

    #[test]
    fn test_0xce_dec_absolute() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xce, 0x20, 0x80, 0x00]);
        cpu.mem.write(0x8020, 0x01);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.mem.read(0x8020), 0x00);
        assert!(cpu.status.contains(Status::ZERO));
    }

    #[test]
    fn test_0xc6_dec_zero_page_underflow() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xc6, 0x10, 0x00]);
        cpu.mem.write(0x10, 0x00);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.mem.read(0x10), 0xff);
        assert!(cpu.status.contains(Status::NEGATIVE));
    }

    // ===== DEX (Decrement X) Tests =====

    #[test]
    fn test_0xca_dex() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xca, 0x00]);
        cpu.reset();
        cpu.reg_x = 0x10;
        cpu.run();

        assert_eq!(cpu.reg_x, 0x0f);
    }

    #[test]
    fn test_0xca_dex_underflow() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xca, 0x00]);
        cpu.reset();
        cpu.reg_x = 0x00;
        cpu.run();

        assert_eq!(cpu.reg_x, 0xff);
        assert!(cpu.status.contains(Status::NEGATIVE));
    }

    // ===== DEY (Decrement Y) Tests =====

    #[test]
    fn test_0x88_dey() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x88, 0x00]);
        cpu.reset();
        cpu.reg_y = 0x20;
        cpu.run();

        assert_eq!(cpu.reg_y, 0x1f);
    }

    #[test]
    fn test_0x88_dey_underflow() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x88, 0x00]);
        cpu.reset();
        cpu.reg_y = 0x00;
        cpu.run();

        assert_eq!(cpu.reg_y, 0xff);
        assert!(cpu.status.contains(Status::NEGATIVE));
    }

    // ===== EOR (Exclusive OR) Tests =====

    #[test]
    fn test_0x49_eor_immediate() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x49, 0x0f, 0x00]);
        cpu.reset();
        cpu.reg_a = 0xf0;
        cpu.run();

        assert_eq!(cpu.reg_a, 0xff);
    }

    #[test]
    fn test_0x45_eor_zero_page() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x45, 0x10, 0x00]);
        cpu.mem.write(0x10, 0x55);
        cpu.reset();
        cpu.reg_a = 0xaa;
        cpu.run();

        assert_eq!(cpu.reg_a, 0xff);
    }

    #[test]
    fn test_0x4d_eor_absolute() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x4d, 0x20, 0x80, 0x00]);
        cpu.mem.write(0x8020, 0xff);
        cpu.reset();
        cpu.reg_a = 0x00;
        cpu.run();

        assert_eq!(cpu.reg_a, 0xff);
    }

    // ===== INC (Increment) Tests =====

    #[test]
    fn test_0xe6_inc_zero_page() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xe6, 0x10, 0x00]);
        cpu.mem.write(0x10, 0x0f);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.mem.read(0x10), 0x10);
    }

    #[test]
    fn test_0xee_inc_absolute() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xee, 0x20, 0x80, 0x00]);
        cpu.mem.write(0x8020, 0xff);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.mem.read(0x8020), 0x00);
        assert!(cpu.status.contains(Status::ZERO));
    }

    #[test]
    fn test_0xe6_inc_zero_page_to_negative() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xe6, 0x10, 0x00]);
        cpu.mem.write(0x10, 0x7f);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.mem.read(0x10), 0x80);
        assert!(cpu.status.contains(Status::NEGATIVE));
    }

    // ===== INX (Increment X) Tests =====

    #[test]
    fn test_0xe8_inx() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xe8, 0x00]);
        cpu.reset();
        cpu.reg_x = 0x20;
        cpu.run();

        assert_eq!(cpu.reg_x, 0x21);
    }

    #[test]
    fn test_0xe8_inx_to_negative() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xe8, 0x00]);
        cpu.reset();
        cpu.reg_x = 0x7f;
        cpu.run();

        assert_eq!(cpu.reg_x, 0x80);
        assert!(cpu.status.contains(Status::NEGATIVE));
    }

    // ===== INY (Increment Y) Tests =====

    #[test]
    fn test_0xc8_iny() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xc8, 0x00]);
        cpu.reset();
        cpu.reg_y = 0x30;
        cpu.run();

        assert_eq!(cpu.reg_y, 0x31);
    }

    #[test]
    fn test_0xc8_iny_to_negative() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xc8, 0x00]);
        cpu.reset();
        cpu.reg_y = 0x7f;
        cpu.run();

        assert_eq!(cpu.reg_y, 0x80);
        assert!(cpu.status.contains(Status::NEGATIVE));
    }

    // ===== JMP (Jump) Tests =====

    #[test]
    fn test_0x4c_jmp_absolute() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x4c, 0x20, 0x80, 0x00]);
        cpu.reset();
        let pc_before = cpu.pc;
        cpu.run();
    }

    #[test]
    fn test_0x6c_jmp_indirect() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x6c, 0x10, 0x00, 0x00]);
        cpu.mem.write_u16(0x10, 0x8000);
        cpu.reset();
        cpu.run();
    }

    // ===== JSR (Jump to Subroutine) Tests =====

    #[test]
    fn test_0x20_jsr() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x20, 0x20, 0x80, 0x00]);
        cpu.reset();
        cpu.run();
    }

    // ===== LDA (Load Accumulator) Tests =====

    #[test]
    fn test_0xa5_lda_zero_page() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xa5, 0x10, 0x00]);
        cpu.mem.write(0x10, 0x42);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.reg_a, 0x42);
    }

    #[test]
    fn test_0xad_lda_absolute() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xad, 0x20, 0x80, 0x00]);
        cpu.mem.write(0x8020, 0x55);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.reg_a, 0x55);
    }

    #[test]
    fn test_0xb5_lda_zero_page_x() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xb5, 0x10, 0x00]);
        cpu.mem.write(0x15, 0x77);
        cpu.reset();
        cpu.reg_x = 0x05;
        cpu.run();

        assert_eq!(cpu.reg_a, 0x77);
    }

    #[test]
    fn test_0xbd_lda_absolute_x() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xbd, 0x20, 0x80, 0x00]);
        cpu.mem.write(0x8025, 0xaa);
        cpu.reset();
        cpu.reg_x = 0x05;
        cpu.run();

        assert_eq!(cpu.reg_a, 0xaa);
    }

    #[test]
    fn test_0xb9_lda_absolute_y() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xb9, 0x20, 0x80, 0x00]);
        cpu.mem.write(0x8030, 0xbb);
        cpu.reset();
        cpu.reg_y = 0x10;
        cpu.run();

        assert_eq!(cpu.reg_a, 0xbb);
    }

    // ===== LDX (Load X) Tests =====

    #[test]
    fn test_0xa2_ldx_immediate() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xa2, 0x44, 0x00]);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.reg_x, 0x44);
    }

    #[test]
    fn test_0xa6_ldx_zero_page() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xa6, 0x10, 0x00]);
        cpu.mem.write(0x10, 0x66);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.reg_x, 0x66);
    }

    #[test]
    fn test_0xae_ldx_absolute() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xae, 0x20, 0x80, 0x00]);
        cpu.mem.write(0x8020, 0x88);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.reg_x, 0x88);
    }

    #[test]
    fn test_0xb6_ldx_zero_page_y() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xb6, 0x10, 0x00]);
        cpu.mem.write(0x15, 0xcc);
        cpu.reset();
        cpu.reg_y = 0x05;
        cpu.run();

        assert_eq!(cpu.reg_x, 0xcc);
    }

    #[test]
    fn test_0xbe_ldx_absolute_y() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xbe, 0x20, 0x80, 0x00]);
        cpu.mem.write(0x8025, 0xdd);
        cpu.reset();
        cpu.reg_y = 0x05;
        cpu.run();

        assert_eq!(cpu.reg_x, 0xdd);
    }

    // ===== LDY (Load Y) Tests =====

    #[test]
    fn test_0xa0_ldy_immediate() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xa0, 0x33, 0x00]);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.reg_y, 0x33);
    }

    #[test]
    fn test_0xa4_ldy_zero_page() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xa4, 0x10, 0x00]);
        cpu.mem.write(0x10, 0x55);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.reg_y, 0x55);
    }

    #[test]
    fn test_0xac_ldy_absolute() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xac, 0x20, 0x80, 0x00]);
        cpu.mem.write(0x8020, 0x77);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.reg_y, 0x77);
    }

    #[test]
    fn test_0xb4_ldy_zero_page_x() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xb4, 0x10, 0x00]);
        cpu.mem.write(0x15, 0x99);
        cpu.reset();
        cpu.reg_x = 0x05;
        cpu.run();

        assert_eq!(cpu.reg_y, 0x99);
    }

    #[test]
    fn test_0xbc_ldy_absolute_x() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xbc, 0x20, 0x80, 0x00]);
        cpu.mem.write(0x8030, 0xee);
        cpu.reset();
        cpu.reg_x = 0x10;
        cpu.run();

        assert_eq!(cpu.reg_y, 0xee);
    }

    // ===== LSR (Logical Shift Right) Tests =====

    #[test]
    fn test_0x4a_lsr_accumulator() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x4a, 0x00]);
        cpu.reset();
        cpu.reg_a = 0x04;
        cpu.run();

        assert_eq!(cpu.reg_a, 0x02);
    }

    #[test]
    fn test_0x46_lsr_zero_page() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x46, 0x10, 0x00]);
        cpu.mem.write(0x10, 0x80);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.mem.read(0x10), 0x40);
    }

    #[test]
    fn test_0x4e_lsr_absolute() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x4e, 0x20, 0x80, 0x00]);
        cpu.mem.write(0x8020, 0x02);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.mem.read(0x8020), 0x01);
    }

    // ===== NOP (No Operation) Tests =====

    #[test]
    fn test_0xea_nop() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xea, 0x00]);
        cpu.reset();
        let reg_a_before = cpu.reg_a;
        cpu.run();

        assert_eq!(cpu.reg_a, reg_a_before);
    }

    // ===== ORA (Logical OR) Tests =====

    #[test]
    fn test_0x09_ora_immediate() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x09, 0x0f, 0x00]);
        cpu.reset();
        cpu.reg_a = 0xf0;
        cpu.run();

        assert_eq!(cpu.reg_a, 0xff);
    }

    #[test]
    fn test_0x05_ora_zero_page() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x05, 0x10, 0x00]);
        cpu.mem.write(0x10, 0x0f);
        cpu.reset();
        cpu.reg_a = 0xf0;
        cpu.run();

        assert_eq!(cpu.reg_a, 0xff);
    }

    #[test]
    fn test_0x0d_ora_absolute() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x0d, 0x20, 0x80, 0x00]);
        cpu.mem.write(0x8020, 0x55);
        cpu.reset();
        cpu.reg_a = 0xaa;
        cpu.run();

        assert_eq!(cpu.reg_a, 0xff);
    }

    // ===== PHA (Push Accumulator) Tests =====

    #[test]
    fn test_0x48_pha() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x48, 0x00]);
        cpu.reset();
        cpu.reg_a = 0x42;
        cpu.run();
    }

    // ===== PHP (Push Processor Status) Tests =====

    #[test]
    fn test_0x08_php() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x08, 0x00]);
        cpu.reset();
        cpu.status = Status::ZERO | Status::NEGATIVE;
        cpu.run();
    }

    // ===== PLA (Pull Accumulator) Tests =====

    #[test]
    fn test_0x68_pla() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x68, 0x00]);
        cpu.reset();
        cpu.run();
    }

    // ===== PLP (Pull Processor Status) Tests =====

    #[test]
    fn test_0x28_plp() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x28, 0x00]);
        cpu.reset();
        cpu.run();
    }

    // ===== ROL (Rotate Left) Tests =====

    #[test]
    fn test_0x2a_rol_accumulator() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x2a, 0x00]);
        cpu.reset();
        cpu.reg_a = 0x40;
        cpu.run();
    }

    #[test]
    fn test_0x26_rol_zero_page() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x26, 0x10, 0x00]);
        cpu.mem.write(0x10, 0x40);
        cpu.reset();
        cpu.run();
    }

    #[test]
    fn test_0x2e_rol_absolute() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x2e, 0x20, 0x80, 0x00]);
        cpu.mem.write(0x8020, 0x40);
        cpu.reset();
        cpu.run();
    }

    // ===== ROR (Rotate Right) Tests =====

    #[test]
    fn test_0x6a_ror_accumulator() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x6a, 0x00]);
        cpu.reset();
        cpu.reg_a = 0x02;
        cpu.run();
    }

    #[test]
    fn test_0x66_ror_zero_page() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x66, 0x10, 0x00]);
        cpu.mem.write(0x10, 0x02);
        cpu.reset();
        cpu.run();
    }

    #[test]
    fn test_0x6e_ror_absolute() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x6e, 0x20, 0x80, 0x00]);
        cpu.mem.write(0x8020, 0x02);
        cpu.reset();
        cpu.run();
    }

    // ===== RTI (Return from Interrupt) Tests =====

    #[test]
    fn test_0x40_rti() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x40, 0x00]);
        cpu.reset();
        cpu.run();
    }

    // ===== RTS (Return from Subroutine) Tests =====

    #[test]
    fn test_0x60_rts() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x60, 0x00]);
        cpu.reset();
        cpu.run();
    }

    // ===== SBC (Subtract with Carry) Tests =====

    #[test]
    fn test_0xe9_sbc_immediate() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xe9, 0x30, 0x00]);
        cpu.reset();
        cpu.reg_a = 0x50;
        cpu.run();
    }

    #[test]
    fn test_0xe5_sbc_zero_page() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xe5, 0x10, 0x00]);
        cpu.mem.write(0x10, 0x20);
        cpu.reset();
        cpu.reg_a = 0x50;
        cpu.run();
    }

    #[test]
    fn test_0xed_sbc_absolute() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xed, 0x20, 0x80, 0x00]);
        cpu.mem.write(0x8020, 0x30);
        cpu.reset();
        cpu.reg_a = 0x60;
        cpu.run();
    }

    // ===== SEC (Set Carry) Tests =====

    #[test]
    fn test_0x38_sec() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x38, 0x00]);
        cpu.reset();
        cpu.run();
    }

    // ===== SED (Set Decimal) Tests =====

    #[test]
    fn test_0xf8_sed() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xf8, 0x00]);
        cpu.reset();
        cpu.run();
    }

    // ===== SEI (Set Interrupt Disable) Tests =====

    #[test]
    fn test_0x78_sei() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x78, 0x00]);
        cpu.reset();
        cpu.run();
    }

    // ===== CLC (Clear Carry) Tests =====

    #[test]
    fn test_0x18_clc() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x18, 0x00]);
        cpu.reset();
        cpu.run();
    }

    // ===== CLD (Clear Decimal) Tests =====

    #[test]
    fn test_0xd8_cld() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xd8, 0x00]);
        cpu.reset();
        cpu.run();
    }

    // ===== CLI (Clear Interrupt Disable) Tests =====

    #[test]
    fn test_0x58_cli() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x58, 0x00]);
        cpu.reset();
        cpu.run();
    }

    // ===== CLV (Clear Overflow) Tests =====

    #[test]
    fn test_0xb8_clv() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xb8, 0x00]);
        cpu.reset();
        cpu.run();
    }

    // ===== STA (Store Accumulator) Tests =====

    #[test]
    fn test_0x85_sta_zero_page() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x85, 0x10, 0x00]);
        cpu.reset();
        cpu.reg_a = 0x42;
        cpu.run();

        assert_eq!(cpu.mem.read(0x10), 0x42);
    }

    #[test]
    fn test_0x8d_sta_absolute() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x8d, 0x20, 0x80, 0x00]);
        cpu.reset();
        cpu.reg_a = 0x55;
        cpu.run();

        assert_eq!(cpu.mem.read(0x8020), 0x55);
    }

    #[test]
    fn test_0x95_sta_zero_page_x() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x95, 0x10, 0x00]);
        cpu.reset();
        cpu.reg_a = 0x77;
        cpu.reg_x = 0x05;
        cpu.run();

        assert_eq!(cpu.mem.read(0x15), 0x77);
    }

    #[test]
    fn test_0x9d_sta_absolute_x() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x9d, 0x20, 0x80, 0x00]);
        cpu.reset();
        cpu.reg_a = 0xaa;
        cpu.reg_x = 0x05;
        cpu.run();

        assert_eq!(cpu.mem.read(0x8025), 0xaa);
    }

    #[test]
    fn test_0x99_sta_absolute_y() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x99, 0x20, 0x80, 0x00]);
        cpu.reset();
        cpu.reg_a = 0xbb;
        cpu.reg_y = 0x10;
        cpu.run();

        assert_eq!(cpu.mem.read(0x8030), 0xbb);
    }

    // ===== STX (Store X) Tests =====

    #[test]
    fn test_0x86_stx_zero_page() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x86, 0x10, 0x00]);
        cpu.reset();
        cpu.reg_x = 0x44;
        cpu.run();

        assert_eq!(cpu.mem.read(0x10), 0x44);
    }

    #[test]
    fn test_0x8e_stx_absolute() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x8e, 0x20, 0x80, 0x00]);
        cpu.reset();
        cpu.reg_x = 0x66;
        cpu.run();

        assert_eq!(cpu.mem.read(0x8020), 0x66);
    }

    #[test]
    fn test_0x96_stx_zero_page_y() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x96, 0x10, 0x00]);
        cpu.reset();
        cpu.reg_x = 0x88;
        cpu.reg_y = 0x05;
        cpu.run();

        assert_eq!(cpu.mem.read(0x15), 0x88);
    }

    // ===== STY (Store Y) Tests =====

    #[test]
    fn test_0x84_sty_zero_page() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x84, 0x10, 0x00]);
        cpu.reset();
        cpu.reg_y = 0x33;
        cpu.run();

        assert_eq!(cpu.mem.read(0x10), 0x33);
    }

    #[test]
    fn test_0x8c_sty_absolute() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x8c, 0x20, 0x80, 0x00]);
        cpu.reset();
        cpu.reg_y = 0x55;
        cpu.run();

        assert_eq!(cpu.mem.read(0x8020), 0x55);
    }

    #[test]
    fn test_0x94_sty_zero_page_x() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x94, 0x10, 0x00]);
        cpu.reset();
        cpu.reg_y = 0x77;
        cpu.reg_x = 0x05;
        cpu.run();

        assert_eq!(cpu.mem.read(0x15), 0x77);
    }

    // ===== TAX (Transfer A to X) Tests =====

    #[test]
    fn test_0xaa_tax_non_zero() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xaa, 0x00]);
        cpu.reset();
        cpu.reg_a = 0x42;
        cpu.run();

        assert_eq!(cpu.reg_x, 0x42);
        assert!(!cpu.status.contains(Status::ZERO));
    }

    #[test]
    fn test_0xaa_tax_zero() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xaa, 0x00]);
        cpu.reset();
        cpu.reg_a = 0x00;
        cpu.run();

        assert_eq!(cpu.reg_x, 0x00);
        assert!(cpu.status.contains(Status::ZERO));
    }

    #[test]
    fn test_0xaa_tax_negative() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xaa, 0x00]);
        cpu.reset();
        cpu.reg_a = 0x80;
        cpu.run();

        assert_eq!(cpu.reg_x, 0x80);
        assert!(cpu.status.contains(Status::NEGATIVE));
    }

    // ===== TAY (Transfer A to Y) Tests =====

    #[test]
    fn test_0xa8_tay() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xa8, 0x00]);
        cpu.reset();
        cpu.reg_a = 0x50;
        cpu.run();

        assert_eq!(cpu.reg_y, 0x50);
    }

    #[test]
    fn test_0xa8_tay_zero() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xa8, 0x00]);
        cpu.reset();
        cpu.reg_a = 0x00;
        cpu.run();

        assert_eq!(cpu.reg_y, 0x00);
        assert!(cpu.status.contains(Status::ZERO));
    }

    // ===== TSX (Transfer Stack Pointer to X) Tests =====

    #[test]
    fn test_0xba_tsx() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xba, 0x00]);
        cpu.reset();
        cpu.run();
    }

    // ===== TXA (Transfer X to A) Tests =====

    #[test]
    fn test_0x8a_txa() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x8a, 0x00]);
        cpu.reset();
        cpu.reg_x = 0x60;
        cpu.run();

        assert_eq!(cpu.reg_a, 0x60);
    }

    #[test]
    fn test_0x8a_txa_zero() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x8a, 0x00]);
        cpu.reset();
        cpu.reg_x = 0x00;
        cpu.run();

        assert_eq!(cpu.reg_a, 0x00);
        assert!(cpu.status.contains(Status::ZERO));
    }

    // ===== TXS (Transfer X to Stack Pointer) Tests =====

    #[test]
    fn test_0x9a_txs() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x9a, 0x00]);
        cpu.reset();
        cpu.reg_x = 0x70;
        cpu.run();
    }

    // ===== TYA (Transfer Y to A) Tests =====

    #[test]
    fn test_0x98_tya() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x98, 0x00]);
        cpu.reset();
        cpu.reg_y = 0x80;
        cpu.run();

        assert_eq!(cpu.reg_a, 0x80);
        assert!(cpu.status.contains(Status::NEGATIVE));
    }

    #[test]
    fn test_0x98_tya_zero() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x98, 0x00]);
        cpu.reset();
        cpu.reg_y = 0x00;
        cpu.run();

        assert_eq!(cpu.reg_a, 0x00);
        assert!(cpu.status.contains(Status::ZERO));
    }

    // ===== Indexed Indirect Addressing Mode Tests =====

    #[test]
    fn test_0xa1_lda_indexed_indirect() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xa1, 0x10, 0x00]);
        cpu.mem.write_u16(0x15, 0x8020);
        cpu.mem.write(0x8020, 0x42);
        cpu.reset();
        cpu.reg_x = 0x05;
        cpu.run();

        assert_eq!(cpu.reg_a, 0x42);
    }

    #[test]
    fn test_0xb1_lda_indirect_indexed() {
        let mut cpu = Cpu::new();
        cpu.load(&[0xb1, 0x10, 0x00]);
        cpu.mem.write_u16(0x10, 0x8020);
        cpu.mem.write(0x8025, 0x55);
        cpu.reset();
        cpu.reg_y = 0x05;
        cpu.run();

        assert_eq!(cpu.reg_a, 0x55);
    }

    #[test]
    fn test_0x81_sta_indexed_indirect() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x81, 0x10, 0x00]);
        cpu.mem.write_u16(0x15, 0x8020);
        cpu.reset();
        cpu.reg_a = 0x99;
        cpu.reg_x = 0x05;
        cpu.run();

        assert_eq!(cpu.mem.read(0x8020), 0x99);
    }

    #[test]
    fn test_0x91_sta_indirect_indexed() {
        let mut cpu = Cpu::new();
        cpu.load(&[0x91, 0x10, 0x00]);
        cpu.mem.write_u16(0x10, 0x8020);
        cpu.reset();
        cpu.reg_a = 0xcc;
        cpu.reg_y = 0x05;
        cpu.run();

        assert_eq!(cpu.mem.read(0x8025), 0xcc);
    }
}
