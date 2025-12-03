bitflags::bitflags! {
    pub struct Status: u8 {
        const ZERO = 0b0000_0010;
        const NEGATIVE = 0b1000_0000;
    }
}

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

    /// For instructions that do not use addressing modes
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
];

pub struct Cpu {
    pub op_table: Vec<Option<&'static Op>>,

    pub halted: bool,

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

            halted: false,

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
        while !self.halted {
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

    fn operand_addr_next(&mut self, mode: AddressingMode) -> u16 {
        match mode {
            AddressingMode::Immediate => self.pc_next(),
            AddressingMode::ZeroPage => u16::from(self.read_pc_next()),
            AddressingMode::ZeroPageX => {
                let addr = self.read_pc_next();
                u16::from(addr.wrapping_add(self.reg_x))
            }
            AddressingMode::ZeroPageY => {
                let addr = self.read_pc_next();
                u16::from(addr.wrapping_add(self.reg_y))
            }
            AddressingMode::Absolute => {
                let addr = self.read_pc_u16_next();
                u16::from(self.mem.read(addr))
            }
            AddressingMode::AbsoluteX => {
                let addr = self.read_pc_u16_next();
                addr.wrapping_add(u16::from(self.reg_x))
            }
            AddressingMode::AbsoluteY => {
                let addr = self.read_pc_u16_next();
                addr.wrapping_add(u16::from(self.reg_y))
            }
            AddressingMode::Relative => {
                let offset = self.read_pc_next();
                self.pc.wrapping_add(u16::from(offset))
            }
            AddressingMode::Indirect => {
                let addr = self.read_pc_u16_next();
                self.mem.read_u16(addr)
            }
            AddressingMode::IndexedIndirect => {
                let base = self.read_pc_next();
                self.mem.read_u16(u16::from(base.wrapping_add(self.reg_x)))
            }
            AddressingMode::IndirectIndexed => {
                let base = self.read_pc_next();
                self.mem
                    .read_u16(u16::from(base))
                    .wrapping_add(u16::from(self.reg_y))
            }
            AddressingMode::None => {
                panic!("Addressing mode 'None' does not have an operand address")
            }
        }
    }

    fn adc(&mut self, _op: &'static Op) {
        todo!("op {:?} not yet implemented", _op.name)
    }

    fn and(&mut self, _op: &'static Op) {
        todo!("op {:?} not yet implemented", _op.name)
    }

    fn asl(&mut self, _op: &'static Op) {
        todo!("op {:?} not yet implemented", _op.name)
    }

    fn bcc(&mut self, _op: &'static Op) {
        todo!("op {:?} not yet implemented", _op.name)
    }

    fn bcs(&mut self, _op: &'static Op) {
        todo!("op {:?} not yet implemented", _op.name)
    }

    fn beq(&mut self, _op: &'static Op) {
        todo!("op {:?} not yet implemented", _op.name)
    }

    fn bit(&mut self, _op: &'static Op) {
        todo!("op {:?} not yet implemented", _op.name)
    }

    fn bmi(&mut self, _op: &'static Op) {
        todo!("op {:?} not yet implemented", _op.name)
    }

    fn bne(&mut self, _op: &'static Op) {
        todo!("op {:?} not yet implemented", _op.name)
    }

    fn bpl(&mut self, _op: &'static Op) {
        todo!("op {:?} not yet implemented", _op.name)
    }

    fn brk(&mut self, _op: &'static Op) {
        self.halted = true;
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
        let addr = self.operand_addr_next(op.mode);
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
        let addr = self.operand_addr_next(op.mode);
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

    fn update_status(&mut self, result: u8) {
        if result == 0 {
            self.status |= Status::ZERO;
        } else {
            self.status &= !Status::ZERO;
        }

        if result & 0b1000_0000 != 0 {
            self.status |= Status::NEGATIVE;
        } else {
            self.status &= !Status::NEGATIVE;
        }
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
}
