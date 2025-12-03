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

    /// ($01, X)
    IndexedIndirect,

    /// ($01), Y
    IndirectIndexed,

    /// For instructions that do not use addressing modes
    None,
}

pub struct CPU {
    pub reg_a: u8,
    pub reg_x: u8,
    pub reg_y: u8,
    pub status: Status,
    pub pc: u16,

    pub mem: Memory,
}

impl CPU {
    pub fn new() -> Self {
        CPU {
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
        loop {
            let op_code = self.read_pc_next();
            match op_code {
                // BRK
                0x00 => break,

                // LDA Immediate
                0xa9 => self.lda(AddressingMode::Immediate),

                // TAX
                0xaa => self.tax(),

                // INX
                0xe8 => self.inx(),

                _ => todo!("opcode {op_code:x} not implemented"),
            }
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

    fn lda(&mut self, mode: AddressingMode) {
        let addr = self.operand_addr_next(mode);
        self.reg_a = self.mem.read(addr);
        self.update_status(self.reg_a);
    }

    fn tax(&mut self) {
        self.reg_x = self.reg_a;
        self.update_status(self.reg_x);
    }

    fn inx(&mut self) {
        self.reg_x = self.reg_x.wrapping_add(1);
        self.update_status(self.reg_x);
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

impl Default for CPU {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_0xa9_lda_immediate_load_data() {
        let mut cpu = CPU::new();
        cpu.load(&[0xa9, 0x05, 0x00]);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.reg_a, 0x05);
        assert!(!cpu.status.contains(Status::ZERO));
        assert!(!cpu.status.contains(Status::NEGATIVE));
    }

    #[test]
    fn test_0xa9_lda_zero_flag() {
        let mut cpu = CPU::new();
        cpu.load(&[0xa9, 0x00, 0x00]);
        cpu.reset();
        cpu.run();

        assert!(cpu.status.contains(Status::ZERO));
    }

    #[test]
    fn test_0xaa_tax_move_a_to_x() {
        let mut cpu = CPU::new();
        cpu.load(&[0xaa, 0x00]);
        cpu.reset();
        cpu.reg_a = 10;
        cpu.run();

        assert_eq!(cpu.reg_x, 10)
    }

    #[test]
    fn test_5_ops_working_together() {
        let mut cpu = CPU::new();
        cpu.load(&[0xa9, 0xc0, 0xaa, 0xe8, 0x00]);
        cpu.reset();
        cpu.reg_a = 0xc0;
        cpu.run();

        assert_eq!(cpu.reg_x, 0xc1)
    }

    #[test]
    fn test_inx_overflow() {
        let mut cpu = CPU::new();
        cpu.load(&[0xe8, 0xe8, 0x00]);
        cpu.reset();
        cpu.reg_x = 0xff;
        cpu.run();

        assert_eq!(cpu.reg_x, 1)
    }
}
