bitflags::bitflags! {
    pub struct Status: u8 {
        const ZERO = 0b0000_0010;
        const NEGATIVE = 0b1000_0000;
    }
}

pub struct CPU {
    pub reg_a: u8,
    pub reg_x: u8,
    pub status: Status,
    pub pc: u16,
}

impl CPU {
    pub fn new() -> Self {
        CPU {
            reg_a: 0,
            reg_x: 0,
            status: Status::empty(),
            pc: 0,
        }
    }

    pub fn interpret(&mut self, program: &[u8]) {
        loop {
            let op_code = self.read_pc_next(program);
            match op_code {
                // BRK
                0x00 => break,

                // LDA Immediate
                0xa9 => {
                    let value = self.read_pc_next(program);
                    self.lda(value);
                }

                // TAX
                0xaa => self.tax(),

                // INX
                0xe8 => self.inx(),

                _ => todo!("opcode {op_code:x} not implemented"),
            }
        }
    }

    fn read_pc_next(&mut self, program: &[u8]) -> u8 {
        let byte = program[self.pc as usize];
        self.pc += 1;
        byte
    }

    fn lda(&mut self, value: u8) {
        self.reg_a = value;
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
        cpu.interpret(&[0xa9, 0x05, 0x00]);
        assert_eq!(cpu.reg_a, 0x05);
        assert!(!cpu.status.contains(Status::ZERO));
        assert!(!cpu.status.contains(Status::NEGATIVE));
    }

    #[test]
    fn test_0xa9_lda_zero_flag() {
        let mut cpu = CPU::new();
        cpu.interpret(&[0xa9, 0x00, 0x00]);
        assert!(cpu.status.contains(Status::ZERO));
    }

    #[test]
    fn test_0xaa_tax_move_a_to_x() {
        let mut cpu = CPU::new();
        cpu.reg_a = 10;
        cpu.interpret(&[0xaa, 0x00]);

        assert_eq!(cpu.reg_x, 10)
    }

    #[test]
    fn test_5_ops_working_together() {
        let mut cpu = CPU::new();
        cpu.interpret(&[0xa9, 0xc0, 0xaa, 0xe8, 0x00]);

        assert_eq!(cpu.reg_x, 0xc1)
    }

    #[test]
    fn test_inx_overflow() {
        let mut cpu = CPU::new();
        cpu.reg_x = 0xff;
        cpu.interpret(&[0xe8, 0xe8, 0x00]);

        assert_eq!(cpu.reg_x, 1)
    }
}
