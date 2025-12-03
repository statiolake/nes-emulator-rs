bitflags::bitflags! {
    struct Status: u8 {
        const ZERO = 0b0000_0010;
        const NEGATIVE = 0b1000_0000;
    }
}

pub struct CPU {
    pub reg_a: u8,
    pub status: Status,
    pub pc: u16,
}

impl CPU {
    pub fn new() -> Self {
        CPU {
            reg_a: 0,
            status: Status::empty(),
            pc: 0,
        }
    }

    pub fn interpret(&mut self, program: &[u8]) {
        self.pc = 0;
        loop {
            let ops = program[self.pc as usize];
            self.pc += 1;
            match ops {
                // BRK
                0x00 => break,

                // LDA Immediate
                0xa9 => {
                    let param = program[self.pc as usize];
                    self.pc += 1;
                    self.reg_a = param;

                    if self.reg_a == 0 {
                        self.status |= Status::ZERO;
                    } else {
                        self.status &= !Status::ZERO;
                    }

                    if self.reg_a & 0b1000_0000 != 0 {
                        self.status |= Status::NEGATIVE;
                    } else {
                        self.status &= !Status::NEGATIVE;
                    }
                }
                _ => todo!(),
            }
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
}
