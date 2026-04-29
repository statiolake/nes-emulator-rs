use std::{
    cell::Cell,
    fmt,
    pin::Pin,
    rc::Rc,
    sync::{Arc, LazyLock, mpsc::Sender},
};

use itertools::Itertools as _;
use log::warn;

use crate::hardware::{bus::Bus, ram::Ram};

/// CPU clock multiplier compared to master clock
const CPU_CLOCK_MUL: usize = 12;

pub struct Cpu {
    pub bus: Rc<Bus>,
    pub state: State,

    pub debug_tx: Option<Sender<String>>,
}

pub struct State {
    halted: Cell<bool>,

    pub reg_a: Cell<u8>,
    pub reg_x: Cell<u8>,
    pub reg_y: Cell<u8>,
    pub status: Cell<Status>,
    pub pc: Cell<u16>,
    pub sp: Cell<u8>,
}

#[derive(Debug, Default)]
pub struct CpuMountOptions {
    pub debug_tx: Option<Sender<String>>,
}

impl Cpu {
    pub fn mount(bus: Rc<Bus>, opts: CpuMountOptions) -> Self {
        let CpuMountOptions { debug_tx } = opts;
        Cpu {
            bus,
            state: State::new(),

            debug_tx,
        }
    }

    pub async fn run(&self) {
        loop {
            // self.state.debug_dump_state();
            let op_code = self.read_pc_next().await;
            let Some(op) = OP_TABLE[op_code as usize] else {
                warn!(
                    "Invalid opcode {op_code:x} at PC {:#06x}",
                    self.state.pc.get().wrapping_sub(1)
                );
                self.state.halted.set(true);
                return;
            };

            (op.handler.imp)(self, op).await;

            if self.is_halted() {
                break;
            }
        }
    }

    pub async fn interrupt_reset(&self) {
        self.state.halted.set(false);
        self.state.reg_a.set(0);
        self.state.reg_x.set(0);
        self.state.reg_y.set(0);
        self.state.status.set(Status::RESERVED);
        self.state.pc.set(self.bus.read_u16(0xfffc).await);
        self.state.sp.set(0xff);
    }

    pub async fn interrupt_nmi(&self) {
        self.stack_push_u16(self.state.pc.get()).await;

        // Remove B flag when pushing to stack by interrupt
        self.stack_push((self.state.status.get() & !Status::B_FLAG).bits())
            .await;
        self.state
            .status
            .set(self.state.status.get() | Status::INTERRUPT_DISABLE);

        self.state.pc.set(self.bus.read_u16(0xfffa).await);
    }

    pub fn is_halted(&self) -> bool {
        self.state.halted.get()
    }

    fn pc_next(&self) -> u16 {
        let pc = self.state.pc.get();
        self.state.pc.set(pc.wrapping_add(1));
        pc
    }

    async fn read_pc_next(&self) -> u8 {
        let pc = self.pc_next();

        self.bus.read(pc).await
    }

    async fn read_pc_u16_next(&self) -> u16 {
        // Increment PC twice to read two bytes
        let pc = self.pc_next();
        let _ = self.pc_next();

        self.bus.read_u16(pc).await
    }

    async fn stack_push(&self, value: u8) {
        let sp_addr = u16::from_be_bytes([0x01, self.state.sp.get()]);
        self.bus.write(sp_addr, value).await;
        self.state.sp.set(self.state.sp.get().wrapping_sub(1));
    }

    async fn stack_pop(&self) -> u8 {
        if self.state.sp.get() == 0xff {
            panic!("Stack underflow");
        }

        self.state.sp.set(self.state.sp.get().wrapping_add(1));
        let sp_addr = u16::from_be_bytes([0x01, self.state.sp.get()]);
        self.bus.read(sp_addr).await
    }

    async fn stack_push_u16(&self, value: u16) {
        let [lo, hi] = value.to_le_bytes();
        self.stack_push(hi).await;
        self.stack_push(lo).await;
    }

    async fn stack_pop_u16(&self) -> u16 {
        let lo = self.stack_pop().await;
        let hi = self.stack_pop().await;
        u16::from_le_bytes([lo, hi])
    }

    async fn operand_addr_next(&self, mode: AddressingMode) -> Address {
        use AddressingMode::*;

        match mode {
            Immediate => Address::Mem(self.pc_next()),
            ZeroPage => Address::Mem(u16::from(self.read_pc_next().await)),
            ZeroPageX => {
                let addr = self.read_pc_next().await;
                Address::Mem(u16::from(addr.wrapping_add(self.state.reg_x.get())))
            }
            ZeroPageY => {
                let addr = self.read_pc_next().await;
                Address::Mem(u16::from(addr.wrapping_add(self.state.reg_y.get())))
            }
            Absolute => Address::Mem(self.read_pc_u16_next().await),
            AbsoluteX => {
                let addr = self.read_pc_u16_next().await;
                Address::Mem(addr.wrapping_add(u16::from(self.state.reg_x.get())))
            }
            AbsoluteY => {
                let addr = self.read_pc_u16_next().await;
                Address::Mem(addr.wrapping_add(u16::from(self.state.reg_y.get())))
            }
            Relative => {
                // this relative offset is signed
                let offset = self.read_pc_next().await as i8;
                Address::Mem(self.state.pc.get().wrapping_add_signed(i16::from(offset)))
            }
            Indirect => {
                let addr = self.read_pc_u16_next().await;

                // Emulate 6502 page boundary hardware bug
                // On page boundary, the high byte does not wrap to the next page
                // So, if the addr is $01FF, the hi byte is read from $0100 instead of $0200
                let [lo_addr, hi_addr] = addr.to_le_bytes();
                let lo = self.bus.read(u16::from_le_bytes([lo_addr, hi_addr])).await;
                let hi = self
                    .bus
                    .read(u16::from_le_bytes([lo_addr.wrapping_add(1), hi_addr]))
                    .await;

                Address::Mem(u16::from_le_bytes([lo, hi]))
            }
            IndexedIndirect => {
                let base = self.read_pc_next().await;
                let offsetted = base.wrapping_add(self.state.reg_x.get());
                // IndexedIndirect always read address from zero page
                let lo = self.bus.read(u16::from(offsetted)).await;
                let hi = self.bus.read(u16::from(offsetted.wrapping_add(1))).await;
                let addr = u16::from_le_bytes([lo, hi]);
                Address::Mem(addr)
            }
            IndirectIndexed => {
                let base = self.read_pc_next().await;
                // IndirectIndexed always read address from zero page
                let lo = self.bus.read(u16::from(base)).await;
                let hi = self.bus.read(u16::from(base.wrapping_add(1))).await;
                let addr = u16::from_le_bytes([lo, hi]);

                Address::Mem(addr.wrapping_add(u16::from(self.state.reg_y.get())))
            }
            Accumulator => Address::Accum,
            Implied => {
                panic!("Implied addressing mode does not have an operand address")
            }
        }
    }

    async fn adc(&self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode).await;
        let value = addr.read_from(self).await;
        self.adc_impl(Address::Accum, value, true).await;
    }

    async fn and(&self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode).await;
        let value = addr.read_from(self).await;
        self.and_impl(Address::Accum, value).await;
    }

    async fn asl(&self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode).await;
        self.asl_impl(addr).await;
    }

    async fn bcc(&self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode).await.expect_mem();
        if !self.state.status.get().contains(Status::CARRY) {
            self.state.pc.set(addr);
        }
    }

    async fn bcs(&self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode).await.expect_mem();
        if self.state.status.get().contains(Status::CARRY) {
            self.state.pc.set(addr);
        }
    }

    async fn beq(&self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode).await.expect_mem();
        if self.state.status.get().contains(Status::ZERO) {
            self.state.pc.set(addr);
        }
    }

    async fn bit(&self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode).await;
        let value = addr.read_from(self).await;
        let mask = self.state.reg_a.get();
        let result = value & mask;

        self.state.set_status_flag(Status::ZERO, result == 0);
        self.state
            .set_status_flag(Status::OVERFLOW, value & 0b0100_0000 != 0);
        self.state
            .set_status_flag(Status::NEGATIVE, value & SIGN_BIT != 0);
    }

    async fn bmi(&self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode).await.expect_mem();
        if self.state.status.get().contains(Status::NEGATIVE) {
            self.state.pc.set(addr);
        }
    }

    async fn bne(&self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode).await.expect_mem();
        if !self.state.status.get().contains(Status::ZERO) {
            self.state.pc.set(addr);
        }
    }

    async fn bpl(&self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode).await.expect_mem();
        if !self.state.status.get().contains(Status::NEGATIVE) {
            self.state.pc.set(addr);
        }
    }

    async fn brk(&self, _op: &'static Opcode) {
        self.state.halted.set(true);
    }

    async fn bvc(&self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode).await.expect_mem();
        if !self.state.status.get().contains(Status::OVERFLOW) {
            self.state.pc.set(addr);
        }
    }

    async fn bvs(&self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode).await.expect_mem();
        if self.state.status.get().contains(Status::OVERFLOW) {
            self.state.pc.set(addr);
        }
    }

    async fn clc(&self, _op: &'static Opcode) {
        self.state.remove_status_flag(Status::CARRY);
    }

    async fn cld(&self, _op: &'static Opcode) {
        // Decimal mode is not supported but we can set the flag
        self.state.remove_status_flag(Status::DECIMAL_MODE);
    }

    async fn cli(&self, _op: &'static Opcode) {
        self.state.remove_status_flag(Status::INTERRUPT_DISABLE);
    }

    async fn clv(&self, _op: &'static Opcode) {
        self.state.remove_status_flag(Status::OVERFLOW);
    }

    async fn cmp(&self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode).await;
        let value = addr.read_from(self).await;
        self.cmp_impl(Address::Accum, value).await;
    }

    async fn cpx(&self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode).await;
        let value = addr.read_from(self).await;

        let reg_x = self.state.reg_x.get();
        self.state.set_status_flag(Status::CARRY, reg_x >= value);
        self.state.set_status_flag(Status::ZERO, reg_x == value);
        self.state.set_status_flag(
            Status::NEGATIVE,
            (reg_x.wrapping_sub(value)) & SIGN_BIT != 0,
        );
    }

    async fn cpy(&self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode).await;
        let value = addr.read_from(self).await;

        let reg_y = self.state.reg_y.get();
        self.state.set_status_flag(Status::CARRY, reg_y >= value);
        self.state.set_status_flag(Status::ZERO, reg_y == value);
        self.state.set_status_flag(
            Status::NEGATIVE,
            (reg_y.wrapping_sub(value)) & SIGN_BIT != 0,
        );
    }

    async fn dec(&self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode).await;
        self.sbc_impl(addr, 1, false).await;
    }

    async fn dex(&self, _op: &'static Opcode) {
        let reg_x = self.state.reg_x.get().wrapping_sub(1);
        self.state.reg_x.set(reg_x);

        self.state.set_status_flag(Status::ZERO, reg_x == 0);
        self.state
            .set_status_flag(Status::NEGATIVE, reg_x & SIGN_BIT != 0);
    }

    async fn dey(&self, _op: &'static Opcode) {
        let reg_y = self.state.reg_y.get().wrapping_sub(1);
        self.state.reg_y.set(reg_y);

        self.state.set_status_flag(Status::ZERO, reg_y == 0);
        self.state
            .set_status_flag(Status::NEGATIVE, reg_y & SIGN_BIT != 0);
    }

    async fn eor(&self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode).await;
        let value = addr.read_from(self).await;

        let result = self.state.reg_a.get() ^ value;
        self.state.reg_a.set(result);

        self.state.set_status_flag(Status::ZERO, result == 0);
        self.state
            .set_status_flag(Status::NEGATIVE, result & SIGN_BIT != 0);
    }

    async fn inc(&self, _op: &'static Opcode) {
        let addr = self.operand_addr_next(_op.mode).await;
        let value = addr.read_from(self).await;
        let result = value.wrapping_add(1);
        addr.write_to(self, result).await;

        self.state.set_status_flag(Status::ZERO, result == 0);
        self.state
            .set_status_flag(Status::NEGATIVE, result & SIGN_BIT != 0);
    }

    async fn inx(&self, _op: &'static Opcode) {
        let reg_x = self.state.reg_x.get().wrapping_add(1);
        self.state.reg_x.set(reg_x);

        self.state.set_status_flag(Status::ZERO, reg_x == 0);
        self.state
            .set_status_flag(Status::NEGATIVE, reg_x & SIGN_BIT != 0);
    }

    async fn iny(&self, _op: &'static Opcode) {
        let reg_y = self.state.reg_y.get().wrapping_add(1);
        self.state.reg_y.set(reg_y);

        self.state.set_status_flag(Status::ZERO, reg_y == 0);
        self.state
            .set_status_flag(Status::NEGATIVE, reg_y & SIGN_BIT != 0);
    }

    async fn jmp(&self, _op: &'static Opcode) {
        let addr = self.operand_addr_next(_op.mode).await.expect_mem();
        self.state.pc.set(addr);
    }

    async fn jsr(&self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode).await.expect_mem();

        self.stack_push_u16(self.state.pc.get().wrapping_sub(1))
            .await;

        self.state.pc.set(addr);
    }

    async fn lda(&self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode).await;
        let value = addr.read_from(self).await;
        self.state.reg_a.set(value);

        self.state.set_status_flag(Status::ZERO, value == 0);
        self.state
            .set_status_flag(Status::NEGATIVE, value & SIGN_BIT != 0);
    }

    async fn ldx(&self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode).await;
        let value = addr.read_from(self).await;
        self.state.reg_x.set(value);

        self.state.set_status_flag(Status::ZERO, value == 0);
        self.state
            .set_status_flag(Status::NEGATIVE, value & SIGN_BIT != 0);
    }

    async fn ldy(&self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode).await;
        let value = addr.read_from(self).await;
        self.state.reg_y.set(value);

        self.state.set_status_flag(Status::ZERO, value == 0);
        self.state
            .set_status_flag(Status::NEGATIVE, value & SIGN_BIT != 0);
    }

    async fn lsr(&self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode).await;
        let value = addr.read_from(self).await;

        let carry = value & 0b0000_0001 != 0;
        let result = value >> 1;
        addr.write_to(self, result).await;

        self.state.set_status_flag(Status::CARRY, carry);
        self.state.set_status_flag(Status::ZERO, result == 0);
        self.state
            .set_status_flag(Status::NEGATIVE, result & SIGN_BIT != 0);
    }

    async fn nop(&self, _op: &'static Opcode) {}

    async fn ora(&self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode).await;
        let value = addr.read_from(self).await;
        self.or_impl(Address::Accum, value).await;
    }

    async fn pha(&self, _op: &'static Opcode) {
        let value = self.state.reg_a.get();
        self.stack_push(value).await;
    }

    async fn php(&self, _op: &'static Opcode) {
        // Set the B flag when pushing to stack by instruction
        let value = (self.state.status.get() | Status::B_FLAG).bits();
        self.stack_push(value).await;
    }

    async fn pla(&self, _op: &'static Opcode) {
        let value = self.stack_pop().await;
        self.state.reg_a.set(value);

        self.state.set_status_flag(Status::ZERO, value == 0);
        self.state
            .set_status_flag(Status::NEGATIVE, value & SIGN_BIT != 0);
    }

    async fn plp(&self, _op: &'static Opcode) {
        let value = self.stack_pop().await;
        // when pulling from stack, B flag is ignored, and RESERVED flag is always set.
        self.state
            .status
            .set(Status::from_bits_truncate(value) & !Status::B_FLAG | Status::RESERVED);
    }

    async fn rol(&self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode).await;
        let value = addr.read_from(self).await;
        self.rol_impl(addr, value).await;
    }

    async fn ror(&self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode).await;
        let value = addr.read_from(self).await;
        self.ror_impl(addr, value).await;
    }

    async fn rti(&self, _op: &'static Opcode) {
        // when pulling from stack, B flag is not set and RESERVED flag is always set.
        let popped = self.stack_pop().await;
        self.state
            .status
            .set(Status::from_bits_truncate(popped) & !Status::B_FLAG | Status::RESERVED);
        let pc = self.stack_pop_u16().await;
        self.state.pc.set(pc);
    }

    async fn rts(&self, _op: &'static Opcode) {
        let pc = self.stack_pop_u16().await.wrapping_add(1);
        self.state.pc.set(pc);
    }

    async fn sbc(&self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode).await;
        let value = addr.read_from(self).await;
        self.sbc_impl(Address::Accum, value, true).await;
    }

    async fn sec(&self, _op: &'static Opcode) {
        self.state.insert_status_flag(Status::CARRY);
    }

    async fn sed(&self, _op: &'static Opcode) {
        // Decimal mode is not supported but we can set the flag
        self.state.insert_status_flag(Status::DECIMAL_MODE);
    }

    async fn sei(&self, _op: &'static Opcode) {
        self.state.insert_status_flag(Status::INTERRUPT_DISABLE);
    }

    async fn sta(&self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode).await;
        addr.write_to(self, self.state.reg_a.get()).await;
    }

    async fn stx(&self, _op: &'static Opcode) {
        let addr = self.operand_addr_next(_op.mode).await;
        addr.write_to(self, self.state.reg_x.get()).await;
    }

    async fn sty(&self, _op: &'static Opcode) {
        let addr = self.operand_addr_next(_op.mode).await;
        addr.write_to(self, self.state.reg_y.get()).await;
    }

    async fn tax(&self, _op: &'static Opcode) {
        let result = self.state.reg_a.get();
        self.state.reg_x.set(result);

        self.state.set_status_flag(Status::ZERO, result == 0);
        self.state
            .set_status_flag(Status::NEGATIVE, result & SIGN_BIT != 0);
    }

    async fn tay(&self, _op: &'static Opcode) {
        let result = self.state.reg_a.get();
        self.state.reg_y.set(result);

        self.state.set_status_flag(Status::ZERO, result == 0);
        self.state
            .set_status_flag(Status::NEGATIVE, result & SIGN_BIT != 0);
    }

    async fn tsx(&self, _op: &'static Opcode) {
        let result = self.state.sp.get();
        self.state.reg_x.set(result);

        self.state.set_status_flag(Status::ZERO, result == 0);
        self.state
            .set_status_flag(Status::NEGATIVE, result & SIGN_BIT != 0);
    }

    async fn txa(&self, _op: &'static Opcode) {
        let result = self.state.reg_x.get();
        self.state.reg_a.set(result);

        self.state.set_status_flag(Status::ZERO, result == 0);
        self.state
            .set_status_flag(Status::NEGATIVE, result & SIGN_BIT != 0);
    }

    async fn txs(&self, _op: &'static Opcode) {
        self.state.sp.set(self.state.reg_x.get());
    }

    async fn tya(&self, _op: &'static Opcode) {
        let result = self.state.reg_y.get();
        self.state.reg_a.set(result);

        self.state.set_status_flag(Status::ZERO, result == 0);
        self.state
            .set_status_flag(Status::NEGATIVE, result & SIGN_BIT != 0);
    }

    async fn ivd(&self, _op: &'static Opcode) {
        panic!("Invalid opcode encountered");
    }

    // unofficial opcodes

    async fn aac(&self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode).await;
        let value = addr.read_from(self).await;
        let res = self.and_impl(Address::Accum, value).await;

        self.state
            .set_status_flag(Status::CARRY, res & SIGN_BIT != 0);
    }

    async fn sax(&self, op: &'static Opcode) {
        let res = self.state.reg_a.get() & self.state.reg_x.get();
        let addr = self.operand_addr_next(op.mode).await;
        addr.write_to(self, res).await;

        // FIXME: According to nestest.log, this instruction does not affect any flags (really?)
        // self.state.set_flag(Status::ZERO, res == 0);
        // self.state.set_flag(Status::NEGATIVE, res & SIGN_BIT != 0);
    }

    async fn arr(&self, _op: &'static Opcode) {
        todo!()
    }

    async fn asr(&self, _op: &'static Opcode) {
        todo!()
    }

    async fn atx(&self, _op: &'static Opcode) {
        todo!()
    }

    async fn axa(&self, _op: &'static Opcode) {
        todo!()
    }

    async fn axs(&self, _op: &'static Opcode) {
        todo!()
    }

    async fn dcp(&self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode).await;
        self.sbc_impl(addr, 1, false).await;
        let value = addr.read_from(self).await;
        self.cmp_impl(Address::Accum, value).await;
    }

    async fn dop(&self, op: &'static Opcode) {
        // Double NOP
        // We need to advance PC even if it is NOP because there might be operands
        let _ = self.operand_addr_next(op.mode).await;
    }

    async fn isb(&self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode).await;
        self.adc_impl(addr, 1, false).await;
        let value = addr.read_from(self).await;
        self.sbc_impl(Address::Accum, value, true).await;
    }

    async fn kil(&self, _op: &'static Opcode) {
        todo!()
    }

    async fn lar(&self, _op: &'static Opcode) {
        todo!()
    }

    async fn lax(&self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode).await;
        let value = addr.read_from(self).await;
        self.state.reg_a.set(value);
        self.state.reg_x.set(value);

        self.state.set_status_flag(Status::ZERO, value == 0);
        self.state
            .set_status_flag(Status::NEGATIVE, value & SIGN_BIT != 0);
    }

    async fn rla(&self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode).await;
        let value = addr.read_from(self).await;
        let res = self.rol_impl(addr, value).await;
        self.and_impl(Address::Accum, res).await;
    }

    async fn rra(&self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode).await;
        let value = addr.read_from(self).await;
        let res = self.ror_impl(addr, value).await;
        self.adc_impl(Address::Accum, res, true).await;
    }

    async fn slo(&self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode).await;
        let res = self.asl_impl(addr).await;
        self.or_impl(Address::Accum, res).await;
    }

    async fn sre(&self, op: &'static Opcode) {
        let addr = self.operand_addr_next(op.mode).await;
        let value = addr.read_from(self).await;
        let carry = value & 0b0000_0001 != 0;
        let result = value >> 1;
        addr.write_to(self, result).await;

        // EOR (XOR) with accumulator
        let xored = self.state.reg_a.get() ^ result;
        self.state.reg_a.set(xored);

        self.state.set_status_flag(Status::CARRY, carry);
        self.state.set_status_flag(Status::ZERO, xored == 0);
        self.state
            .set_status_flag(Status::NEGATIVE, xored & SIGN_BIT != 0);
    }

    async fn sxa(&self, _op: &'static Opcode) {
        todo!()
    }

    async fn sya(&self, _op: &'static Opcode) {
        todo!()
    }

    async fn top(&self, op: &'static Opcode) {
        // Triple NOP
        // We need to advance PC even if it is NOP because there might be operands
        let _ = self.operand_addr_next(op.mode).await;
    }

    async fn xaa(&self, _op: &'static Opcode) {
        todo!()
    }

    async fn xas(&self, _op: &'static Opcode) {
        todo!()
    }

    // impls

    async fn and_impl(&self, res_addr: Address, value: u8) -> u8 {
        let res = self.state.reg_a.get() & value;
        res_addr.write_to(self, res).await;

        self.state.set_status_flag(Status::ZERO, res == 0);
        self.state
            .set_status_flag(Status::NEGATIVE, res & SIGN_BIT != 0);

        res
    }

    async fn or_impl(&self, res_addr: Address, value: u8) -> u8 {
        let res = self.state.reg_a.get() | value;
        res_addr.write_to(self, res).await;

        self.state.set_status_flag(Status::ZERO, res == 0);
        self.state
            .set_status_flag(Status::NEGATIVE, res & SIGN_BIT != 0);

        res
    }

    async fn cmp_impl(&self, target_addr: Address, value: u8) {
        let target = target_addr.read_from(self).await;

        self.state.set_status_flag(Status::CARRY, target >= value);
        self.state.set_status_flag(Status::ZERO, target == value);
        self.state.set_status_flag(
            Status::NEGATIVE,
            (target.wrapping_sub(value)) & SIGN_BIT != 0,
        );
    }

    async fn asl_impl(&self, res_addr: Address) -> u8 {
        let value = res_addr.read_from(self).await;
        let carry = value & 0b1000_0000 != 0;
        let res = value.wrapping_shl(1);
        res_addr.write_to(self, res).await;

        self.state.set_status_flag(Status::CARRY, carry);
        self.state.set_status_flag(Status::ZERO, res == 0);
        self.state
            .set_status_flag(Status::NEGATIVE, res & SIGN_BIT != 0);

        res
    }

    async fn adc_impl(&self, res_addr: Address, value: u8, respect_carry: bool) {
        let carry = if respect_carry && self.state.status.get().contains(Status::CARRY) {
            1
        } else {
            0
        };

        let orig_value = res_addr.read_from(self).await;
        let res = orig_value.wrapping_add(value).wrapping_add(carry);
        let res_signed = (orig_value as i8)
            .wrapping_add(value as i8)
            .wrapping_add(carry as i8);

        let res_ext = u16::from(orig_value) + u16::from(value) + u16::from(carry);
        let res_ext_signed =
            i16::from(orig_value as i8) + i16::from(value as i8) + i16::from(carry);

        res_addr.write_to(self, res).await;

        self.state.set_status_flag(Status::ZERO, res == 0);
        self.state.set_status_flag(Status::NEGATIVE, res_signed < 0);
        if respect_carry {
            self.state
                .set_status_flag(Status::CARRY, res_ext > u16::from(u8::MAX));
            self.state.set_status_flag(
                Status::OVERFLOW,
                i16::from(res_signed.signum()) * res_ext_signed.signum() < 0,
            );
        }
    }

    async fn sbc_impl(&self, res_addr: Address, value: u8, respect_carry: bool) {
        if respect_carry {
            // We need to add 1 to the value before negation, because:
            // sbc(reg_a, value)
            // = reg_a - value - (1 - carry)
            // = reg_a - (value + 1) + carry
            // = adc(reg_a, -(value + 1))
            self.adc_impl(res_addr, negate(value.wrapping_add(1)), true)
                .await;
        } else {
            // No need to worry about the carry if we are not respecting it
            self.adc_impl(res_addr, negate(value), false).await;
        }
    }

    async fn rol_impl(&self, res_addr: Address, value: u8) -> u8 {
        let next_carry = value & 0b1000_0000 != 0;
        let mut res = value << 1;
        if self.state.status.get().contains(Status::CARRY) {
            res |= 0b0000_0001;
        }

        res_addr.write_to(self, res).await;

        self.state.set_status_flag(Status::CARRY, next_carry);
        self.state
            .set_status_flag(Status::NEGATIVE, res & SIGN_BIT != 0);

        res
    }

    async fn ror_impl(&self, res_addr: Address, value: u8) -> u8 {
        let next_carry = value & 0b0000_0001 != 0;
        let mut res = value >> 1;
        if self.state.status.get().contains(Status::CARRY) {
            res |= 0b1000_0000;
        }

        res_addr.write_to(self, res).await;

        self.state.set_status_flag(Status::CARRY, next_carry);
        self.state
            .set_status_flag(Status::NEGATIVE, res & SIGN_BIT != 0);

        res
    }
}

impl State {
    pub fn new() -> Self {
        Self {
            halted: Cell::new(false),
            reg_a: Cell::new(0),
            reg_x: Cell::new(0),
            reg_y: Cell::new(0),
            status: Cell::new(Status::empty()),
            pc: Cell::new(0),
            sp: Cell::new(0),
        }
    }

    fn set_status_flag(&self, flag: Status, value: bool) {
        let mut s = self.status.get();
        s.set(flag, value);
        self.status.set(s);
    }

    fn insert_status_flag(&self, flag: Status) {
        self.status.set(self.status.get() | flag);
    }

    fn remove_status_flag(&self, flag: Status) {
        self.status.set(self.status.get() & !flag);
    }
}

fn negate(value: u8) -> u8 {
    // Two's complement negation
    (!value).wrapping_add(1)
}

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
    async fn read_from(self, cpu: &Cpu) -> u8 {
        match self {
            Address::Mem(addr) => cpu.bus.read(addr).await,
            Address::Accum => cpu.state.reg_a.get(),
        }
    }

    async fn write_to(self, cpu: &Cpu, value: u8) {
        match self {
            Address::Mem(addr) => cpu.bus.write(addr, value).await,
            Address::Accum => cpu.state.reg_a.set(value),
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
    pub handler: OpcodeHandler,
}

pub struct OpcodeHandler {
    imp: Arc<
        dyn (for<'a> Fn(&'a Cpu, &'static Opcode) -> Pin<Box<dyn Future<Output = ()> + 'a>>)
            + Send
            + Sync,
    >,
}

impl Opcode {
    pub fn new<F>(code: u8, name: &'static str, mode: AddressingMode, cycles: u8, imp: F) -> Self
    where
        F: AsyncFn(&Cpu, &'static Opcode) + Copy + Send + Sync + 'static,
    {
        Opcode {
            code,
            name,
            is_official: true,
            mode,
            cycles,
            handler: OpcodeHandler::new(imp),
        }
    }

    pub fn new_unofficial<F>(
        code: u8,
        name: &'static str,
        mode: AddressingMode,
        cycles: u8,
        imp: F,
    ) -> Self
    where
        F: AsyncFn(&Cpu, &'static Opcode) + Copy + Send + Sync + 'static,
    {
        Opcode {
            code,
            name,
            is_official: false,
            mode,
            cycles,
            handler: OpcodeHandler::new(imp),
        }
    }

    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        self.mode.len() + 1
    }
}

impl fmt::Debug for OpcodeHandler {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "OpcodeHandler {{ ... }}")
    }
}

impl OpcodeHandler {
    pub fn new<F>(imp: F) -> Self
    where
        F: AsyncFnOnce(&Cpu, &'static Opcode) + Copy + Send + Sync + 'static,
    {
        Self {
            imp: Arc::new(move |cpu, op_code| Box::pin(imp(cpu, op_code))),
        }
    }
}

static CPU_OPCODES: LazyLock<Vec<Opcode>> = LazyLock::new(|| {
    vec![
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
    ]
});

static OP_TABLE: LazyLock<Vec<Option<&Opcode>>> = LazyLock::new(|| {
    let mut op_table = vec![None; 256];
    for op in &*CPU_OPCODES {
        op_table[op.code as usize] = Some(op);
    }
    op_table
});

pub struct Disassembled {
    /// Whether the instruction is an official opcode
    pub is_official: bool,

    /// An assembly-like notation of the instruction, e.g. "ORA ($33),Y"
    pub repr: String,

    /// A hint to visualize the indirect addressing resolution
    pub addr_value_hint: Option<String>,
}

pub fn debug_dump_state(cpu: &Cpu, prg_ram: &Ram) -> String {
    let pc = cpu.state.pc.get();
    let op_code = prg_ram.read(pc);
    let maybe_op = OP_TABLE[op_code as usize];
    let instr_len = if let Some(op) = maybe_op { op.len() } else { 1 };
    let instr = (0..instr_len)
        .map(|i| prg_ram.read(pc + i as u16))
        .collect::<Vec<u8>>();
    let dis = debug_disassemble(&cpu, prg_ram, &instr);

    let instr = (0..3)
        .map(|i| {
            if i < instr_len {
                format!("{:02X}", prg_ram.read(pc + i as u16))
            } else {
                "  ".to_string()
            }
        })
        .join(" ");
    let ext_mark = if dis.is_official { " " } else { "*" };
    let disassembled = format!("{} {}", dis.repr, dis.addr_value_hint.unwrap_or_default());
    let reg_a = cpu.state.reg_a.get();
    let reg_x = cpu.state.reg_x.get();
    let reg_y = cpu.state.reg_y.get();
    let p = cpu.state.status.get().bits();
    let sp = cpu.state.sp.get();

    format!(
        "{pc:04X}  {instr} {ext_mark}{disassembled:31} A:{reg_a:02X} X:{reg_x:02X} Y:{reg_y:02X} P:{p:02X} SP:{sp:02X}",
    )
}

pub fn debug_disassemble(cpu: &Cpu, prg_ram: &Ram, instr: &[u8]) -> Disassembled {
    return inner(cpu, prg_ram, instr).unwrap_or_else(|| Disassembled {
        is_official: true,
        repr: "???".to_string(),
        addr_value_hint: None,
    });

    fn inner(cpu: &Cpu, prg_ram: &Ram, instr: &[u8]) -> Option<Disassembled> {
        use AddressingMode::*;

        let op = OP_TABLE[instr[0] as usize]?;
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
                let value = prg_ram.read(u16::from(first));
                Disassembled {
                    is_official,
                    repr: format!("{op_name} ${first:02X}"),
                    addr_value_hint: Some(format!("= {:02X}", value)),
                }
            }
            ZeroPageX => {
                let first = first?;
                let addr = first.wrapping_add(cpu.state.reg_x.get());
                let value = prg_ram.read(u16::from(addr));
                Disassembled {
                    is_official,
                    repr: format!("{op_name} ${first:02X},X"),
                    addr_value_hint: Some(format!("@ {addr:02X} = {value:02X}",)),
                }
            }
            ZeroPageY => {
                let first = first?;
                let addr = first.wrapping_add(cpu.state.reg_y.get());
                let value = prg_ram.read(u16::from(addr));
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
                let value = prg_ram.read(addr);
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
                let addr = base_addr.wrapping_add(u16::from(cpu.state.reg_x.get()));
                let value = prg_ram.read(addr);
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
                let addr = base_addr.wrapping_add(u16::from(cpu.state.reg_y.get()));
                let value = prg_ram.read(addr);
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
                    .state
                    .pc
                    .get()
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
                let lo = prg_ram.read(u16::from_le_bytes([lo_addr, hi_addr]));
                let hi = prg_ram.read(u16::from_le_bytes([lo_addr.wrapping_add(1), hi_addr]));

                let addr = u16::from_le_bytes([lo, hi]);
                Disassembled {
                    is_official,
                    repr: format!("{op_name} (${ptr_addr:04X})"),
                    addr_value_hint: Some(format!("= {addr:04X}")),
                }
            }
            IndexedIndirect => {
                let first = first?;
                let offsetted = first.wrapping_add(cpu.state.reg_x.get());
                // IndexedIndirect always reads from zero page
                let lo = prg_ram.read(u16::from(offsetted));
                let hi = prg_ram.read(u16::from(offsetted.wrapping_add(1)));
                let addr = u16::from_le_bytes([lo, hi]);
                let value = prg_ram.read(addr);
                Disassembled {
                    is_official,
                    repr: format!("{op_name} (${:02X},X)", first),
                    addr_value_hint: Some(format!("@ {offsetted:02X} = {addr:04X} = {value:02X}")),
                }
            }
            IndirectIndexed => {
                let first = first?;
                // IndirectIndexed always reads from zero page
                let lo = prg_ram.read(u16::from(first));
                let hi = prg_ram.read(u16::from(first.wrapping_add(1)));
                let base_addr = u16::from_le_bytes([lo, hi]);
                let addr = base_addr.wrapping_add(u16::from(cpu.state.reg_y.get()));
                let value = prg_ram.read(addr);
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

#[cfg(test)]
mod test {
    use crate::{
        hardware::ram::{Ram, RamMountOptions},
        rt::{ClockedFuture, Runtime, Schedule},
    };

    use super::*;

    struct Tester {
        bus: Rc<Bus>,
        cpu: Rc<Cpu>,
        mem: Rc<Ram>,
    }

    impl Tester {
        fn to_schedule(&self) -> Schedule<()> {
            Schedule::new()
                .with_main(ClockedFuture {
                    clock_mul: 12,
                    future: Box::pin({
                        let cpu = Rc::clone(&self.cpu);
                        async move {
                            cpu.interrupt_reset().await;
                            cpu.run().await
                        }
                    }),
                })
                .with_sub(ClockedFuture {
                    clock_mul: 1,
                    future: Box::pin({
                        let mem = Rc::clone(&self.mem);
                        async move { mem.run().await }
                    }),
                })
        }
    }

    fn create_tester(program: &[u8]) -> Tester {
        let bus = Rc::new(Bus::new());

        // Memory that holds the testing program
        let mem = Ram::mount(
            Rc::clone(&bus),
            RamMountOptions {
                address_range: 0x0000..=0xffff,
                address_mask: !0,
            },
        );
        mem.load(0x0000, program);
        let mem = Rc::new(mem);

        let cpu = Rc::new(Cpu::mount(Rc::clone(&bus), CpuMountOptions::default()));

        Tester { bus, cpu, mem }
    }

    #[test]
    fn test_0xa9_lda_immediate_load_data() {
        let tester = create_tester(&[0xa9, 0x05, 0x00]);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_a.get(), 0x05);
        assert!(!tester.cpu.state.status.get().contains(Status::ZERO));
        assert!(!tester.cpu.state.status.get().contains(Status::NEGATIVE));
    }

    #[test]
    fn test_0xa9_lda_zero_flag() {
        let tester = create_tester(&[0xa9, 0x00, 0x00]);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert!(tester.cpu.state.status.get().contains(Status::ZERO));
    }

    #[test]
    fn test_0xaa_tax_move_a_to_x() {
        let tester = create_tester(&[0xaa, 0x00]);
        tester.cpu.state.reg_a.set(10);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_x.get(), 10)
    }

    #[test]
    fn test_5_ops_working_together() {
        let tester = create_tester(&[0xa9, 0xc0, 0xaa, 0xe8, 0x00]);
        tester.cpu.state.reg_a.set(0xc0);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_x.get(), 0xc1)
    }

    #[test]
    fn test_inx_overflow() {
        let tester = create_tester(&[0xe8, 0xe8, 0x00]);
        tester.cpu.state.reg_x.set(0xff);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_x.get(), 1)
    }

    #[test]
    fn test_lda_from_memory() {
        let tester = create_tester(&[0xa5, 0x10, 0x00]);
        tester.cpu.bus.write(0x10, 0x55);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_a.get(), 0x55);
    }

    // ===== ADC (Add with Carry) Tests =====

    #[test]
    fn test_0x69_adc_immediate() {
        let tester = create_tester(&[0x69, 0x50, 0x00]);
        tester.cpu.state.reg_a.set(0x30);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_a.get(), 0x80);
    }

    #[test]
    fn test_0x69_adc_immediate_with_zero_result() {
        let tester = create_tester(&[0x69, 0x00, 0x00]);
        tester.cpu.state.reg_a.set(0x00);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert!(tester.cpu.state.status.get().contains(Status::ZERO));
    }

    #[test]
    fn test_0x65_adc_zero_page() {
        let tester = create_tester(&[0x65, 0x10, 0x00]);
        tester.cpu.bus.write(0x10, 0x25);
        tester.cpu.state.reg_a.set(0x25);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_a.get(), 0x4a);
    }

    #[test]
    fn test_0x6d_adc_absolute() {
        let tester = create_tester(&[0x6d, 0x20, 0x80, 0x00]);
        tester.cpu.bus.write(0x8020, 0x40);
        tester.cpu.state.reg_a.set(0x10);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_a.get(), 0x50);
    }

    #[test]
    fn test_0x69_adc_carry_flag() {
        // Carry flag is set when addition overflows (result > 255)
        // 0xFF (255) + 0x02 (2) = 0x101 (257 unsigned) -> Carry set, result 0x01
        let tester = create_tester(&[0x69, 0x02, 0x00]);
        tester.cpu.state.reg_a.set(0xff);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_a.get(), 0x01);
        assert!(tester.cpu.state.status.get().contains(Status::CARRY));
    }

    #[test]
    fn test_0x69_adc_no_carry_flag() {
        // Carry flag is not set when addition doesn't overflow (result <= 255)
        // 0x50 (80) + 0x50 (80) = 0xA0 (160) -> No carry
        let tester = create_tester(&[0x69, 0x50, 0x00]);
        tester.cpu.state.reg_a.set(0x50);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_a.get(), 0xa0);
        assert!(!tester.cpu.state.status.get().contains(Status::CARRY));
    }

    #[test]
    fn test_0x69_adc_overflow_positive_plus_positive() {
        // Overflow occurs when adding two positive numbers results in a negative number
        // 0x50 (80 as i8) + 0x40 (64 as i8) = 0x90 (-112 as i8) -> Overflow
        let tester = create_tester(&[0x69, 0x40, 0x00]);
        tester.cpu.state.reg_a.set(0x50);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_a.get(), 0x90);
        assert!(tester.cpu.state.status.get().contains(Status::OVERFLOW));
    }

    #[test]
    fn test_0x69_adc_overflow_negative_plus_negative() {
        // Overflow occurs when adding two negative numbers results in a positive number
        // 0xB0 (-80 as i8) + 0xC0 (-64 as i8) = 0x70 (112 as i8) -> Overflow
        let tester = create_tester(&[0x69, 0xc0, 0x00]);
        tester.cpu.state.reg_a.set(0xb0);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_a.get(), 0x70);
        assert!(tester.cpu.state.status.get().contains(Status::OVERFLOW));
    }

    #[test]
    fn test_0x69_adc_no_overflow_positive_plus_negative() {
        // No overflow when adding positive and negative numbers
        // 0x50 (80 as i8) + 0xD0 (-48 as i8) = 0x20 (32 as i8)
        let tester = create_tester(&[0x69, 0xd0, 0x00]);
        tester.cpu.state.reg_a.set(0x50);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_a.get(), 0x20);
        assert!(!tester.cpu.state.status.get().contains(Status::OVERFLOW));
    }

    #[test]
    fn test_0x69_adc_no_overflow_positive_plus_positive_no_sign_change() {
        // No overflow when adding positive numbers that stay positive
        // 0x30 (48 as i8) + 0x20 (32 as i8) = 0x50 (80 as i8) - both positive, stays positive
        let tester = create_tester(&[0x69, 0x20, 0x00]);
        tester.cpu.state.reg_a.set(0x30);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_a.get(), 0x50);
        assert!(!tester.cpu.state.status.get().contains(Status::OVERFLOW));
    }

    // ===== AND (Logical AND) Tests =====

    #[test]
    fn test_0x29_and_immediate() {
        let tester = create_tester(&[0x29, 0x0f, 0x00]);
        tester.cpu.state.reg_a.set(0xf0);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_a.get(), 0x00);
        assert!(tester.cpu.state.status.get().contains(Status::ZERO));
    }

    #[test]
    fn test_0x25_and_zero_page() {
        let tester = create_tester(&[0x25, 0x10, 0x00]);
        tester.cpu.bus.write(0x10, 0x0f);
        tester.cpu.state.reg_a.set(0xf5);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_a.get(), 0x05);
    }

    #[test]
    fn test_0x2d_and_absolute() {
        let tester = create_tester(&[0x2d, 0x20, 0x80, 0x00]);
        tester.cpu.bus.write(0x8020, 0xff);
        tester.cpu.state.reg_a.set(0x55);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_a.get(), 0x55);
    }

    // ===== ASL (Arithmetic Shift Left) Tests =====

    #[test]
    fn test_0x0a_asl_accumulator() {
        let tester = create_tester(&[0x0a, 0x00]);
        tester.cpu.state.reg_a.set(0x02);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_a.get(), 0x04);
    }

    #[test]
    fn test_0x06_asl_zero_page() {
        let tester = create_tester(&[0x06, 0x10, 0x00]);
        tester.cpu.bus.write(0x10, 0x40);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.mem.read(0x10), 0x80);
    }

    #[test]
    fn test_0x0e_asl_absolute() {
        let tester = create_tester(&[0x0e, 0x20, 0x80, 0x00]);
        tester.cpu.bus.write(0x8020, 0x01);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.mem.read(0x8020), 0x02);
    }

    // ===== Branch Instructions Tests =====

    // BEQ (Branch if Equal - ZERO flag set)
    #[test]
    fn test_0xf0_beq_branch_taken() {
        let tester = create_tester(&[0xf0, 0x02, 0xff, 0xff, 0x00]);
        tester
            .cpu
            .state
            .status
            .set(tester.cpu.state.status.get() | Status::ZERO);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        // If branch is not taken by bug, CPU will panic due to unknown opcode 0xff
    }

    #[test]
    fn test_0xf0_beq_branch_not_taken() {
        let tester = create_tester(&[0xf0, 0x02, 0x00]);
        tester.cpu.state.status.set(Status::empty());
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert!(!tester.cpu.state.status.get().contains(Status::ZERO));
    }

    // BNE (Branch if Not Equal - ZERO flag clear)
    #[test]
    fn test_0xd0_bne_branch_taken() {
        let tester = create_tester(&[0xd0, 0x02, 0xff, 0xff, 0x00]);
        tester.cpu.state.status.set(Status::empty());
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        // If branch is not taken by bug, CPU will panic due to unknown opcode 0xff
    }

    #[test]
    fn test_0xd0_bne_branch_not_taken() {
        let tester = create_tester(&[0xd0, 0x02, 0x00]);
        tester
            .cpu
            .state
            .status
            .set(tester.cpu.state.status.get() | Status::ZERO);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert!(tester.cpu.state.status.get().contains(Status::ZERO));
    }

    // BCC (Branch if Carry Clear - CARRY flag clear)
    #[test]
    fn test_0x90_bcc_branch_taken() {
        let tester = create_tester(&[0x90, 0x02, 0xff, 0xff, 0x00]);
        tester.cpu.state.status.set(Status::empty());
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        // If branch is not taken by bug, CPU will panic due to unknown opcode 0xff
    }

    #[test]
    fn test_0x90_bcc_branch_not_taken() {
        let tester = create_tester(&[0x90, 0x02, 0x00]);
        tester
            .cpu
            .state
            .status
            .set(tester.cpu.state.status.get() | Status::CARRY);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert!(tester.cpu.state.status.get().contains(Status::CARRY));
    }

    // BCS (Branch if Carry Set - CARRY flag set)
    #[test]
    fn test_0xb0_bcs_branch_taken() {
        let tester = create_tester(&[0xb0, 0x02, 0xff, 0xff, 0x00]);
        tester
            .cpu
            .state
            .status
            .set(tester.cpu.state.status.get() | Status::CARRY);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        // If branch is not taken by bug, CPU will panic due to unknown opcode 0xff
    }

    #[test]
    fn test_0xb0_bcs_branch_not_taken() {
        let tester = create_tester(&[0xb0, 0x02, 0x00]);
        tester.cpu.state.status.set(Status::empty());
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());
    }

    // BMI (Branch if Minus - NEGATIVE flag set)
    #[test]
    fn test_0x30_bmi_branch_taken() {
        let tester = create_tester(&[0x30, 0x02, 0xff, 0xff, 0x00]);
        tester
            .cpu
            .state
            .status
            .set(tester.cpu.state.status.get() | Status::NEGATIVE);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        // If branch is not taken by bug, CPU will panic due to unknown opcode 0xff
    }

    #[test]
    fn test_0x30_bmi_branch_not_taken() {
        let tester = create_tester(&[0x30, 0x02, 0x00]);
        tester.cpu.state.status.set(Status::empty());
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());
    }

    // BPL (Branch if Plus - NEGATIVE flag clear)
    #[test]
    fn test_0x10_bpl_branch_taken() {
        let tester = create_tester(&[0x10, 0x02, 0xff, 0xff, 0x00]);
        tester.cpu.state.status.set(Status::empty());
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        // If branch is not taken by bug, CPU will panic due to unknown opcode 0xff
    }

    #[test]
    fn test_0x10_bpl_branch_not_taken() {
        let tester = create_tester(&[0x10, 0x02, 0x00]);
        tester
            .cpu
            .state
            .status
            .set(tester.cpu.state.status.get() | Status::NEGATIVE);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert!(tester.cpu.state.status.get().contains(Status::NEGATIVE));
    }

    // BVC (Branch if Overflow Clear - OVERFLOW flag clear)
    #[test]
    fn test_0x50_bvc_branch_taken() {
        let tester = create_tester(&[0x50, 0x02, 0xff, 0xff, 0x00]);
        tester.cpu.state.status.set(Status::empty());
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        // If branch is not taken by bug, CPU will panic due to unknown opcode 0xff
    }

    #[test]
    fn test_0x50_bvc_branch_not_taken() {
        let tester = create_tester(&[0x50, 0x02, 0x00]);
        tester
            .cpu
            .state
            .status
            .set(tester.cpu.state.status.get() | Status::OVERFLOW);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert!(tester.cpu.state.status.get().contains(Status::OVERFLOW));
    }

    // BVS (Branch if Overflow Set - OVERFLOW flag set)
    #[test]
    fn test_0x70_bvs_branch_taken() {
        let tester = create_tester(&[0x70, 0x02, 0xff, 0xff, 0x00]);
        tester
            .cpu
            .state
            .status
            .set(tester.cpu.state.status.get() | Status::OVERFLOW);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        // If branch is not taken by bug, CPU will panic due to unknown opcode 0xff
    }

    #[test]
    fn test_0x70_bvs_branch_not_taken() {
        let tester = create_tester(&[0x70, 0x02, 0x00]);
        tester.cpu.state.status.set(Status::empty());
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());
    }

    // ===== BIT Test =====

    #[test]
    fn test_0x24_bit_zero_page() {
        let tester = create_tester(&[0x24, 0x10, 0x00]);
        tester.cpu.bus.write(0x10, 0xc0);
        tester.cpu.state.reg_a.set(0x3f);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert!(tester.cpu.state.status.get().contains(Status::ZERO));
        assert!(tester.cpu.state.status.get().contains(Status::NEGATIVE));
    }

    #[test]
    fn test_0x2c_bit_absolute() {
        let tester = create_tester(&[0x2c, 0x20, 0x80, 0x00]);
        tester.cpu.bus.write(0x8020, 0x80);
        tester.cpu.state.reg_a.set(0x01);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert!(tester.cpu.state.status.get().contains(Status::ZERO));
        assert!(tester.cpu.state.status.get().contains(Status::NEGATIVE));
    }

    // ===== BRK Test =====

    #[test]
    fn test_0x00_brk() {
        let tester = create_tester(&[0x00]);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());
    }

    // ===== CMP (Compare) Tests =====

    #[test]
    fn test_0xc9_cmp_immediate_equal() {
        let tester = create_tester(&[0xc9, 0x50, 0x00]);
        tester.cpu.state.reg_a.set(0x50);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert!(tester.cpu.state.status.get().contains(Status::CARRY));
        assert!(tester.cpu.state.status.get().contains(Status::ZERO));
        assert!(!tester.cpu.state.status.get().contains(Status::NEGATIVE));
    }

    #[test]
    fn test_0xc5_cmp_zero_page() {
        let tester = create_tester(&[0xc5, 0x10, 0x00]);
        tester.cpu.bus.write(0x10, 0x30);
        tester.cpu.state.reg_a.set(0x40);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());
    }

    #[test]
    fn test_0xcd_cmp_absolute() {
        let tester = create_tester(&[0xcd, 0x20, 0x80, 0x00]);
        tester.cpu.bus.write(0x8020, 0x80);
        tester.cpu.state.reg_a.set(0x80);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert!(tester.cpu.state.status.get().contains(Status::ZERO));
    }

    // ===== CPX (Compare X) Tests =====

    #[test]
    fn test_0xe0_cpx_immediate_equal() {
        let tester = create_tester(&[0xe0, 0x40, 0x00]);
        tester.cpu.state.reg_x.set(0x40);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert!(tester.cpu.state.status.get().contains(Status::ZERO));
    }

    #[test]
    fn test_0xe4_cpx_zero_page() {
        let tester = create_tester(&[0xe4, 0x10, 0x00]);
        tester.cpu.bus.write(0x10, 0x50);
        tester.cpu.state.reg_x.set(0x50);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert!(tester.cpu.state.status.get().contains(Status::ZERO));
    }

    #[test]
    fn test_0xec_cpx_absolute() {
        let tester = create_tester(&[0xec, 0x20, 0x80, 0x00]);
        tester.cpu.bus.write(0x8020, 0x60);
        tester.cpu.state.reg_x.set(0x60);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert!(tester.cpu.state.status.get().contains(Status::ZERO));
    }

    // ===== CPY (Compare Y) Tests =====

    #[test]
    fn test_0xc0_cpy_immediate_equal() {
        let tester = create_tester(&[0xc0, 0x30, 0x00]);
        tester.cpu.state.reg_y.set(0x30);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert!(tester.cpu.state.status.get().contains(Status::ZERO));
    }

    #[test]
    fn test_0xc4_cpy_zero_page() {
        let tester = create_tester(&[0xc4, 0x10, 0x00]);
        tester.cpu.bus.write(0x10, 0x70);
        tester.cpu.state.reg_y.set(0x70);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert!(tester.cpu.state.status.get().contains(Status::ZERO));
    }

    #[test]
    fn test_0xcc_cpy_absolute() {
        let tester = create_tester(&[0xcc, 0x20, 0x80, 0x00]);
        tester.cpu.bus.write(0x8020, 0x90);
        tester.cpu.state.reg_y.set(0x90);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert!(tester.cpu.state.status.get().contains(Status::ZERO));
    }

    // ===== DEC (Decrement) Tests =====

    #[test]
    fn test_0xc6_dec_zero_page() {
        let tester = create_tester(&[0xc6, 0x10, 0x00]);
        tester.cpu.bus.write(0x10, 0x10);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.mem.read(0x10), 0x0f);
    }

    #[test]
    fn test_0xce_dec_absolute() {
        let tester = create_tester(&[0xce, 0x20, 0x80, 0x00]);
        tester.cpu.bus.write(0x8020, 0x01);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.mem.read(0x8020), 0x00);
        assert!(tester.cpu.state.status.get().contains(Status::ZERO));
    }

    #[test]
    fn test_0xc6_dec_zero_page_underflow() {
        let tester = create_tester(&[0xc6, 0x10, 0x00]);
        tester.cpu.bus.write(0x10, 0x00);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.mem.read(0x10), 0xff);
        assert!(tester.cpu.state.status.get().contains(Status::NEGATIVE));
    }

    // ===== DEX (Decrement X) Tests =====

    #[test]
    fn test_0xca_dex() {
        let tester = create_tester(&[0xca, 0x00]);
        tester.cpu.state.reg_x.set(0x10);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_x.get(), 0x0f);
    }

    #[test]
    fn test_0xca_dex_underflow() {
        let tester = create_tester(&[0xca, 0x00]);
        tester.cpu.state.reg_x.set(0x00);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_x.get(), 0xff);
        assert!(tester.cpu.state.status.get().contains(Status::NEGATIVE));
    }

    // ===== DEY (Decrement Y) Tests =====

    #[test]
    fn test_0x88_dey() {
        let tester = create_tester(&[0x88, 0x00]);
        tester.cpu.state.reg_y.set(0x20);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_y.get(), 0x1f);
    }

    #[test]
    fn test_0x88_dey_underflow() {
        let tester = create_tester(&[0x88, 0x00]);
        tester.cpu.state.reg_y.set(0x00);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_y.get(), 0xff);
        assert!(tester.cpu.state.status.get().contains(Status::NEGATIVE));
    }

    // ===== EOR (Exclusive OR) Tests =====

    #[test]
    fn test_0x49_eor_immediate() {
        let tester = create_tester(&[0x49, 0x0f, 0x00]);
        tester.cpu.state.reg_a.set(0xf0);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_a.get(), 0xff);
    }

    #[test]
    fn test_0x45_eor_zero_page() {
        let tester = create_tester(&[0x45, 0x10, 0x00]);
        tester.cpu.bus.write(0x10, 0x55);
        tester.cpu.state.reg_a.set(0xaa);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_a.get(), 0xff);
    }

    #[test]
    fn test_0x4d_eor_absolute() {
        let tester = create_tester(&[0x4d, 0x20, 0x80, 0x00]);
        tester.cpu.bus.write(0x8020, 0xff);
        tester.cpu.state.reg_a.set(0x00);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_a.get(), 0xff);
    }

    // ===== INC (Increment) Tests =====

    #[test]
    fn test_0xe6_inc_zero_page() {
        let tester = create_tester(&[0xe6, 0x10, 0x00]);
        tester.cpu.bus.write(0x10, 0x0f);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.mem.read(0x10), 0x10);
    }

    #[test]
    fn test_0xee_inc_absolute() {
        let tester = create_tester(&[0xee, 0x20, 0x80, 0x00]);
        tester.cpu.bus.write(0x8020, 0xff);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.mem.read(0x8020), 0x00);
        assert!(tester.cpu.state.status.get().contains(Status::ZERO));
    }

    #[test]
    fn test_0xe6_inc_zero_page_to_negative() {
        let tester = create_tester(&[0xe6, 0x10, 0x00]);
        tester.cpu.bus.write(0x10, 0x7f);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.mem.read(0x10), 0x80);
        assert!(tester.cpu.state.status.get().contains(Status::NEGATIVE));
    }

    // ===== INX (Increment X) Tests =====

    #[test]
    fn test_0xe8_inx() {
        let tester = create_tester(&[0xe8, 0x00]);
        tester.cpu.state.reg_x.set(0x20);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_x.get(), 0x21);
    }

    #[test]
    fn test_0xe8_inx_to_negative() {
        let tester = create_tester(&[0xe8, 0x00]);
        tester.cpu.state.reg_x.set(0x7f);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_x.get(), 0x80);
        assert!(tester.cpu.state.status.get().contains(Status::NEGATIVE));
    }

    // ===== INY (Increment Y) Tests =====

    #[test]
    fn test_0xc8_iny() {
        let tester = create_tester(&[0xc8, 0x00]);
        tester.cpu.state.reg_y.set(0x30);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_y.get(), 0x31);
    }

    #[test]
    fn test_0xc8_iny_to_negative() {
        let tester = create_tester(&[0xc8, 0x00]);
        tester.cpu.state.reg_y.set(0x7f);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_y.get(), 0x80);
        assert!(tester.cpu.state.status.get().contains(Status::NEGATIVE));
    }

    // ===== JMP (Jump) Tests =====

    #[test]
    fn test_0x4c_jmp_absolute() {
        let tester = create_tester(&[0x4c, 0x20, 0x80, 0xff]);
        tester.cpu.bus.write(0x8020, 0x00);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());
    }

    #[test]
    fn test_0x6c_jmp_indirect() {
        let tester = create_tester(&[0x6c, 0x10, 0xff, 0x00]);
        tester.cpu.bus.write_u16(0x10, 0x8002);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());
    }

    // ===== JSR (Jump to Subroutine) Tests =====

    #[test]
    fn test_0x20_jsr() {
        let tester = create_tester(&[0x20, 0x20, 0x80, 0x00]);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        // return_addr should be the last byte of the JSR instruction
        let return_addr = tester.cpu.stack_pop_u16();
        assert_eq!(return_addr, 0x8002);
    }

    // ===== LDA (Load Accumulator) Tests =====

    #[test]
    fn test_0xa5_lda_zero_page() {
        let tester = create_tester(&[0xa5, 0x10, 0x00]);
        tester.cpu.bus.write(0x10, 0x42);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_a.get(), 0x42);
    }

    #[test]
    fn test_0xad_lda_absolute() {
        let tester = create_tester(&[0xad, 0x20, 0x80, 0x00]);
        tester.cpu.bus.write(0x8020, 0x55);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_a.get(), 0x55);
    }

    #[test]
    fn test_0xb5_lda_zero_page_x() {
        let tester = create_tester(&[0xb5, 0x10, 0x00]);
        tester.cpu.bus.write(0x15, 0x77);
        tester.cpu.state.reg_x.set(0x05);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_a.get(), 0x77);
    }

    #[test]
    fn test_0xbd_lda_absolute_x() {
        let tester = create_tester(&[0xbd, 0x20, 0x80, 0x00]);
        tester.cpu.bus.write(0x8025, 0xaa);
        tester.cpu.state.reg_x.set(0x05);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_a.get(), 0xaa);
    }

    #[test]
    fn test_0xb9_lda_absolute_y() {
        let tester = create_tester(&[0xb9, 0x20, 0x80, 0x00]);
        tester.cpu.bus.write(0x8030, 0xbb);
        tester.cpu.state.reg_y.set(0x10);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_a.get(), 0xbb);
    }

    // ===== LDX (Load X) Tests =====

    #[test]
    fn test_0xa2_ldx_immediate() {
        let tester = create_tester(&[0xa2, 0x44, 0x00]);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_x.get(), 0x44);
    }

    #[test]
    fn test_0xa6_ldx_zero_page() {
        let tester = create_tester(&[0xa6, 0x10, 0x00]);
        tester.cpu.bus.write(0x10, 0x66);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_x.get(), 0x66);
    }

    #[test]
    fn test_0xae_ldx_absolute() {
        let tester = create_tester(&[0xae, 0x20, 0x80, 0x00]);
        tester.cpu.bus.write(0x8020, 0x88);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_x.get(), 0x88);
    }

    #[test]
    fn test_0xb6_ldx_zero_page_y() {
        let tester = create_tester(&[0xb6, 0x10, 0x00]);
        tester.cpu.bus.write(0x15, 0xcc);
        tester.cpu.state.reg_y.set(0x05);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_x.get(), 0xcc);
    }

    #[test]
    fn test_0xbe_ldx_absolute_y() {
        let tester = create_tester(&[0xbe, 0x20, 0x80, 0x00]);
        tester.cpu.bus.write(0x8025, 0xdd);
        tester.cpu.state.reg_y.set(0x05);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_x.get(), 0xdd);
    }

    // ===== LDY (Load Y) Tests =====

    #[test]
    fn test_0xa0_ldy_immediate() {
        let tester = create_tester(&[0xa0, 0x33, 0x00]);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_y.get(), 0x33);
    }

    #[test]
    fn test_0xa4_ldy_zero_page() {
        let tester = create_tester(&[0xa4, 0x10, 0x00]);
        tester.cpu.bus.write(0x10, 0x55);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_y.get(), 0x55);
    }

    #[test]
    fn test_0xac_ldy_absolute() {
        let tester = create_tester(&[0xac, 0x20, 0x80, 0x00]);
        tester.cpu.bus.write(0x8020, 0x77);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_y.get(), 0x77);
    }

    #[test]
    fn test_0xb4_ldy_zero_page_x() {
        let tester = create_tester(&[0xb4, 0x10, 0x00]);
        tester.cpu.bus.write(0x15, 0x99);
        tester.cpu.state.reg_x.set(0x05);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_y.get(), 0x99);
    }

    #[test]
    fn test_0xbc_ldy_absolute_x() {
        let tester = create_tester(&[0xbc, 0x20, 0x80, 0x00]);
        tester.cpu.bus.write(0x8030, 0xee);
        tester.cpu.state.reg_x.set(0x10);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_y.get(), 0xee);
    }

    // ===== LSR (Logical Shift Right) Tests =====

    #[test]
    fn test_0x4a_lsr_accumulator() {
        let tester = create_tester(&[0x4a, 0x00]);
        tester.cpu.state.reg_a.set(0x04);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_a.get(), 0x02);
    }

    #[test]
    fn test_0x46_lsr_zero_page() {
        let tester = create_tester(&[0x46, 0x10, 0x00]);
        tester.cpu.bus.write(0x10, 0x80);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.mem.read(0x10), 0x40);
    }

    #[test]
    fn test_0x4e_lsr_absolute() {
        let tester = create_tester(&[0x4e, 0x20, 0x80, 0x00]);
        tester.cpu.bus.write(0x8020, 0x02);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.mem.read(0x8020), 0x01);
    }

    // ===== NOP (No Operation) Tests =====

    #[test]
    fn test_0xea_nop() {
        let tester = create_tester(&[0xea, 0x00]);
        let reg_a_before = tester.cpu.state.reg_a.get();
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_a.get(), reg_a_before);
    }

    // ===== ORA (Logical OR) Tests =====

    #[test]
    fn test_0x09_ora_immediate() {
        let tester = create_tester(&[0x09, 0x0f, 0x00]);
        tester.cpu.state.reg_a.set(0xf0);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_a.get(), 0xff);
    }

    #[test]
    fn test_0x05_ora_zero_page() {
        let tester = create_tester(&[0x05, 0x10, 0x00]);
        tester.cpu.bus.write(0x10, 0x0f);
        tester.cpu.state.reg_a.set(0xf0);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_a.get(), 0xff);
    }

    #[test]
    fn test_0x0d_ora_absolute() {
        let tester = create_tester(&[0x0d, 0x20, 0x80, 0x00]);
        tester.cpu.bus.write(0x8020, 0x55);
        tester.cpu.state.reg_a.set(0xaa);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_a.get(), 0xff);
    }

    // ===== PHA (Push Accumulator) Tests =====

    #[test]
    fn test_0x48_pha() {
        let tester = create_tester(&[0x48, 0x00]);
        tester.cpu.state.reg_a.set(0x42);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());
    }

    // ===== PHP (Push Processor Status) Tests =====

    #[test]
    fn test_0x08_php() {
        let tester = create_tester(&[0x08, 0x00]);
        tester.cpu.state.status.set(Status::ZERO | Status::NEGATIVE);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());
    }

    // ===== PLA (Pull Accumulator) Tests =====

    #[test]
    fn test_0x68_pla() {
        let tester = create_tester(&[0x68, 0x00]);
        tester.cpu.bus.write(0x01ff, 0x42);
        tester.cpu.state.sp.set(0xfe);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_a.get(), 0x42);
    }

    // ===== PLP (Pull Processor Status) Tests =====

    #[test]
    fn test_0x28_plp() {
        let tester = create_tester(&[0x28, 0x00]);
        tester
            .cpu
            .bus
            .write(0x01ff, (Status::INTERRUPT_DISABLE | Status::B_FLAG).bits());
        tester.cpu.state.sp.set(0xfe);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        // B Flag should be ignored when pulling status
        // RESERVED flag
        assert_eq!(
            tester.cpu.state.status.get(),
            Status::INTERRUPT_DISABLE | Status::RESERVED
        );
    }

    // ===== ROL (Rotate Left) Tests =====

    #[test]
    fn test_0x2a_rol_accumulator() {
        let tester = create_tester(&[0x2a, 0x00]);
        tester.cpu.state.reg_a.set(0x40);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());
    }

    #[test]
    fn test_0x26_rol_zero_page() {
        let tester = create_tester(&[0x26, 0x10, 0x00]);
        tester.cpu.bus.write(0x10, 0x40);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());
    }

    #[test]
    fn test_0x2e_rol_absolute() {
        let tester = create_tester(&[0x2e, 0x20, 0x80, 0x00]);
        tester.cpu.bus.write(0x8020, 0x40);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());
    }

    // ===== ROR (Rotate Right) Tests =====

    #[test]
    fn test_0x6a_ror_accumulator() {
        let tester = create_tester(&[0x6a, 0x00]);
        tester.cpu.state.reg_a.set(0x02);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());
    }

    #[test]
    fn test_0x66_ror_zero_page() {
        let tester = create_tester(&[0x66, 0x10, 0x00]);
        tester.cpu.bus.write(0x10, 0x02);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());
    }

    #[test]
    fn test_0x6e_ror_absolute() {
        let tester = create_tester(&[0x6e, 0x20, 0x80, 0x00]);
        tester.cpu.bus.write(0x8020, 0x02);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());
    }

    // ===== RTI (Return from Interrupt) Tests =====

    #[test]
    fn test_0x40_rti() {
        // RTI pops status and PC from stack
        // Load program with RTI instruction at 0x8000
        let tester = create_tester(&[0x40, 0xff, 0xff, 0x00]);

        // Set up stack with expected return address and status
        // RTI will pop in reverse order: first status, then PC (lo then hi)
        let return_addr = 0x8003;
        let expected_status = Status::ZERO | Status::NEGATIVE;

        // Push values onto stack (push_u16 pushes hi then lo, so stack will be: hi, lo)
        tester.cpu.bus.write_u16(0x01fe, return_addr);
        tester.cpu.bus.write(0x01fd, expected_status.bits());
        tester.cpu.state.sp.set(0xfc);

        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        // Verify RTI restored the status and PC correctly
        assert_eq!(
            tester.cpu.state.status.get(),
            expected_status | Status::RESERVED
        );
        assert_eq!(tester.cpu.state.pc.get(), return_addr + 1); // CPU stops at 0x8003 BRK so PC should be 0x8004
    }

    // ===== RTS (Return from Subroutine) Tests =====

    #[test]
    fn test_0x60_rts() {
        // RTS pops PC from stack
        // Load program with RTS instruction at 0x80bus00
        let tester = create_tester(&[0x60, 0xff, 0xff, 0xff, 0xff, 0x00, 0xff]);

        // Set up stack with return address (minus one)
        tester.cpu.bus.write_u16(0x01fe, 0x8004);
        tester.cpu.state.sp.set(0xfd);

        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        // Verify RTS restored the PC correctly (and then executed BRK)
        assert_eq!(tester.cpu.state.pc.get(), 0x8006);
    }

    // ===== SBC (Subtract with Carry) Tests =====

    #[test]
    fn test_0xe9_sbc_immediate() {
        let tester = create_tester(&[0xe9, 0x30, 0x00]);
        tester.cpu.state.reg_a.set(0x50);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());
    }

    #[test]
    fn test_0xe5_sbc_zero_page() {
        let tester = create_tester(&[0xe5, 0x10, 0x00]);
        tester.cpu.bus.write(0x10, 0x20);
        tester.cpu.state.reg_a.set(0x50);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());
    }

    #[test]
    fn test_0xed_sbc_absolute() {
        let tester = create_tester(&[0xed, 0x20, 0x80, 0x00]);
        tester.cpu.bus.write(0x8020, 0x30);
        tester.cpu.state.reg_a.set(0x60);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());
    }

    #[test]
    fn test_0xe9_sbc_carry_flag_borrow() {
        // In 6502, SBC uses carry as borrow. Carry clear means borrow occurred.
        // 0x50 (80) - 0xFF (255) requires borrow, so carry is cleared
        let tester = create_tester(&[0xe9, 0xff, 0x00]);
        tester.cpu.state.reg_a.set(0x50);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        // Result: 0x50 - 0xFF - 1 (borrow) = 0x50
        assert!(!tester.cpu.state.status.get().contains(Status::CARRY));
    }

    #[test]
    fn test_0xe9_sbc_carry_flag_no_borrow() {
        // Carry is set when subtraction doesn't require borrow
        // 0x50 (80) - 0x30 (48) = 0x20 (32) -> No borrow
        let tester = create_tester(&[0xe9, 0x30, 0x00]);
        tester.cpu.state.reg_a.set(0x50);
        tester
            .cpu
            .state
            .status
            .set(tester.cpu.state.status.get() | Status::CARRY); // Set carry before operation to prevent borrow
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_a.get(), 0x20);
        assert!(tester.cpu.state.status.get().contains(Status::CARRY));
    }

    #[test]
    fn test_0xe9_sbc_overflow_positive_minus_negative() {
        // Overflow occurs when subtracting a negative number from a positive number results in negative
        // 0x50 (80 as i8) - 0xC0 (-64 as i8) = 0x50 - (-64) = 0x90 (-112 as i8) -> Overflow
        let tester = create_tester(&[0xe9, 0xc0, 0x00]);
        tester.cpu.state.reg_a.set(0x50);
        tester
            .cpu
            .state
            .status
            .set(tester.cpu.state.status.get() | Status::CARRY); // Set carry to avoid borrow
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_a.get(), 0x90);
        assert!(tester.cpu.state.status.get().contains(Status::OVERFLOW));
    }

    #[test]
    fn test_0xe9_sbc_overflow_negative_minus_positive() {
        // Overflow occurs when subtracting a positive number from a negative number results in positive
        // 0xC0 (-64 as i8) - 0x50 (80 as i8) = 0xC0 - 80 = 0x70 (112 as i8) -> Overflow
        let tester = create_tester(&[0xe9, 0x50, 0x00]);
        tester.cpu.state.reg_a.set(0xc0);
        tester
            .cpu
            .state
            .status
            .set(tester.cpu.state.status.get() | Status::CARRY); // Set carry to avoid borrow
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_a.get(), 0x70);
        assert!(tester.cpu.state.status.get().contains(Status::OVERFLOW));
    }

    #[test]
    fn test_0xe9_sbc_no_overflow_positive_minus_positive() {
        // No overflow when subtracting positive from positive (both positive)
        // 0x50 (80 as i8) - 0x20 (32 as i8) = 0x30 (48 as i8)
        let tester = create_tester(&[0xe9, 0x20, 0x00]);
        tester.cpu.state.reg_a.set(0x50);
        tester
            .cpu
            .state
            .status
            .set(tester.cpu.state.status.get() | Status::CARRY); // Set carry to avoid borrow
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_a.get(), 0x30);
        assert!(!tester.cpu.state.status.get().contains(Status::OVERFLOW));
    }

    #[test]
    fn test_0xe9_sbc_no_overflow_negative_minus_negative() {
        // No overflow when subtracting negative from negative
        // 0xD0 (-48 as i8) - 0xA0 (-96 as i8) = 0x30 (48 as i8)
        let tester = create_tester(&[0xe9, 0xa0, 0x00]);
        tester.cpu.state.reg_a.set(0xd0);
        tester
            .cpu
            .state
            .status
            .set(tester.cpu.state.status.get() | Status::CARRY); // Set carry to avoid borrow
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_a.get(), 0x30);
        assert!(!tester.cpu.state.status.get().contains(Status::OVERFLOW));
    }

    // ===== SEC (Set Carry) Tests =====

    #[test]
    fn test_0x38_sec() {
        let tester = create_tester(&[0x38, 0x00]);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());
    }

    // ===== SED (Set Decimal) Tests =====

    #[test]
    fn test_0xf8_sed() {
        let tester = create_tester(&[0xf8, 0x00]);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());
    }

    // ===== SEI (Set Interrupt Disable) Tests =====

    #[test]
    fn test_0x78_sei() {
        let tester = create_tester(&[0x78, 0x00]);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());
    }

    // ===== CLC (Clear Carry) Tests =====

    #[test]
    fn test_0x18_clc() {
        let tester = create_tester(&[0x18, 0x00]);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());
    }

    // ===== CLD (Clear Decimal) Tests =====

    #[test]
    fn test_0xd8_cld() {
        let tester = create_tester(&[0xd8, 0x00]);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());
    }

    // ===== CLI (Clear Interrupt Disable) Tests =====

    #[test]
    fn test_0x58_cli() {
        let tester = create_tester(&[0x58, 0x00]);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());
    }

    // ===== CLV (Clear Overflow) Tests =====

    #[test]
    fn test_0xb8_clv() {
        let tester = create_tester(&[0xb8, 0x00]);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());
    }

    // ===== STA (Store Accumulator) Tests =====

    #[test]
    fn test_0x85_sta_zero_page() {
        let tester = create_tester(&[0x85, 0x10, 0x00]);
        tester.cpu.state.reg_a.set(0x42);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.mem.read(0x10), 0x42);
    }

    #[test]
    fn test_0x8d_sta_absolute() {
        let tester = create_tester(&[0x8d, 0x20, 0x80, 0x00]);
        tester.cpu.state.reg_a.set(0x55);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.mem.read(0x8020), 0x55);
    }

    #[test]
    fn test_0x95_sta_zero_page_x() {
        let tester = create_tester(&[0x95, 0x10, 0x00]);
        tester.cpu.state.reg_a.set(0x77);
        tester.cpu.state.reg_x.set(0x05);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.mem.read(0x15), 0x77);
    }

    #[test]
    fn test_0x9d_sta_absolute_x() {
        let tester = create_tester(&[0x9d, 0x20, 0x80, 0x00]);
        tester.cpu.state.reg_a.set(0xaa);
        tester.cpu.state.reg_x.set(0x05);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.mem.read(0x8025), 0xaa);
    }

    #[test]
    fn test_0x99_sta_absolute_y() {
        let tester = create_tester(&[0x99, 0x20, 0x80, 0x00]);
        tester.cpu.state.reg_a.set(0xbb);
        tester.cpu.state.reg_y.set(0x10);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.mem.read(0x8030), 0xbb);
    }

    // ===== STX (Store X) Tests =====

    #[test]
    fn test_0x86_stx_zero_page() {
        let tester = create_tester(&[0x86, 0x10, 0x00]);
        tester.cpu.state.reg_x.set(0x44);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.mem.read(0x10), 0x44);
    }

    #[test]
    fn test_0x8e_stx_absolute() {
        let tester = create_tester(&[0x8e, 0x20, 0x80, 0x00]);
        tester.cpu.state.reg_x.set(0x66);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.mem.read(0x8020), 0x66);
    }

    #[test]
    fn test_0x96_stx_zero_page_y() {
        let tester = create_tester(&[0x96, 0x10, 0x00]);
        tester.cpu.state.reg_x.set(0x88);
        tester.cpu.state.reg_y.set(0x05);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.mem.read(0x15), 0x88);
    }

    // ===== STY (Store Y) Tests =====

    #[test]
    fn test_0x84_sty_zero_page() {
        let tester = create_tester(&[0x84, 0x10, 0x00]);
        tester.cpu.state.reg_y.set(0x33);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.mem.read(0x10), 0x33);
    }

    #[test]
    fn test_0x8c_sty_absolute() {
        let tester = create_tester(&[0x8c, 0x20, 0x80, 0x00]);
        tester.cpu.state.reg_y.set(0x55);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.mem.read(0x8020), 0x55);
    }

    #[test]
    fn test_0x94_sty_zero_page_x() {
        let tester = create_tester(&[0x94, 0x10, 0x00]);
        tester.cpu.state.reg_y.set(0x77);
        tester.cpu.state.reg_x.set(0x05);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.mem.read(0x15), 0x77);
    }

    // ===== TAX (Transfer A to X) Tests =====

    #[test]
    fn test_0xaa_tax_non_zero() {
        let tester = create_tester(&[0xaa, 0x00]);
        tester.cpu.state.reg_a.set(0x42);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_x.get(), 0x42);
        assert!(!tester.cpu.state.status.get().contains(Status::ZERO));
    }

    #[test]
    fn test_0xaa_tax_zero() {
        let tester = create_tester(&[0xaa, 0x00]);
        tester.cpu.state.reg_a.set(0x00);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_x.get(), 0x00);
        assert!(tester.cpu.state.status.get().contains(Status::ZERO));
    }

    #[test]
    fn test_0xaa_tax_negative() {
        let tester = create_tester(&[0xaa, 0x00]);
        tester.cpu.state.reg_a.set(0x80);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_x.get(), 0x80);
        assert!(tester.cpu.state.status.get().contains(Status::NEGATIVE));
    }

    // ===== TAY (Transfer A to Y) Tests =====

    #[test]
    fn test_0xa8_tay() {
        let tester = create_tester(&[0xa8, 0x00]);
        tester.cpu.state.reg_a.set(0x50);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_y.get(), 0x50);
    }

    #[test]
    fn test_0xa8_tay_zero() {
        let tester = create_tester(&[0xa8, 0x00]);
        tester.cpu.state.reg_a.set(0x00);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_y.get(), 0x00);
        assert!(tester.cpu.state.status.get().contains(Status::ZERO));
    }

    // ===== TSX (Transfer Stack Pointer to X) Tests =====

    #[test]
    fn test_0xba_tsx() {
        let tester = create_tester(&[0xba, 0x00]);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());
    }

    // ===== TXA (Transfer X to A) Tests =====

    #[test]
    fn test_0x8a_txa() {
        let tester = create_tester(&[0x8a, 0x00]);
        tester.cpu.state.reg_x.set(0x60);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_a.get(), 0x60);
    }

    #[test]
    fn test_0x8a_txa_zero() {
        let tester = create_tester(&[0x8a, 0x00]);
        tester.cpu.state.reg_x.set(0x00);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_a.get(), 0x00);
        assert!(tester.cpu.state.status.get().contains(Status::ZERO));
    }

    // ===== TXS (Transfer X to Stack Pointer) Tests =====

    #[test]
    fn test_0x9a_txs() {
        let tester = create_tester(&[0x9a, 0x00]);
        tester.cpu.state.reg_x.set(0x70);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());
    }

    // ===== TYA (Transfer Y to A) Tests =====

    #[test]
    fn test_0x98_tya() {
        let tester = create_tester(&[0x98, 0x00]);
        tester.cpu.state.reg_y.set(0x80);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_a.get(), 0x80);
        assert!(tester.cpu.state.status.get().contains(Status::NEGATIVE));
    }

    #[test]
    fn test_0x98_tya_zero() {
        let tester = create_tester(&[0x98, 0x00]);
        tester.cpu.state.reg_y.set(0x00);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_a.get(), 0x00);
        assert!(tester.cpu.state.status.get().contains(Status::ZERO));
    }

    // ===== Indexed Indirect Addressing Mode Tests =====

    #[test]
    fn test_0xa1_lda_indexed_indirect() {
        let tester = create_tester(&[0xa1, 0x10, 0x00]);
        tester.cpu.bus.write_u16(0x15, 0x8020);
        tester.cpu.bus.write(0x8020, 0x42);
        tester.cpu.state.reg_x.set(0x05);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_a.get(), 0x42);
    }

    #[test]
    fn test_0xb1_lda_indirect_indexed() {
        let tester = create_tester(&[0xb1, 0x10, 0x00]);
        tester.cpu.bus.write_u16(0x10, 0x8020);
        tester.cpu.bus.write(0x8025, 0x55);
        tester.cpu.state.reg_y.set(0x05);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.cpu.state.reg_a.get(), 0x55);
    }

    #[test]
    fn test_0x81_sta_indexed_indirect() {
        let tester = create_tester(&[0x81, 0x10, 0x00]);
        tester.cpu.bus.write_u16(0x15, 0x8020);
        tester.cpu.state.reg_a.set(0x99);
        tester.cpu.state.reg_x.set(0x05);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.mem.read(0x8020), 0x99);
    }

    #[test]
    fn test_0x91_sta_indirect_indexed() {
        let tester = create_tester(&[0x91, 0x10, 0x00]);
        tester.cpu.bus.write_u16(0x10, 0x8020);
        tester.cpu.state.reg_a.set(0xcc);
        tester.cpu.state.reg_y.set(0x05);
        let mut rt = Runtime::new();
        rt.run(tester.to_schedule());

        assert_eq!(tester.mem.read(0x8025), 0xcc);
    }

    // ===== Disassemble Tests by Addressing Mode =====

    #[test]
    fn disassemble_implied() {
        let tester = create_tester(&[]);

        // Example: NOP (nestest.log: "NOP")
        let result = debug_disassemble(&tester.cpu, &tester.mem, &[0xea]); // NOP
        assert_eq!(result.repr, "NOP");
        assert_eq!(result.addr_value_hint, None);

        // Example: SEC (nestest.log: "SEC")
        let result = debug_disassemble(&tester.cpu, &tester.mem, &[0x38]); // SEC
        assert_eq!(result.repr, "SEC");
        assert_eq!(result.addr_value_hint, None);

        // Example: CLC (nestest.log: "CLC")
        let result = debug_disassemble(&tester.cpu, &tester.mem, &[0x18]); // CLC
        assert_eq!(result.repr, "CLC");
        assert_eq!(result.addr_value_hint, None);
    }

    #[test]
    fn disassemble_accumulator() {
        let tester = create_tester(&[]);

        // Example: ASL A (nestest.log: "ASL A")
        let result = debug_disassemble(&tester.cpu, &tester.mem, &[0x0a]); // ASL A
        assert_eq!(result.repr, "ASL A");
        assert_eq!(result.addr_value_hint, None);

        // Example: LSR A (nestest.log: "LSR A")
        let result = debug_disassemble(&tester.cpu, &tester.mem, &[0x4a]); // LSR A
        assert_eq!(result.repr, "LSR A");
        assert_eq!(result.addr_value_hint, None);

        // Example: ROR A (nestest.log: "ROR A")
        let result = debug_disassemble(&tester.cpu, &tester.mem, &[0x6a]); // ROR A
        assert_eq!(result.repr, "ROR A");
        assert_eq!(result.addr_value_hint, None);
    }

    #[test]
    fn disassemble_immediate() {
        let tester = create_tester(&[]);

        // Example: LDA #$00 (nestest.log: "LDA #$00")
        let result = debug_disassemble(&tester.cpu, &tester.mem, &[0xa9, 0x00]); // LDA #$00
        assert_eq!(result.repr, "LDA #$00");
        assert_eq!(result.addr_value_hint, None);

        // Example: LDA #$40
        let result = debug_disassemble(&tester.cpu, &tester.mem, &[0xa9, 0x40]); // LDA #$40
        assert_eq!(result.repr, "LDA #$40");
        assert_eq!(result.addr_value_hint, None);

        // Example: LDA #$FF
        let result = debug_disassemble(&tester.cpu, &tester.mem, &[0xa9, 0xff]); // LDA #$FF
        assert_eq!(result.repr, "LDA #$FF");
        assert_eq!(result.addr_value_hint, None);

        // Example: AND #$EF
        let result = debug_disassemble(&tester.cpu, &tester.mem, &[0x29, 0xef]); // AND #$EF
        assert_eq!(result.repr, "AND #$EF");
        assert_eq!(result.addr_value_hint, None);
    }

    #[test]
    fn disassemble_zero_page() {
        let tester = create_tester(&[]);
        tester.cpu.bus.write(0x00, 0x00);
        tester.cpu.bus.write(0x01, 0xff);
        tester.cpu.bus.write(0x10, 0x00);

        // Example: LDA $00 (nestest.log: "LDA $00 = 00")
        let result = debug_disassemble(&tester.cpu, &tester.mem, &[0xa5, 0x00]); // LDA $00
        assert_eq!(result.repr, "LDA $00");
        assert_eq!(result.addr_value_hint, Some("= 00".to_string()));

        // Example: STA $01 = FF
        let result = debug_disassemble(&tester.cpu, &tester.mem, &[0x85, 0x01]); // STA $01
        assert_eq!(result.repr, "STA $01");
        assert_eq!(result.addr_value_hint, Some("= FF".to_string()));

        // Example: BIT $01 = FF
        let result = debug_disassemble(&tester.cpu, &tester.mem, &[0x24, 0x10]); // BIT $10
        assert_eq!(result.repr, "BIT $10");
        assert_eq!(result.addr_value_hint, Some("= 00".to_string()));
    }

    #[test]
    fn disassemble_zero_page_x() {
        let tester = create_tester(&[]);
        tester.cpu.state.reg_x.set(0x10);
        tester.cpu.bus.write(0x15, 0xaa);

        // Example: STY $33,X @ 33 = AA (nestest.log format)
        let result = debug_disassemble(&tester.cpu, &tester.mem, &[0xb5, 0x05]); // LDA $05,X
        assert_eq!(result.repr, "LDA $05,X");
        assert_eq!(result.addr_value_hint, Some("@ 15 = AA".to_string()));

        tester.cpu.state.reg_x.set(0x00);
        let result = debug_disassemble(&tester.cpu, &tester.mem, &[0x86, 0x00]); // STX $00
        assert_eq!(result.repr, "STX $00");
        assert_eq!(result.addr_value_hint, Some("= 00".to_string()));
    }

    #[test]
    fn disassemble_zero_page_y() {
        let tester = create_tester(&[]);
        tester.cpu.state.reg_y.set(0x10);
        tester.cpu.bus.write(0x15, 0xbb);

        // Example: LDX $00,Y @ 78 = 33 (nestest.log format)
        let result = debug_disassemble(&tester.cpu, &tester.mem, &[0xb6, 0x05]); // LDX $05,Y
        assert_eq!(result.repr, "LDX $05,Y");
        assert_eq!(result.addr_value_hint, Some("@ 15 = BB".to_string()));

        tester.cpu.state.reg_y.set(0x00);
        tester.cpu.bus.write(0x01, 0xff);
        let result = debug_disassemble(&tester.cpu, &tester.mem, &[0x96, 0x01]); // STX $01,Y
        assert_eq!(result.repr, "STX $01,Y");
        assert_eq!(result.addr_value_hint, Some("@ 01 = FF".to_string()));
    }

    #[test]
    fn disassemble_absolute() {
        let tester = create_tester(&[]);
        tester.cpu.bus.write(0x8020, 0x42);

        // Example: JMP $8020 = 42 (4-digit hex address)
        let result = debug_disassemble(&tester.cpu, &tester.mem, &[0x4c, 0x20, 0x80]); // JMP $8020
        assert_eq!(result.repr, "JMP $8020");
        assert_eq!(result.addr_value_hint, None);

        let result = debug_disassemble(&tester.cpu, &tester.mem, &[0xad, 0x20, 0x80]); // LDA $8020
        assert_eq!(result.repr, "LDA $8020");
        assert_eq!(result.addr_value_hint, Some("= 42".to_string()));
    }

    #[test]
    fn disassemble_absolute_x() {
        let tester = create_tester(&[]);
        tester.cpu.state.reg_x.set(0x10);
        tester.cpu.bus.write(0x0633, 0x99);

        // Example: LDY $33,X @ 33 = AA (4-digit result address)
        let result = debug_disassemble(&tester.cpu, &tester.mem, &[0xbc, 0x23, 0x06]); // LDY $0623,X
        assert_eq!(result.repr, "LDY $0623,X");
        assert_eq!(result.addr_value_hint, Some("@ 0633 = 99".to_string()));
    }

    #[test]
    fn disassemble_absolute_y() {
        let tester = create_tester(&[]);
        tester.cpu.state.reg_y.set(0x10);
        tester.cpu.bus.write(0x0610, 0x77);

        // Example: LDX $0600,Y @ 0610 = 77
        let result = debug_disassemble(&tester.cpu, &tester.mem, &[0xbe, 0x00, 0x06]); // LDX $0600,Y
        assert_eq!(result.repr, "LDX $0600,Y");
        assert_eq!(result.addr_value_hint, Some("@ 0610 = 77".to_string()));
    }

    #[test]
    fn disassemble_relative() {
        let tester = create_tester(&[]);
        tester.cpu.state.pc.set(0x8000);

        // Example: BCS $8005
        let result = debug_disassemble(&tester.cpu, &tester.mem, &[0xb0, 0x03]);
        assert_eq!(result.repr, "BCS $8005");
        assert_eq!(result.addr_value_hint, None);

        // Negative offset test
        tester.cpu.state.pc.set(0x8010);
        tester.cpu.bus.write(0x800C, 0xaa);
        let result = debug_disassemble(&tester.cpu, &tester.mem, &[0xf0, 0xfc]);
        assert_eq!(result.repr, "BEQ $800E");
        assert_eq!(result.addr_value_hint, None);
    }

    #[test]
    fn disassemble_indirect() {
        let tester = create_tester(&[]);
        tester.cpu.bus.write_u16(0x0200, 0xdb7e);

        // Example: JMP ($0200) = DB7E (nestest.log format - 4 digit result)
        let result = debug_disassemble(&tester.cpu, &tester.mem, &[0x6c, 0x00, 0x02]); // JMP ($0200)
        assert_eq!(result.repr, "JMP ($0200)");
        assert_eq!(result.addr_value_hint, Some("= DB7E".to_string()));
    }

    #[test]
    fn disassemble_indexed_indirect() {
        let tester = create_tester(&[]);
        tester.cpu.state.reg_x.set(0x00);
        tester.cpu.bus.write_u16(0x80, 0x0200);
        tester.cpu.bus.write(0x0200, 0x5a);

        // Example: LDA ($80,X) @ 80 = 0200 = 5A (nestest.log format)
        let result = debug_disassemble(&tester.cpu, &tester.mem, &[0xa1, 0x80]); // LDA ($80,X)
        assert_eq!(result.repr, "LDA ($80,X)");
        assert_eq!(result.addr_value_hint, Some("@ 80 = 0200 = 5A".to_string()));

        // Test with X offset
        tester.cpu.state.reg_x.set(0x02);
        tester.cpu.bus.write_u16(0x82, 0x0300);
        tester.cpu.bus.write(0x0300, 0x5b);
        let result = debug_disassemble(&tester.cpu, &tester.mem, &[0xa1, 0x80]); // LDA ($80,X) with X=2
        assert_eq!(result.repr, "LDA ($80,X)");
        assert_eq!(result.addr_value_hint, Some("@ 82 = 0300 = 5B".to_string()));
    }

    #[test]
    fn disassemble_indirect_indexed() {
        let tester = create_tester(&[]);
        tester.cpu.state.reg_y.set(0x00);
        tester.cpu.bus.write_u16(0x89, 0x0300);
        tester.cpu.bus.write(0x0300, 0x89);

        // Example: LDA ($89),Y = 0300 @ 0300 = 89 (nestest.log format)
        let result = debug_disassemble(&tester.cpu, &tester.mem, &[0xb1, 0x89]); // LDA ($89),Y
        assert_eq!(result.repr, "LDA ($89),Y");
        assert_eq!(
            result.addr_value_hint,
            Some("= 0300 @ 0300 = 89".to_string())
        );

        // Test with Y offset
        tester.cpu.state.reg_y.set(0x34);
        tester.cpu.bus.write_u16(0x97, 0xffff);
        tester.cpu.bus.write(0x0033, 0xa3);
        let result = debug_disassemble(&tester.cpu, &tester.mem, &[0xb1, 0x97]); // LDA ($97),Y with Y=0x34
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
    //     tester.cpu.state.reg_a.set(1);
    //     tester.cpu.state.reg_x.set(2);
    //     tester.cpu.state.reg_y.set(3);
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
