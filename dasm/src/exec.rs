use crate::types::reg::*;
use crate::types::Flag::*;
use crate::types::*;

pub type ResultExecuteResult = Result<ExecuteResult, String>;

#[allow(unused)]
pub struct ExecuteResult {
  pub branch_taken: bool,
  pub address_unaligned: bool,
}

impl ExecuteResult {
  fn new() -> Self {
    Self { branch_taken: false, address_unaligned: false }
  }

  fn with_branch_taken(mut self) -> Self {
    self.branch_taken = true;
    self
  }

  fn with_alignment(mut self, address_unaligned: bool) -> Self {
    self.address_unaligned = address_unaligned;
    self
  }
}

pub fn exec_mov(inst: &Instruction, sa: &mut SegmentedAccess, regs: &mut Registers) -> ResultExecuteResult {
  let [dst, src] = &inst.operands;
  let w = inst.flags & InstructionFlag::Wide as u16 != 0;
  let val = src.get_val(&regs, sa, inst.segment_register);
  dst.set_val(regs, sa, inst.segment_register, w, val);
  Ok(ExecuteResult::new().with_alignment(inst.has_unaligned_address_operand(&regs)))
}

pub fn exec_add(inst: &Instruction, sa: &mut SegmentedAccess, regs: &mut Registers) -> ResultExecuteResult {
  let [dst, src] = &inst.operands;
  let [dval, sval] = [dst.get_val(&regs, sa, inst.segment_register), src.get_val(&regs, sa, inst.segment_register)];

  let w: usize = (inst.flags & InstructionFlag::Wide as u16 != 0) as usize;
  let sign_bit: u16 = [0x80, 0x8000][w];
  let width_mask: i32 = [0x00FF, 0xFFFF][w];
  let res = (sval as i32 & width_mask) + (dval as i32 & width_mask);
  let res_masked = (res & width_mask) as u16;

  regs.set_flag(AF, (dval ^ sval ^ res_masked) & 0x10 != 0);
  regs.set_flag(OF, sign_bit & !(dval ^ sval) & (dval ^ res_masked) != 0);
  update_arithmetic_flags(regs, res, res_masked, sign_bit);

  dst.set_val(regs, sa, inst.segment_register, w == 1, res_masked);
  Ok(ExecuteResult::new().with_alignment(inst.has_unaligned_address_operand(&regs)))
}

pub fn exec_sub(inst: &Instruction, sa: &mut SegmentedAccess, regs: &mut Registers) -> ResultExecuteResult {
  let [dst, src] = &inst.operands;
  let [dval, sval] = [dst.get_val(&regs, sa, inst.segment_register), src.get_val(&regs, sa, inst.segment_register)];

  let w: usize = (inst.flags & InstructionFlag::Wide as u16 != 0) as usize;
  let sign_bit: u16 = [0x80, 0x8000][w];
  let width_mask: i32 = [0x00FF, 0xFFFF][w];
  let res = (dval as i32 & width_mask) - (sval as i32 & width_mask);
  let res_masked = (res & width_mask) as u16;

  regs.set_flag(AF, (dval ^ sval ^ res_masked) & 0x10 != 0);
  regs.set_flag(OF, sign_bit & (dval ^ sval) & (dval ^ res_masked) != 0);
  update_arithmetic_flags(regs, res, res_masked, sign_bit);

  dst.set_val(regs, sa, inst.segment_register, w == 1, res_masked);
  Ok(ExecuteResult::new().with_alignment(inst.has_unaligned_address_operand(&regs)))
}

pub fn exec_cmp(inst: &Instruction, sa: &mut SegmentedAccess, regs: &mut Registers) -> ResultExecuteResult {
  let [dst, src] = &inst.operands;
  let [dval, sval] = [dst.get_val(&regs, sa, inst.segment_register), src.get_val(&regs, sa, inst.segment_register)];

  let w: usize = (inst.flags & InstructionFlag::Wide as u16 != 0) as usize;
  let sign_bit: u16 = [0x80, 0x8000][w];
  let width_mask: i32 = [0x00FF, 0xFFFF][w];
  let res = (dval as i32 & width_mask) - (sval as i32 & width_mask);
  let res_masked = (res & width_mask) as u16;

  regs.set_flag(AF, (dval ^ sval ^ res_masked) & 0x10 != 0);
  regs.set_flag(OF, sign_bit & (dval ^ sval) & (dval ^ res_masked) != 0);
  update_arithmetic_flags(regs, res, res_masked, sign_bit);
  Ok(ExecuteResult::new().with_alignment(inst.has_unaligned_address_operand(&regs)))
}

fn exec_conditional_jump<F>(inst: &Instruction, regs: &mut Registers, condition: F) -> ResultExecuteResult
where
  F: Fn(&Registers) -> bool,
{
  let [_, src] = &inst.operands;
  let disp = src.get_immediate().expect("Jump src can only be immediate") as i16;
  if condition(regs) {
    regs[IP] = regs[IP].wrapping_add_signed(disp);
    Ok(ExecuteResult::new().with_branch_taken())
  } else {
    Ok(ExecuteResult::new())
  }
}

macro_rules! conditional_jump {
  ($name:ident, $closure:expr) => {
    pub fn $name(inst: &Instruction, _sa: &mut SegmentedAccess, regs: &mut Registers) -> ResultExecuteResult {
      exec_conditional_jump(inst, regs, $closure)
    }
  };
}

conditional_jump!(exec_jo, |reg| reg.get_flag(OF));
conditional_jump!(exec_jb, |reg| reg.get_flag(CF));
conditional_jump!(exec_jz, |reg| reg.get_flag(ZF));
conditional_jump!(exec_ja, |reg| !(reg.get_flag(CF) || reg.get_flag(ZF)));
conditional_jump!(exec_js, |reg| reg.get_flag(SF));
conditional_jump!(exec_jp, |reg| reg.get_flag(PF));
conditional_jump!(exec_jl, |reg| reg.get_flag(SF) ^ reg.get_flag(OF));
conditional_jump!(exec_jg, |reg| !(reg.get_flag(SF) ^ reg.get_flag(OF) | reg.get_flag(ZF)));

conditional_jump!(exec_jno, |reg| !reg.get_flag(OF));
conditional_jump!(exec_jnb, |reg| !reg.get_flag(CF));
conditional_jump!(exec_jnz, |reg| !reg.get_flag(ZF));
conditional_jump!(exec_jna, |reg| reg.get_flag(CF) || reg.get_flag(ZF));
conditional_jump!(exec_jns, |reg| !reg.get_flag(SF));
conditional_jump!(exec_jnp, |reg| !reg.get_flag(PF));
conditional_jump!(exec_jnl, |reg| !(reg.get_flag(SF) ^ reg.get_flag(OF)));
conditional_jump!(exec_jng, |reg| reg.get_flag(SF) ^ reg.get_flag(OF) | reg.get_flag(ZF));

pub fn exec_loopnz(inst: &Instruction, _sa: &mut SegmentedAccess, regs: &mut Registers) -> ResultExecuteResult {
  regs[CX] -= 1;
  exec_conditional_jump(inst, regs, |reg| !reg.get_flag(ZF) && reg[CX] != 0)
}

pub fn exec_loop(inst: &Instruction, _sa: &mut SegmentedAccess, regs: &mut Registers) -> ResultExecuteResult {
  regs[CX] -= 1;
  exec_conditional_jump(inst, regs, |reg| reg[CX] != 0)
}

pub fn exec_todo(inst: &Instruction, _sa: &mut SegmentedAccess, _reg: &mut Registers) -> ResultExecuteResult {
  Err(format!("{:?} not implemented", inst.kind))
}

pub fn exec_continue(_inst: &Instruction, _sa: &mut SegmentedAccess, _reg: &mut Registers) -> ResultExecuteResult {
  Ok(ExecuteResult::new())
}

pub fn update_common_flags(regs: &mut Registers, res_masked: u16, sign_bit: u16) {
  regs.set_flag(PF, (res_masked & 0x00FF).count_ones() & 1 == 0);
  regs.set_flag(SF, res_masked & sign_bit != 0);
  regs.set_flag(ZF, res_masked == 0);
}

pub fn update_arithmetic_flags(regs: &mut Registers, res: i32, res_masked: u16, sign_bit: u16) {
  regs.set_flag(CF, res & ((sign_bit as i32) << 1) != 0);
  update_common_flags(regs, res_masked, sign_bit);
}
