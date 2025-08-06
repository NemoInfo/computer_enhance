use crate::types::*;

pub fn exec_mov(instruction: &Instruction, _sa: &mut SegmentedAccess, registers: &mut Registers) -> Result<(), String> {
  let [dst, src] = &instruction.operands;
  let val = src.get_val(&registers);
  dst.set_val(registers, val);
  Ok(())
}

pub fn exec_add(instruction: &Instruction, _sa: &mut SegmentedAccess, registers: &mut Registers) -> Result<(), String> {
  let [dst, src] = &instruction.operands;
  let [dval, sval] = [src.get_val(&registers), dst.get_val(&registers)];
  let w: usize = (instruction.flags & InstructionFlag::Wide as u16 != 0) as usize;
  // let width: u16 = [1, 2][w];
  let sign_bit: u16 = [0x80, 0x8000][w];
  let width_mask: i32 = [0x00FF, 0xFFFF][w];
  let res = (sval as i32 & width_mask) + (dval as i32 & width_mask);
  let res_masked = (res & width_mask) as u16;
  // TODO OF
  // TODO AF
  registers.set_flag((res_masked & 0x00FF).count_ones() & 1 == 0, Flag::PF as u16);
  registers.set_flag(res_masked & sign_bit != 0, Flag::SF as u16);
  registers.set_flag(res_masked == 0, Flag::ZF as u16);

  dst.set_val(registers, res_masked);
  Ok(())
}

pub fn exec_sub(instruction: &Instruction, _sa: &mut SegmentedAccess, registers: &mut Registers) -> Result<(), String> {
  let [dst, src] = &instruction.operands;
  let [dval, sval] = [src.get_val(&registers), dst.get_val(&registers)];
  let w: usize = (instruction.flags & InstructionFlag::Wide as u16 != 0) as usize;
  // let width: u16 = [1, 2][w];
  let sign_bit: u16 = [0x80, 0x8000][w];
  let width_mask: i32 = [0x00FF, 0xFFFF][w];
  let res = (sval as i32 & width_mask) - (dval as i32 & width_mask);
  let res_masked = (res & width_mask) as u16;
  // TODO OF
  // TODO AF
  registers.set_flag((res_masked & 0x00FF).count_ones() & 1 == 0, Flag::PF as u16);
  registers.set_flag(res_masked & sign_bit != 0, Flag::SF as u16);
  registers.set_flag(res_masked == 0, Flag::ZF as u16);

  dst.set_val(registers, res_masked);
  Ok(())
}

pub fn exec_cmp(instruction: &Instruction, _sa: &mut SegmentedAccess, registers: &mut Registers) -> Result<(), String> {
  let [dst, src] = &instruction.operands;
  let [dval, sval] = [src.get_val(&registers), dst.get_val(&registers)];
  let w: usize = (instruction.flags & InstructionFlag::Wide as u16 != 0) as usize;
  // let width: u16 = [1, 2][w];
  let sign_bit: u16 = [0x80, 0x8000][w];
  let width_mask: i32 = [0x00FF, 0xFFFF][w];
  let res = (sval as i32 & width_mask) - (dval as i32 & width_mask);
  let res_masked = (res & width_mask) as u16;
  // TODO OF
  // TODO AF
  registers.set_flag((res_masked & 0x00FF).count_ones() & 1 == 0, Flag::PF as u16);
  registers.set_flag(res_masked & sign_bit != 0, Flag::SF as u16);
  registers.set_flag(res_masked == 0, Flag::ZF as u16);

  Ok(())
}

pub fn exec_todo(instruction: &Instruction, _sa: &mut SegmentedAccess, _reg: &mut Registers) -> Result<(), String> {
  Err(format!("{:?} not implemented", instruction.kind))
}
