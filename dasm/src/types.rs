use crate::clocks::{Processor, TimingState};
use crate::exec::ResultExecuteResult;
use crate::table::{InstructionKind, DECODE_TABLE};
use instruction_proc_macro::AsRef;
use strum::{EnumIter, IntoEnumIterator};

use std::io::Read;
use std::ops::{Index, IndexMut, Range, RangeFrom, RangeFull, RangeTo};
use std::path::Path;

pub struct Program {
  pub mem: Vec<u8>,
  pub reg: Registers,
}

impl Program {
  pub fn new(mem_size_log2: usize) -> Self {
    Self { mem: vec![0; 1 << mem_size_log2], reg: Registers::new_segmented() }
  }

  pub fn load_executable_from_file<P: AsRef<Path>>(&mut self, path: P, offset: u16) -> (usize, usize) {
    let mut sa = SegmentedAccess::new_with_segment_base(&mut self.mem, self.reg[CS]);
    let bytes_read = sa.load_memory_from_file(path, offset);
    (sa.get_absoulute_address_of(offset), bytes_read)
  }

  pub fn simulate_from_file<P: AsRef<Path>>(&mut self, path: P, exec: bool, time: Option<Processor>) -> String {
    let (_addr, bytes_read) = self.load_executable_from_file(path, self.reg[IP]); // Should allow reading at a sa
    self.simulate(bytes_read as u16, exec, time)
  }

  pub fn simulate(&mut self, num_bytes: u16, exec: bool, time: Option<Processor>) -> String {
    let Program { mem, reg } = self;
    *reg = reg.clear_program_registers();
    let mut context = DecodeContext::new();
    let mut lines = vec!["bits 16".into()];
    let mut main_memory = SegmentedAccess::full(mem);
    let mut at = SegmentedAccess::clone_from(&mut main_memory, reg[CS]);
    let mut timing_state = TimingState::new();
    let start_ip = reg[IP];

    while reg.get(IP) < start_ip + num_bytes {
      let prev_reg = reg.clone();
      at.segment_offset = reg[IP];

      let opcode = u16::from_be_bytes(at[..2].try_into().unwrap()) as usize;
      let mut instruction = DECODE_TABLE[opcode].expect(&format!("Unexpected instruction code {opcode:016b}"))(&at);
      reg[IP] += instruction.size;

      match instruction.kind {
        InstructionKind::SEGMENT => {
          context.flags |= InstructionFlag::SegmentOverride as u16;
          let Operand::Register(segment_register) = instruction.operands[1] else { unreachable!() };
          context.segment_register = segment_register;
        }
        _ => {
          instruction.flags |= context.flags;
          if instruction.flags & InstructionFlag::SegmentOverride as u16 != 0 {
            instruction.segment_register = Some(context.segment_register);
          }
          let asm = instruction.print_asm();
          let mut line = format!("{asm:<30} ; [{:04x}]", instruction.address);

          if exec {
            let result = instruction
              .exec(&mut main_memory, reg)
              .unwrap_or_else(|e| panic!("Could not execute: {e}\nSimulated so far:\n{}", lines.join("\n")));

            timing_state.branch_taken = result.branch_taken;
            match time {
              Some(processor) => {
                timing_state.current_time +=
                  timing_state.estimate_instruction_clocks(&instruction).clocks(result.address_unaligned, processor);

                line += &format!(" cycle: {:<3}", timing_state.current_time.to_string());
                let instruction_timing = timing_state.estimate_instruction_clocks(&instruction);
                line += &format!(
                  "{:<30}",
                  format!(" (+{})", instruction_timing.to_debug_string(result.address_unaligned, processor))
                );
              }
              None => {}
            }
          } else if let Some(processor) = time {
            let instruction_timing = timing_state.estimate_instruction_clocks(&instruction);
            line += &format!(" {}", instruction_timing.to_debug_string_no_address(processor));
          }

          if exec {
            let dif = reg.difference(&prev_reg);
            line += &format!(" {dif}")
          }

          lines.push(line);
          context = DecodeContext::new();
        }
      }
    }

    if exec {
      let reg_str = reg.to_string().lines().map(|x| "; ".to_string() + x).collect::<Vec<_>>().join("\n");
      lines.push("".into());
      lines.push(reg_str);

      if let Some(processor) = time {
        lines.push(format!("; Total cycles: {} ({processor:?})", timing_state.current_time));
      }
    }

    lines.join("\n")
  }

  pub fn dump(&mut self) -> Vec<u8> {
    let Program { mem, reg } = self;
    let ds = SegmentedAccess::new_with_segment_base(mem, reg[DS]);
    ds[..].to_vec()
  }
}

pub struct DecodeContext {
  pub flags: u16,
  pub segment_register: Register,
}

impl DecodeContext {
  pub fn new() -> Self {
    Self { flags: 0, segment_register: DS }
  }
}

#[repr(u16)]
pub enum InstructionFlag {
  Wide = 0x1,
  SegmentOverride = 0x2,
}

#[derive(Debug)]
pub struct Instruction {
  pub address: usize,
  pub kind: InstructionKind,
  pub size: u16,
  pub flags: u16,
  pub operands: [Operand; 2],
  pub exec: fn(&Instruction, &mut SegmentedAccess, &mut Registers) -> ResultExecuteResult,
  pub segment_register: Option<Register>,
}

impl Instruction {
  pub fn print_asm(&self) -> String {
    format!(
      "{} {}{}{}",
      self.kind.to_string(),
      self.operands[0].to_string(self.flags, self.segment_register),
      if self.operands[0].is_none() || self.operands[1].is_none() { "" } else { ", " },
      self.operands[1].to_string(self.flags, self.segment_register),
    )
  }

  pub fn exec(&self, sa: &mut SegmentedAccess, registers: &mut Registers) -> ResultExecuteResult {
    (self.exec)(self, sa, registers)
  }

  pub fn has_unaligned_address_operand(&self, registers: &Registers) -> bool {
    match self.operands {
      [Operand::Memory(ea), _] | [_, Operand::Memory(ea)] => ea.get_addr(registers) & 1 == 1,
      _ => false,
    }
  }
}

pub type RegisterIndex = RegisterName;

#[derive(Debug)]
pub enum Operand {
  None,
  Register(Register),
  Memory(EffectiveAddressExpression),
  Immediate(Immediate),
}

impl Operand {
  pub fn to_string(&self, flags: u16, segment_register: Option<Register>) -> String {
    match self {
      Self::None => "".to_string(),
      Self::Register(register) => register.to_string(),
      Self::Memory(ea_expr) => ea_expr.to_string(flags, segment_register),
      Self::Immediate(im) => im.to_string(),
    }
  }

  pub fn is_none(&self) -> bool {
    match self {
      Self::None => true,
      _ => false,
    }
  }

  pub fn set_val(
    &self,
    registers: &mut Registers,
    memory: &mut SegmentedAccess,
    segment_register: Option<Register>,
    w: bool,
    val: u16,
  ) {
    match self {
      Self::None => panic!("Cannot set value of none"),
      Self::Register(reg) => registers.set(reg, val),
      Self::Memory(ea_expr) => ea_expr.set(registers, memory, segment_register, w, val),
      Self::Immediate(_) => panic!("Cannot set value of immediate"),
    }
  }

  pub fn get_val(
    &self,
    registers: &Registers,
    memory: &mut SegmentedAccess,
    segment_register: Option<Register>,
  ) -> u16 {
    match self {
      Self::None => panic!("Cannot get value of none"),
      Self::Register(reg) => registers.get(reg),
      Self::Memory(ea_expr) => ea_expr.get(registers, memory, segment_register),
      Self::Immediate(_) => self.get_immediate().unwrap(),
    }
  }

  pub fn get_immediate(&self) -> Option<u16> {
    if let Self::Immediate(im) = self {
      Some(match im {
        Immediate::ByteValue(b) => *b as u8 as u16, // CHECK is it sound to not sign extend here?
        Immediate::WordValue(w) => *w as u16,
        Immediate::JumpDisplacement(d) => *d as u16,
      })
    } else {
      None
    }
  }
}

#[derive(Clone, Copy, Debug, AsRef, PartialEq)]
pub struct Register {
  pub index: RegisterIndex,
  pub offset: u32,
  pub wide: bool,
}

impl Register {
  pub fn to_string(&self) -> String {
    if (self.index as usize) < 4 {
      self.index.to_string() + ["l", "h", "x"][self.wide as usize * 2 + self.offset as usize]
    } else {
      self.index.to_string()
    }
  }
}

#[derive(Clone, Copy, Debug)]
pub struct EffectiveAddressTerm {
  pub register: Register,
}

#[derive(Clone, Copy, Debug)]
pub enum EffectiveAddressExpression {
  Expression { terms: [Option<EffectiveAddressTerm>; 2], displacement: i16 },
  Direct(u16),
}

impl EffectiveAddressExpression {
  pub fn set(
    &self,
    registers: &Registers,
    memory: &mut SegmentedAccess,
    segment_register: Option<Register>,
    w: bool,
    val: u16,
  ) {
    let sr = segment_register.expect("All effective address calculations have a segment register");
    let sr = registers.get(sr);
    let mut segment = memory.clone_from(sr);
    let addr = self.get_addr(registers);
    let [lo, hi] = val.to_le_bytes();

    segment[addr] = lo;
    if w {
      segment[addr + 1] = hi;
    }
  }

  pub fn get(&self, registers: &Registers, memory: &mut SegmentedAccess, segment_register: Option<Register>) -> u16 {
    let sr = segment_register.expect("All effective address calculations have a segment register");
    let sr = registers.get(sr);
    let segment = memory.clone_from(sr);
    let addr = self.get_addr(registers);

    u16::from_le_bytes([segment[addr], segment[addr + 1]])
  }

  pub fn get_addr(&self, registers: &Registers) -> u16 {
    match *self {
      Self::Expression { terms, displacement } => {
        let base: u16 = terms.map(|term| term.map(|term| registers.get(term.register)).unwrap_or(0)).iter().sum();
        base.wrapping_add_signed(displacement)
      }
      Self::Direct(addr) => addr,
    }
  }

  pub fn to_string(&self, flags: u16, segment_register: Option<Register>) -> String {
    let sr = if flags & InstructionFlag::SegmentOverride as u16 != 0 {
      segment_register.unwrap().to_string() + ":"
    } else {
      "".into()
    };

    match self {
      Self::Direct(addr) => format!("{sr}[{addr}]"),
      Self::Expression { terms, displacement } => {
        format!(
          "{sr}[{}{}{} + {displacement}]",
          terms[0].map(|x| x.register.to_string()).unwrap_or("".into()),
          if terms[0].is_some() && terms[1].is_some() { " + " } else { "" },
          terms[1].map(|x| x.register.to_string()).unwrap_or("".into()),
        )
      }
    }
  }
}

#[derive(Debug)]
pub enum Immediate {
  ByteValue(i8),
  WordValue(i16),
  JumpDisplacement(i16),
}

impl Immediate {
  pub fn to_string(&self) -> String {
    match self {
      Self::ByteValue(i) => format!("byte {i}"),
      Self::WordValue(i) => format!("word {i}"),
      Self::JumpDisplacement(i) => format!("${:+}", i + 2),
    }
  }
}

#[derive(Debug, AsRef)]
pub struct SegmentedAccess<'a> {
  pub memory: &'a mut [u8],
  pub mask: u32,
  pub segment_base: u16,
  pub segment_offset: u16, // I'm not sure why this exists
}

impl<'a> Index<Range<u16>> for SegmentedAccess<'a> {
  type Output = [u8];
  fn index(&self, index: Range<u16>) -> &Self::Output {
    &self.memory[self.get_absoulute_address_of(index.start)..self.get_absoulute_address_of(index.end)]
  }
}

impl<'a> Index<RangeTo<u16>> for SegmentedAccess<'a> {
  type Output = [u8];
  fn index(&self, index: RangeTo<u16>) -> &Self::Output {
    &self.memory[self.get_absoulute_address_of(0)..self.get_absoulute_address_of(index.end)]
  }
}

impl<'a> Index<RangeFull> for SegmentedAccess<'a> {
  type Output = [u8];
  fn index(&self, _: RangeFull) -> &Self::Output {
    &self.memory[self.get_absoulute_address_of(0)..self.get_absoulute_address_of(u16::MAX)]
  }
}

impl<'a> Index<u16> for SegmentedAccess<'a> {
  type Output = u8;
  fn index(&self, offset: u16) -> &Self::Output {
    &self.memory[self.get_absoulute_address_of(offset)]
  }
}

impl<'a> IndexMut<u16> for SegmentedAccess<'a> {
  fn index_mut(&mut self, offset: u16) -> &mut Self::Output {
    &mut self.memory[self.get_absoulute_address_of(offset)]
  }
}

impl<'a> IndexMut<RangeFull> for SegmentedAccess<'a> {
  fn index_mut(&mut self, _: RangeFull) -> &mut Self::Output {
    let start = self.get_absoulute_address_of(0);
    let end = self.get_absoulute_address_of(u16::MAX);
    &mut self.memory[start..end]
  }
}

impl<'a> Index<RangeFrom<u16>> for SegmentedAccess<'a> {
  type Output = [u8];
  fn index(&self, index: RangeFrom<u16>) -> &Self::Output {
    &self.memory[self.get_absoulute_address_of(index.start)..self.get_absoulute_address_of(u16::MAX)]
  }
}

impl<'a> IndexMut<RangeFrom<u16>> for SegmentedAccess<'a> {
  fn index_mut(&mut self, index: RangeFrom<u16>) -> &mut Self::Output {
    let start = self.get_absoulute_address_of(index.start);
    let end = self.get_absoulute_address_of(u16::MAX);
    &mut self.memory[start..end]
  }
}

impl<'a> SegmentedAccess<'a> {
  pub fn clone_from(&mut self, segment_base: u16) -> Self {
    let Self { segment_offset, mask, .. } = *self;
    let memory = unsafe { std::slice::from_raw_parts_mut(self.memory.as_mut_ptr(), self.memory.len()) };
    Self { memory, segment_base, segment_offset, mask }
  }

  pub fn get_absoulute_address_of(&self, offset: u16) -> usize {
    let SegmentedAccess { mask, segment_base, segment_offset, .. } = *self;
    (((segment_base as usize) << 4) + (segment_offset as usize + offset as usize)) & (mask as usize)
  }

  pub fn get_memory(&self, offset: u16) -> &[u8] {
    let absolute_address = self.get_absoulute_address_of(offset);
    &self.memory[absolute_address..]
  }

  pub fn full(memory: &'a mut [u8]) -> Self {
    let size_log2 = memory.len().ilog2();
    assert_eq!(1 << size_log2, memory.len(), "Memory must be power of 2");
    Self { memory, mask: (1 << size_log2) - 1, segment_offset: 0, segment_base: 0 }
  }

  pub fn new_with_segment_base(memory: &'a mut [u8], segment_base: u16) -> Self {
    let size_log2 = memory.len().ilog2();
    assert_eq!(1 << size_log2, memory.len(), "Memory must be power of 2");
    Self { memory, mask: (1 << size_log2) - 1, segment_offset: 0, segment_base }
  }

  pub fn load_memory_from_file<P: AsRef<Path>>(&mut self, path: P, offset: u16) -> usize {
    std::fs::File::open(&path)
      .expect(&format!("Could not open file {}", path.as_ref().to_string_lossy()))
      .read(&mut self[offset..])
      .expect("Could not read bytes into memory")
  }

  pub fn _move_base_by(&mut self, offset: u16) {
    self.segment_offset += offset;
    self.segment_base += self.segment_offset >> 4;
    self.segment_offset &= 0xf;
  }
}

#[derive(Debug, Clone, Copy)]
pub struct Registers {
  store: [u16; 14],
}

impl Index<Register> for Registers {
  type Output = u16;
  fn index(&self, reg: Register) -> &Self::Output {
    assert!(reg.wide || reg.offset != 0, "Can't index byte registers, use get instead");
    &self.store[reg.index as usize]
  }
}

impl IndexMut<Register> for Registers {
  fn index_mut(&mut self, reg: Register) -> &mut Self::Output {
    assert!(reg.wide || reg.offset != 0, "Can't index byte registers, use get instead");
    &mut self.store[reg.index as usize]
  }
}

impl Registers {
  pub fn _new() -> Self {
    Self { store: [0; 14] }
  }

  pub fn new_segmented() -> Self {
    let mut registers = Self { store: [0; 14] };
    registers[CS] = 0xF000;
    registers[DS] = 0x1000;
    registers[SS] = 0x2000;
    registers[ES] = 0x3000;
    registers[IP] = 0x0;
    registers
  }

  pub fn clear_program_registers(self) -> Self {
    let mut new = Self { store: [0; 14] };
    new[CS] = self[CS];
    new[DS] = self[DS];
    new[SS] = self[SS];
    new[ES] = self[ES];
    new[IP] = self[IP];
    new
  }

  pub fn get_flag(&self, flag: Flag) -> bool {
    self.get(FLAG) & flag as u16 != 0
  }

  pub fn set_flag(&mut self, flag: Flag, val: bool) {
    let mask = flag as u16;
    self.set(FLAG, (self.get(FLAG) & !mask) | [0, mask][val as usize]);
  }

  fn difference(&self, prev: &Self) -> String {
    let mut s = String::new();
    for i in 0..(self.store.len() as u8) {
      let reg: RegisterName = unsafe { std::mem::transmute(i) };
      let name: String = format!("{reg:?}").to_lowercase();
      if self.store[reg as usize] == prev.store[reg as usize] {
        continue;
      }
      if let RegisterName::FLAG = reg {
        s = format!(
          "{s} ({name:x<2}):{}->{}",
          flags_to_string(prev.store[reg as usize]),
          flags_to_string(self.store[reg as usize])
        );
      } else {
        s = format!("{s} ({name:x<2}):{:04x}->{:04x}", prev.store[reg as usize], self.store[reg as usize]);
      }
    }
    s
  }

  pub fn get<R: AsRef<Register>>(&self, reg: R) -> u16 {
    let reg = reg.as_ref();
    let mask = 0x00FF | ((reg.wide as u16) * 0xFF00);
    (self.store[reg.index as usize] >> (reg.offset * 8)) & mask
  }

  pub fn set<R: AsRef<Register>>(&mut self, reg: R, mut val: u16) {
    let reg = reg.as_ref();
    let wide = reg.wide;
    let offset = reg.offset == 1;
    let mask = (wide as u16 * 0xFFFF) | ((!wide & !offset) as u16 * 0x00FF) | ((!wide & offset) as u16 * 0xFF00);
    val = val << ((!wide & offset) as u16 * 8);
    self.store[reg.index as usize] = (val & mask) | (self.store[reg.index as usize] & !mask);
  }

  pub fn to_string(&self) -> String {
    let mut lines = vec![];
    lines.push("+---------------------+".into());
    lines.push("|    Register Store   |".into());
    lines.push("+----+-------+--------+".into());
    lines.push("|    |  hex  |   i16  |".into());
    lines.push("+----+-------+--------+".into());
    for i in 0..(self.store.len() as u8) {
      let reg_name: RegisterName = unsafe { std::mem::transmute(i) };
      let name: String = format!("{:X<2}", format!("{reg_name:?}"));
      if self.store[reg_name as usize] == 0 {
        continue;
      }
      if let RegisterName::FLAG = reg_name {
        lines.push(format!(
          "|{name:^4}| x{:04x} |{:^8}|",
          self.store[reg_name as usize],
          flags_to_string(self.store[reg_name as usize])
        ));
      } else {
        lines.push(format!("|{name:^4}| x{n:04x} | {n:<6} |", n = self.store[reg_name as usize] as i16));
      }
      lines.push("+----+-------+--------+".into());
    }

    lines.join("\n")
  }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum RegisterName {
  A,
  B,
  C,
  D,
  SP,
  BP,
  SI,
  DI,
  ES,
  CS,
  SS,
  DS,
  IP,
  FLAG,
}

#[repr(u16)]
#[derive(Debug, Clone, Copy, EnumIter)]
pub enum Flag {
  CF = 1 << 0,
  PF = 1 << 2,
  AF = 1 << 4,
  ZF = 1 << 6,
  SF = 1 << 7,
  TF = 1 << 8,
  IF = 1 << 9,
  DF = 1 << 10,
  OF = 1 << 11,
}

fn flags_to_string(flag: u16) -> String {
  let mut flags = vec!["[".into()];
  for f in Flag::iter() {
    if f as u16 & flag != 0 {
      let mut flag_s = format!("{f:?}");
      flag_s.pop();
      flags.push(flag_s);
    }
  }
  flags.push("]".into());
  flags.join("")
}

impl RegisterName {
  fn to_string(&self) -> String {
    format!("{:?}", self).to_lowercase()
  }
}

pub const R_M_TO_EA_TERMS: [[Option<EffectiveAddressTerm>; 2]; 8] = {
  const fn term(register: Register) -> Option<EffectiveAddressTerm> {
    Some(EffectiveAddressTerm { register })
  }
  [
    [term(BX), term(SI)],
    [term(BX), term(DI)],
    [term(BP), term(SI)],
    [term(BP), term(DI)],
    [term(SI), None],
    [term(DI), None],
    [term(BP), None],
    [term(BX), None],
  ]
};

pub const R_M_TO_DEFAULT_SEGMENT_REGISTER: [Register; 8] = [DS, DS, SS, SS, DS, DS, SS, DS];

#[rustfmt::skip]
pub const REG_TO_REGISTER: [[Register; 2]; 8] = [[AL, AX], 
                                                 [CL, CX], 
                                                 [DL, DX], 
                                                 [BL, BX], 
                                                 [AH, SP], 
                                                 [CH, BP], 
                                                 [DH, SI], 
                                                 [BH, DI]];

pub const SR_TO_REGISTER: [Register; 4] = [ES, CS, SS, DS];

use reg::*;
pub mod reg {
  use super::{Register, RegisterName as R};

  const fn reg(index: R, offset: u32, wide: bool) -> Register {
    Register { index, offset, wide }
  }
  const T: bool = true;
  const F: bool = false;

  pub const AX: Register = reg(R::A, 0, T);
  pub const BX: Register = reg(R::B, 0, T);
  pub const CX: Register = reg(R::C, 0, T);
  pub const DX: Register = reg(R::D, 0, T);
  pub const AL: Register = reg(R::A, 0, F);
  pub const BL: Register = reg(R::B, 0, F);
  pub const CL: Register = reg(R::C, 0, F);
  pub const DL: Register = reg(R::D, 0, F);
  pub const AH: Register = reg(R::A, 1, F);
  pub const BH: Register = reg(R::B, 1, F);
  pub const CH: Register = reg(R::C, 1, F);
  pub const DH: Register = reg(R::D, 1, F);
  pub const SP: Register = reg(R::SP, 0, T);
  pub const BP: Register = reg(R::BP, 0, T);
  pub const SI: Register = reg(R::SI, 0, T);
  pub const DI: Register = reg(R::DI, 0, T);
  pub const ES: Register = reg(R::ES, 0, T);
  pub const CS: Register = reg(R::CS, 0, T);
  pub const SS: Register = reg(R::SS, 0, T);
  pub const DS: Register = reg(R::DS, 0, T);
  pub const IP: Register = reg(R::IP, 0, T);
  pub const FLAG: Register = reg(R::FLAG, 0, T);
}
