use crate::table::{DecodeTable, InstructionKind, DECODE_TABLE};
use instruction_proc_macro::AsRef;

use num_enum::TryFromPrimitive;
use std::io::Read;
use std::path::Path;

pub struct Program {
  mem: Vec<u8>,
  reg: Registers,
}

impl Program {
  pub fn new(mem_size_log2: usize) -> Self {
    Self { mem: vec![0; 1 << mem_size_log2], reg: Registers::new() }
  }

  pub fn load_memory_from_file<P: AsRef<Path>>(&mut self, path: P) -> (usize, usize) {
    let mut sa = SegmentedAccess::full(&mut self.mem);
    let bytes_read = sa.load_memory_from_file(path, 0); // TODO maybe propagate error
    (sa.get_absoulute_address_of(0), bytes_read)
  }

  pub fn simulate_from_file<P: AsRef<Path>>(&mut self, path: P) -> String {
    let (_addr, bytes_read) = self.load_memory_from_file(path); // Should allow reading at a sa
    self.reg.set(&IP, 0);
    self.simulate(bytes_read as u16)
  }

  pub fn simulate(&mut self, num_bytes: u16) -> String {
    let Program { mem, reg } = self;
    let mut lines = vec![];

    let main_mem_slice = unsafe { std::slice::from_raw_parts_mut(mem.as_mut_ptr(), mem.len()) };
    let mut main_memory = SegmentedAccess::full(main_mem_slice);

    while reg.get(IP) < num_bytes {
      let at = SegmentedAccess::new_16(mem, reg.get(CS), reg.get(IP));

      let instruction = decode(&at);
      reg.set(IP, reg.get(IP) + instruction.size);

      let prev_reg = reg.clone();
      instruction
        .exec(&mut main_memory, reg)
        .unwrap_or_else(|e| panic!("Could not execute: {e}\nSimulated so far:\n{}", lines.join("\n")));

      let asm = instruction.print_asm();
      let dif = reg.difference(&prev_reg);
      lines.push(format!("{asm:<20} ; [{:04x}] {dif}", instruction.address));
    }

    let reg_str = reg.to_string().lines().map(|x| "; ".to_string() + x).collect::<Vec<_>>().join("\n");
    lines.push("".into());
    lines.push(reg_str);

    lines.join("\n")
  }
}

#[repr(u16)]
pub enum InstructionFlag {
  Wide = 0x8,
}

#[derive(Debug)]
pub struct Instruction {
  pub address: usize,
  pub kind: InstructionKind,
  pub size: u16,
  pub flags: u16,
  pub operands: [Operand; 2],
  pub exec: fn(&Instruction, &mut SegmentedAccess, &mut Registers) -> Result<(), String>,
}

impl Instruction {
  pub fn print_asm(&self) -> String {
    format!(
      "{} {}{}{}",
      self.kind.str(),
      self.operands[0].str(),
      if self.operands[0].is_none() || self.operands[1].is_none() { "" } else { ", " },
      self.operands[1].str(),
    )
  }

  pub fn exec(&self, sa: &mut SegmentedAccess, registers: &mut Registers) -> Result<(), String> {
    (self.exec)(self, sa, registers)
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
  pub fn str(&self) -> String {
    match self {
      Self::None => "".to_string(),
      Self::Register(register) => register.to_string(),
      Self::Memory(ea_expr) => ea_expr.to_string(),
      Self::Immediate(im) => im.to_string(),
    }
  }

  pub fn is_none(&self) -> bool {
    match self {
      Self::None => true,
      _ => false,
    }
  }

  pub fn set_val(&self, registers: &mut Registers, val: u16) {
    match self {
      Self::None => panic!("Cannot set value of none"),
      Self::Register(reg) => registers.set(reg, val),
      Self::Memory(ea_expr) => todo!(),
      Self::Immediate(im) => panic!("Cannot set value of immediate"),
    }
  }

  pub fn get_val(&self, registers: &Registers) -> u16 {
    match self {
      Self::None => panic!("Cannot get value of none"),
      Self::Register(reg) => registers.get(reg),
      Self::Memory(ea_expr) => todo!(),
      Self::Immediate(im) => match im {
        Immediate::ByteValue(b) => *b as u8 as u16, // CHECK is it sound to not sign extend here?
        Immediate::WordValue(w) => *w as u16,
        Immediate::JumpDisplacement(_) => panic!("Cannot get val value of jump displacement"),
      },
    }
  }
}

#[derive(Clone, Copy, Debug, AsRef)]
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
  pub fn to_string(&self) -> String {
    match self {
      Self::Direct(addr) => format!("[{addr}]"),
      Self::Expression { terms, displacement } => {
        format!(
          "[{}{}{} + {displacement}]",
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
  pub segment_offset: u16,
}

pub fn decode<'a, SA: AsRef<SegmentedAccess<'a>>>(at: SA) -> Instruction {
  let at = at.as_ref();
  let b = at.get_memory(0);
  let code = u16::from_be_bytes([b[0], b[1]]) as usize;
  let instruction = DECODE_TABLE[code].expect(&format!("Unexpected instruction code {code:016b}"))(&at);
  instruction
}

impl<'a> SegmentedAccess<'a> {
  pub fn new_16(memory: &'a mut [u8], segment_base: u16, segment_offset: u16) -> Self {
    Self { memory, segment_offset, segment_base, mask: 0xFFFF }
  }

  pub fn decode_and_consume(&mut self, decode: &DecodeTable) -> Instruction {
    let b = self.get_memory(0);
    let code = u16::from_be_bytes([b[0], b[1]]) as usize;
    let instruction = decode[code].expect(&format!("Unexpected instruction code {code:016b}"))(&self);
    self.segment_offset += instruction.size;
    instruction
  }

  pub fn get_absoulute_address_of(&self, offset: u16) -> usize {
    let SegmentedAccess { mask, segment_base, segment_offset, .. } = &self;
    (((*segment_base as usize) << 4) + (*segment_offset as usize + offset as usize)) & (*mask as usize)
  }

  pub fn get_mut_memory(&mut self, offset: u16) -> &mut [u8] {
    let absolute_address = self.get_absoulute_address_of(offset);
    &mut self.memory[absolute_address..]
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

  pub fn new_memory_pow2(size_pow2: u32, memory: &'a mut [u8]) -> Self {
    Self { memory, mask: (1 << size_pow2) - 1, segment_offset: 0, segment_base: 0 }
  }

  pub fn load_memory_from_file<P: AsRef<Path>>(&mut self, path: P, offset: u16) -> usize {
    std::fs::File::open(&path)
      .expect(&format!("Could not open file {}", path.as_ref().to_string_lossy()))
      .read(self.get_mut_memory(offset))
      .expect("Could not read bytes into memory")
  }

  pub fn move_base_by(&mut self, offset: u16) {
    self.segment_offset += offset;
    self.segment_base += self.segment_offset >> 4;
    self.segment_offset &= 0xf;
  }
}

#[derive(Debug, Clone, Copy)]
pub struct Registers {
  store: [u16; 14],
}

impl Registers {
  pub fn new() -> Self {
    Self { store: [0; 14] }
  }

  pub fn set_flag(&mut self, val: bool, mask: u16) {
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
        s = format!("{s} ({name:x<2}):{}->{}", flags_to_string(prev.store[reg as usize]), flags_to_string(self.store[reg as usize]));
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
#[derive(Debug, Clone, Copy, TryFromPrimitive)]
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
#[derive(Debug, Clone, Copy, TryFromPrimitive)]
pub enum Flag {
  PF = 1 << 2,
  ZF = 1 << 6,
  SF = 1 << 7,
}

fn flags_to_string(flag: u16) -> String {
  let mut flags = vec!["[".into()];
  for i in 0..16 {
    let Ok(f) = Flag::try_from(1 << i) else {
      continue;
    };
    if (1 << i) & flag != 0 {
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

#[rustfmt::skip]
pub const RW_REGISTER: [[&'static str; 2]; 8] = [
  ["al", "ax"],
  ["cl", "cx"],
  ["dl", "dx"],
  ["bl", "bx"],
  ["ah", "sp"],
  ["ch", "bp"],
  ["dh", "si"],
  ["bh", "di"],
];

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
