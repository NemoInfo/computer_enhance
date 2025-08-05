#![allow(non_snake_case)]

use std::{
  ffi::OsString,
  io::Read,
  path::{Path, PathBuf},
};

use instruction_proc_macro::{generate_instruction_decode_table, generate_instruction_enum};

#[rustfmt::skip]
const RW_REGISTER: [[&'static str; 2]; 8] = [
  ["al", "ax"],
  ["cl", "cx"],
  ["dl", "dx"],
  ["bl", "bx"],
  ["ah", "sp"],
  ["ch", "bp"],
  ["dh", "si"],
  ["bh", "di"],
];

struct SegmentedAccess<'a> {
  memory: &'a mut [u8],
  mask: u32,
  segment_base: u16,
  segment_offset: u16,
}

impl<'a> SegmentedAccess<'a> {
  fn get_absoulute_address_of(&self, offset: u16) -> u32 {
    let SegmentedAccess { mask, segment_base, segment_offset, .. } = &self;
    (((*segment_base as u32) << 4) + (*segment_offset as u32 + offset as u32)) & mask
  }

  fn get_mut_memory(&mut self, offset: u16) -> &mut [u8] {
    let absolute_address = self.get_absoulute_address_of(offset) as usize;
    let ptr = self.memory.get_mut(absolute_address).map(|x| x as *mut u8).expect("Invalid memory access");
    unsafe { std::slice::from_raw_parts_mut(ptr, self.memory.len() - absolute_address) }
  }

  fn get_memory(&self, offset: u16) -> &[u8] {
    let absolute_address = self.get_absoulute_address_of(offset) as usize;
    let ptr = self.memory.get(absolute_address).map(|x| x as *const u8).expect("Invalid memory access");
    unsafe { std::slice::from_raw_parts(ptr, self.memory.len() - absolute_address) }
  }

  fn new_memory_pow2(size_pow2: u32, memory: &'a mut [u8]) -> Self {
    Self { memory, mask: (1 << size_pow2) - 1, segment_offset: 0, segment_base: 0 }
  }

  fn load_memory_from_file<P: AsRef<Path>>(&mut self, path: P, offset: u16) -> usize {
    std::fs::File::open(&path)
      .expect(&format!("Could not open file {}", path.as_ref().to_string_lossy()))
      .read(self.get_mut_memory(offset))
      .expect("Could not read bytes into memory")
  }

  fn move_base_by(&mut self, offset: u16) {
    self.segment_offset += offset;
    self.segment_base += self.segment_offset >> 4;
    self.segment_offset &= 0xf;
  }
}

fn allocate_memory_pow2(size_pow2: u32) -> Vec<u8> {
  vec![0; 1usize << size_pow2]
}

const EXEC_TABLE: [fn(&Instruction, &mut SegmentedAccess); 1] = [exec];

fn exec(instruction: &Instruction, sa: &mut SegmentedAccess) {}

fn exec_mov(instruction: &Instruction, sa: &mut SegmentedAccess) {}

macro_rules! instructions {
  ($( $x:tt )*) => {
    generate_instruction_enum!($($x)*);
    generate_instruction_decode_table!($($x)*);
  };
}

instructions! {
  [MOV, *exec_mov, [B(0b100010, 6), D, W, MOD, REG, R_M, DISP_LO, DISP_HI]],
  [MOV, *exec_mov, [B(0b1100011, 7), W, MOD, B(0b000,3), R_M, DISP_LO, DISP_HI, DATA_LO, DATA_HI, I(D,0)]],
  [MOV, *exec_mov, [B(0b1011, 4), W, REG, DATA_LO, DATA_HI, I(D, 1)]],
  [MOV, *exec_mov, [B(0b1010000, 7), W, ADDR_LO, ADDR_HI, I(REG, 0), I(D,1)]],
  [MOV, *exec_mov, [B(0b1010001, 7), W, ADDR_LO, ADDR_HI, I(REG, 0), I(D,0)]],

  [ADD, *exec_mov, [B(0b000000, 6), D, W, MOD, REG, R_M, DISP_LO, DISP_HI]],
  [ADD, *exec_mov, [B(0b100000, 6), S, W, MOD, B(0b000,3), R_M, DISP_LO, DISP_HI, DATA_LO, DATA_HI_IF_SW, I(D,0)]],
  [ADD, *exec_mov, [B(0b0000010, 7), W, DATA_LO, DATA_HI, I(D,1), I(REG, 0)]],

  [SUB, *exec_mov, [B(0b001010, 6), D, W, MOD, REG, R_M, DISP_LO, DISP_HI]],
  [SUB, *exec_mov, [B(0b100000, 6), S, W, MOD, B(0b101,3), R_M, DISP_LO, DISP_HI, DATA_LO, DATA_HI_IF_SW, I(D,0)]],
  [SUB, *exec_mov, [B(0b0010110, 7), W, DATA_LO, DATA_HI, I(D,1), I(REG, 0)]],

  [CMP, *exec_mov, [B(0b001110, 6), D, W, MOD, REG, R_M, DISP_LO, DISP_HI]],
  [CMP, *exec_mov, [B(0b100000, 6), S, W, MOD, B(0b111,3), R_M, DISP_LO, DISP_HI, DATA_LO, DATA_HI_IF_SW, I(D,0)]],
  [CMP, *exec_mov, [B(0b0011110, 7), W, DATA_LO, DATA_HI, I(D,1), I(REG, 0)]],

  [JO , *exec_mov, [B(0x70, 8), IP8, I(D,0)]],
  [JNO, *exec_mov, [B(0x71, 8), IP8, I(D,0)]],
  [JB , *exec_mov, [B(0x72, 8), IP8, I(D,0)]],
  [JNB, *exec_mov, [B(0x73, 8), IP8, I(D,0)]],
  [JZ , *exec_mov, [B(0x74, 8), IP8, I(D,0)]],
  [JNZ, *exec_mov, [B(0x75, 8), IP8, I(D,0)]],
  [JNA, *exec_mov, [B(0x76, 8), IP8, I(D,0)]],
  [JA , *exec_mov, [B(0x77, 8), IP8, I(D,0)]],
  [JS , *exec_mov, [B(0x78, 8), IP8, I(D,0)]],
  [JNS, *exec_mov, [B(0x79, 8), IP8, I(D,0)]],
  [JP , *exec_mov, [B(0x7a, 8), IP8, I(D,0)]],
  [JNP, *exec_mov, [B(0x7b, 8), IP8, I(D,0)]],
  [JL , *exec_mov, [B(0x7c, 8), IP8, I(D,0)]],
  [JNL, *exec_mov, [B(0x7d, 8), IP8, I(D,0)]],
  [JNG, *exec_mov, [B(0x7e, 8), IP8, I(D,0)]],
  [JG , *exec_mov, [B(0x7f, 8), IP8, I(D,0)]],

  [LOOPNZ, *exec_mov, [B(0xe0, 8), IP8, I(D,0)]],
  [LOOPZ , *exec_mov, [B(0xe1, 8), IP8, I(D,0)]],
  [LOOP  , *exec_mov, [B(0xe2, 8), IP8, I(D,0)]],
  [JCXZ  , *exec_mov, [B(0xe3, 8), IP8, I(D,0)]],
}

#[derive(Debug)]
struct Instruction {
  address: u32,
  kind: InstructionKind,
  size: u16,
  flags: u32,
  operands: [Operand; 2],
}

impl Instruction {
  fn print_asm(&self) -> String {
    format!(
      "{:<30} ;{:04x}",
      format!(
        "{} {}{}{}",
        self.kind.str(),
        self.operands[1].str(),
        if self.operands[0].is_none() || self.operands[1].is_none() { "" } else { ", " },
        self.operands[0].str(),
      ),
      self.address,
    )
  }
}

#[repr(u8)]
#[allow(unused)]
enum RegisterName {
  A,
  C,
  D,
  B,
  SP,
  BP,
  SI,
  DI,
  ES,
  CS,
  SS,
  DS,
  IP,
}

const T: bool = true;
const F: bool = false;

const R_M_TO_EA_TERMS: [[Option<EffectiveAddressTerm>; 2]; 8] = {
  use RegisterName::*;
  const fn term(reg: RegisterName, offset: u32, wide: bool) -> Option<EffectiveAddressTerm> {
    let register = Register { index: reg as RegisterIndex, offset, wide };
    Some(EffectiveAddressTerm { register })
  }
  [
    [term(B, 0, T), term(SI, 0, T)],
    [term(B, 0, T), term(DI, 0, T)],
    [term(BP, 0, T), term(SI, 0, T)],
    [term(BP, 0, T), term(DI, 0, T)],
    [term(SI, 0, T), None],
    [term(DI, 0, T), None],
    [term(BP, 0, T), None],
    [term(B, 0, T), None],
  ]
};

const REG_TO_REGISTER: [[Register; 2]; 8] = {
  use RegisterName::*;
  const fn reg(reg: RegisterName, offset: u32, wide: bool) -> Register {
    Register { index: reg as RegisterIndex, offset, wide }
  }
  [
    [reg(A, 0, F), reg(A, 0, T)],
    [reg(C, 0, F), reg(C, 0, T)],
    [reg(D, 0, F), reg(D, 0, T)],
    [reg(B, 0, F), reg(B, 0, T)],
    [reg(A, 1, F), reg(SP, 0, T)],
    [reg(C, 1, F), reg(BP, 0, T)],
    [reg(D, 1, F), reg(SI, 0, T)],
    [reg(B, 1, F), reg(DI, 0, T)],
  ]
};

impl Instruction {
  fn exec(&self, sa: &mut SegmentedAccess) {
    EXEC_TABLE[self.kind as usize](&self, sa);
  }
}

type RegisterIndex = u8;

#[derive(Debug)]
enum Operand {
  None,
  Register(Register),
  Memory(EffectiveAddressExpression),
  Immediate(Immediate),
}

impl Operand {
  fn str(&self) -> String {
    match self {
      Self::None => "".to_string(),
      Self::Register(register) => register.to_string(),
      Self::Memory(ea_expr) => ea_expr.to_string(),
      Self::Immediate(im) => im.to_string(),
    }
  }

  fn is_none(&self) -> bool {
    match self {
      Self::None => true,
      _ => false,
    }
  }
}

#[derive(Clone, Copy, Debug)]
struct Register {
  index: RegisterIndex,
  offset: u32,
  wide: bool,
}

impl Register {
  fn to_string(&self) -> String {
    RW_REGISTER[self.index as usize + self.offset as usize * 4][self.wide as usize].to_string()
  }
}

#[derive(Clone, Copy, Debug)]
struct EffectiveAddressTerm {
  register: Register,
}

#[derive(Clone, Copy, Debug)]
enum EffectiveAddressExpression {
  Expression { terms: [Option<EffectiveAddressTerm>; 2], displacement: i16 },
  Direct(u16),
}

impl EffectiveAddressExpression {
  fn to_string(&self) -> String {
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
enum Immediate {
  ByteValue(i8),
  WordValue(i16),
  JumpDisplacement(i16),
}

impl Immediate {
  fn to_string(&self) -> String {
    match self {
      Self::ByteValue(i) => format!("byte {i}"),
      Self::WordValue(i) => format!("word {i}"),
      Self::JumpDisplacement(i) => format!("${:+}", i + 2),
    }
  }
}

fn dasm<P: AsRef<Path>>(path: P) -> Result<String, ()> {
  let mem_pow2 = 20;
  let mut memory = allocate_memory_pow2(20);
  let mut sa = SegmentedAccess::new_memory_pow2(mem_pow2, &mut memory);
  let mut bytes_to_read = sa.load_memory_from_file(path, 0) as u16;
  let offset = 0;
  let mut lines = vec![];

  while bytes_to_read > 0 {
    let b = sa.get_memory(offset);
    let i = u16::from_be_bytes([b[0], b[1]]);
    let instr = DECODE_TABLE1[i as usize].expect(&format!("Unexpected instruction code {i:016b}"))(&sa);
    bytes_to_read -= instr.size;
    sa.segment_offset += instr.size;
    println!("{}", instr.print_asm());
    lines.push(instr.print_asm());
  }

  Ok(lines.join("\n"))
}

fn read_dasm_write<P: AsRef<Path>>(path: P) -> std::io::Result<PathBuf> {
  use std::io::ErrorKind;

  let path = path.as_ref();
  let asm = dasm(path).expect("disassembly failed");
  println!("{asm}");

  let stem = path.file_stem().ok_or(ErrorKind::InvalidInput)?;
  let mut file_name = OsString::from(stem);
  let asm = format!("; {} disassembly\nbits 16\n{asm}", file_name.to_string_lossy());

  file_name.push("_dasm");

  if let Some(ext) = path.extension() {
    file_name.push(".");
    file_name.push(ext);
  } else {
    file_name.push(".asm");
  }

  let mut out_path = path.parent().ok_or(ErrorKind::InvalidInput)?.to_path_buf();
  out_path.push(file_name);

  std::fs::write(&out_path, asm)?;

  Ok(out_path)
}

fn main() {
  let mut args = std::env::args();
  args.next();
  let args = args.collect::<Vec<_>>();
  let max_arg_len = args.iter().map(|s| s.len()).max();

  for arg in &args {
    match read_dasm_write(&arg) {
      Err(e) => eprintln!("{arg: >width$}: {e}", width = max_arg_len.unwrap()),
      Ok(m) => println!("{arg: >width$}: Disassembled in {}", m.to_string_lossy(), width = max_arg_len.unwrap()),
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[rstest::rstest]
  #[case("../assets/part1/listing_0037_single_register_mov")]
  #[case("../assets/part1/listing_0038_many_register_mov")]
  #[case("../assets/part1/listing_0039_more_movs")]
  #[case("../assets/part1/listing_0040_challenge_movs")]
  #[case("../assets/part1/listing_0041_add_sub_cmp_jnz")]
  #[case("../assets/part1/listing_0042_completionist_decode")]
  fn test(#[case] test_file: &str) {
    println!("test: {test_file}");
    let out_asm = read_dasm_write(&test_file).unwrap();
    let out_bin = format!("{test_file}_bin");
    let res = std::process::Command::new("nasm")
      .args([&out_asm.to_string_lossy().to_string(), "-o", &out_bin])
      .status()
      .expect("failed to execute");
    assert!(res.success());
    let expected = std::fs::read(test_file).unwrap();
    let generated = std::fs::read(&out_bin).unwrap();
    assert_eq!(expected, generated);
    _ = std::fs::remove_file(&out_bin);
  }

  fn _decode_table() {
    for (i, entry) in DECODE_TABLE1.iter().enumerate() {
      if let Some(entry) = entry {
        println!("{i:016b} {entry:?}");
      }
    }
    assert!(false);
  }
}
