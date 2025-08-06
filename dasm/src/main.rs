#![allow(non_snake_case)]

mod table;
use crate::table::DECODE_TABLE;

mod types;
use crate::types::*;

mod exec;

use std::{
  ffi::OsString,
  path::{Path, PathBuf},
};

fn allocate_memory_pow2(size_pow2: u32) -> Vec<u8> {
  vec![0; 1usize << size_pow2]
}

fn simulate<P: AsRef<Path>>(path: P) -> Result<String, ()> {
  let mut program = Program::new(20);
  Ok(program.simulate_from_file(path))
}

fn dasm<P: AsRef<Path>>(path: P) -> Result<String, ()> {
  let mem_pow2 = 20;
  let mut memory = allocate_memory_pow2(20);
  let mut sa = SegmentedAccess::new_memory_pow2(mem_pow2, &mut memory);
  let mut bytes_to_read = sa.load_memory_from_file(path, 0) as u16;
  let mut lines = vec![];

  while bytes_to_read > 0 {
    let instr = sa.decode_and_consume(&DECODE_TABLE);
    bytes_to_read -= instr.size;
    lines.push(instr.print_asm());
  }

  Ok(lines.join("\n"))
}

fn read_sim_write<P: AsRef<Path>>(path: P) -> std::io::Result<PathBuf> {
  use std::io::ErrorKind;

  let path = path.as_ref();
  let asm = simulate(path).expect("disassembly failed");
  println!("{asm}");

  let stem = path.file_stem().ok_or(ErrorKind::InvalidInput)?;
  let mut file_name = OsString::from(stem);
  let asm = format!("; {} disassembly\nbits 16\n{asm}", file_name.to_string_lossy());

  file_name.push("_simulation");

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

  let mut files = vec![];
  let mut f: fn(_) -> std::io::Result<PathBuf> = read_dasm_write;

  for arg in args {
    match arg.as_str() {
      "--sim" => f = read_sim_write,
      arg => files.push(arg.to_string()),
    }
  }

  let max_arg_len = files.iter().map(|s| s.len()).max();

  for arg in files {
    match f(arg.clone()) {
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

  // #[test]
  fn _decode_table() {
    for (i, entry) in DECODE_TABLE.iter().enumerate() {
      if let Some(entry) = entry {
        println!("{i:016b} {entry:?}");
      }
    }
    assert!(false);
  }

  #[test]
  fn vibe_rust() {
    let mut v = vec![0; 10];
    let ptr = v.as_mut_ptr();

    let s1 = unsafe { std::slice::from_raw_parts_mut(ptr, 10) };
    let s2 = unsafe { std::slice::from_raw_parts_mut(ptr, 10) };

    s1[0] = 1;
    s2[0] = 2;

    assert_eq!(2, v[0]);
  }
}
