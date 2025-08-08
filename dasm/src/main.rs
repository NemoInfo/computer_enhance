#![allow(non_snake_case)]

mod clocks;
mod exec;
mod table;
mod types;

use crate::types::*;

use std::{
  ffi::OsString,
  path::{Path, PathBuf},
};

fn simulate<P: AsRef<Path>>(path: P) -> Result<String, ()> {
  let args: Vec<_> = std::env::args().into_iter().collect();
  let time = args.iter().position(|s| s == &"--time".to_string()).map(|i| args[i + 1].as_str().try_into().unwrap());

  let mut program = Program::new(20);
  Ok(program.simulate_from_file(path, true, time))
}

fn decode<P: AsRef<Path>>(path: P) -> Result<String, ()> {
  let args: Vec<_> = std::env::args().into_iter().collect();
  let time = args.iter().position(|s| s == &"--time".to_string()).map(|i| args[i + 1].as_str().try_into().unwrap());

  let mut program = Program::new(20);
  Ok(program.simulate_from_file(path, false, time))
}

fn read_simulate_dump<P: AsRef<Path>>(path: P) -> std::io::Result<PathBuf> {
  let args: Vec<_> = std::env::args().into_iter().collect();
  let time = args.iter().position(|s| s == &"--time".to_string()).map(|i| args[i + 1].as_str().try_into().unwrap());

  let mut program = Program::new(20);
  let asm = program.simulate_from_file(&path, true, time);
  let dump = program.dump();

  let args: Vec<_> = std::env::args().into_iter().collect();
  if !args.contains(&"--quiet".into()) {
    println!("{asm}");
  }

  let mut out_path = path.as_ref().as_os_str().to_owned();
  out_path.push(".data");

  std::fs::write(&out_path, dump)?;

  Ok(out_path.into())
}

fn read_simulate_write<P: AsRef<Path>>(path: P) -> std::io::Result<PathBuf> {
  use std::io::ErrorKind;

  let path = path.as_ref();
  let asm = simulate(path).expect("disassembly failed");

  let args: Vec<_> = std::env::args().into_iter().collect();
  if !args.contains(&"--quiet".into()) {
    println!("{asm}");
  }

  let stem = path.file_stem().ok_or(ErrorKind::InvalidInput)?;
  let mut file_name = OsString::from(stem);
  let asm = format!("; {} disassembly\n{asm}", file_name.to_string_lossy());

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

fn read_decode_write<P: AsRef<Path>>(path: P) -> std::io::Result<PathBuf> {
  use std::io::ErrorKind;

  let path = path.as_ref();
  let asm = decode(path).expect("disassembly failed");

  let args: Vec<_> = std::env::args().into_iter().collect();
  if !args.contains(&"--quiet".into()) {
    println!("{asm}");
  }

  let stem = path.file_stem().ok_or(ErrorKind::InvalidInput)?;
  let mut file_name = OsString::from(stem);
  let asm = format!("; {} disassembly\n{asm}", file_name.to_string_lossy());

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
  let mut f: fn(_) -> std::io::Result<PathBuf> = read_decode_write;
  let mut skip: bool = false;
  for arg in args {
    if skip {
      skip = false;
      continue;
    }
    match arg.as_str() {
      "--sim" => f = read_simulate_write,
      "--dump" => f = read_simulate_dump,
      "--quiet" => continue,
      "--time" => skip = true,
      arg => files.push(arg.to_string()),
    }
  }

  let max_arg_len = files.iter().map(|s| s.len()).max();

  for arg in files {
    match f(arg.clone()) {
      Err(e) => eprintln!("{arg: >width$}: {e}", width = max_arg_len.unwrap()),
      Ok(m) => println!("{arg: >width$}: Wrote {}", m.to_string_lossy(), width = max_arg_len.unwrap()),
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::table::DECODE_TABLE;

  #[rstest::rstest]
  #[case("../assets/part1/listing_0037_single_register_mov")]
  #[case("../assets/part1/listing_0038_many_register_mov")]
  #[case("../assets/part1/listing_0039_more_movs")]
  #[case("../assets/part1/listing_0040_challenge_movs")]
  #[case("../assets/part1/listing_0041_add_sub_cmp_jnz")]
  //#[case("../assets/part1/listing_0042_completionist_decode")]
  fn decode(#[case] test_file: &str) {
    println!("test: {test_file}");
    let out_asm = read_decode_write(&test_file).unwrap();
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

  #[test]
  fn test_multiple_program_load() {
    let files = ["../assets/part1/listing_0039_more_movs", "../assets/part1/listing_0040_challenge_movs"];
    let cxs = [-12i16 as u16, 0];

    let mut program = Program::new(20);
    for (file, cx) in files.iter().zip(cxs) {
      program.simulate_from_file(file, true, None);
      assert_eq!(program.reg[reg::CX], cx);
    }

    assert_eq!(program.reg[reg::IP], 80);
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
}
