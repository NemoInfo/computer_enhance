mod generate;
use std::path::PathBuf;

use generate::{GenerationArgs, GenerationMethod};

mod compute;

/*********************
*  Argument Parsing  *
*********************/
use clap::{builder::ValueParser, CommandFactory, Parser, Subcommand};
use regex::Regex;

/// Program that generates haversine input
#[derive(Debug, Parser)]
#[command(version, about, long_about = None)]
pub struct Args {
  #[command(subcommand)]
  command: Option<Command>,
}

#[derive(Debug, Subcommand)]
enum Command {
  Generate(GenerationArgs),
  /// Clean JSON / answer files that match the output naming convetion from a directory
  Clean {
    /// Output dir to save JSON/answer file to
    #[clap(default_value = "data/", value_parser = dir_name_parser())]
    output_dir: PathBuf,
  },
}

impl Args {
  pub fn execute(&self) -> std::io::Result<()> {
    use Command::*;
    match &self.command {
      None => Self::command().print_help(),
      Some(Generate(gen)) => gen.execute(),
      Some(Clean { output_dir }) => {
        let re = Regex::new(&format!(r"^data_({})_(\d+)_(\d+)$", GenerationMethod::lowercase_options().join("|")))
          .map_err(|_| std::io::ErrorKind::Other)?;
        for path in output_dir.read_dir()?.filter_map(|x| x.ok().filter(|x| x.path().is_file()).map(|x| x.path())) {
          let Some(stem) = path.file_stem().unwrap().to_str() else {
            continue;
          };
          if re.is_match(stem) {
            std::fs::remove_file(&path)?;
            println!("Removed file: {}", path.to_string_lossy());
          }
        }
        Ok(())
      }
    }
  }
}

pub fn bytes_of<T>(t: &T) -> &[u8] {
  unsafe { std::slice::from_raw_parts((t as *const T).cast::<u8>(), core::mem::size_of::<T>()) }
}

pub fn chunked<I>(a: impl IntoIterator<Item = I>, chunk_size: usize) -> impl Iterator<Item = Vec<I>> {
  let mut a = a.into_iter();
  std::iter::from_fn(move || Some(a.by_ref().take(chunk_size).collect()).filter(|chunk: &Vec<_>| !chunk.is_empty()))
}

pub fn non_zero_usize_parser() -> ValueParser {
  ValueParser::new(|s: &str| match s.parse::<usize>().map_err(|_| "must be an unsigned integer")? {
    0 => Err("must be > 0".to_string()),
    val => Ok(val),
  })
}

pub fn dir_name_parser() -> ValueParser {
  ValueParser::new(|s: &str| {
    let path_buf = PathBuf::from(s);
    if path_buf.is_dir() {
      Ok(path_buf)
    } else {
      Err("must be directory name")
    }
  })
}
