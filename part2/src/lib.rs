mod generate;
use compute::{reference_haversine, Pair, EARTH_RADIUS};
use generate::{pair_to_json, GenerationArgs};

mod compute;

use std::{
  io::{BufWriter, Write},
  path::Path,
};

/*********************
*  Argument Parsing  *
*********************/
use clap::{CommandFactory, Parser, Subcommand};

/// Program that generates haversine input
#[derive(Debug, Parser)]
#[command(version, about, long_about = None)]
pub struct Args {
  #[command(subcommand)]
  command: Option<Command>,
}

impl Args {
  pub fn execute(&self) -> std::io::Result<()> {
    match &self.command {
      None => Self::command().print_help(),
      Some(cmd) => cmd.execute(),
    }
  }
}

#[derive(Debug, Subcommand)]
enum Command {
  Generate(GenerationArgs),
}

impl Command {
  pub fn execute(&self) -> std::io::Result<()> {
    match &self {
      Command::Generate(gen) => {
        let dir = Path::new("data/");
        assert!(dir.is_dir(), "{dir:?} is not directory");

        let mut json_writer: Box<dyn FnMut(Pair) -> Result<(), std::io::Error>> = if !gen.no_json {
          let json_name: String = gen.file_name("json");
          let json_path = dir.join(json_name);
          let mut json_writer = BufWriter::new(std::fs::File::create(json_path)?);
          json_writer.write_all(r#"{"pairs":["#.as_bytes())?;
          Box::new(move |pair| json_writer.write_all(pair_to_json(&pair).as_bytes()))
        } else {
          Box::new(move |_| Ok(()))
        };

        let mut bin_writer: Box<dyn FnMut(f64) -> Result<(), std::io::Error>> = if !gen.no_answers {
          let bin_name: String = gen.file_name("");
          let bin_path = dir.join(bin_name);
          let mut bin_writer = BufWriter::new(std::fs::File::create(bin_path)?);
          Box::new(move |answer| bin_writer.write_all(bytes_of(&answer)))
        } else {
          Box::new(move |_| Ok(()))
        };

        let mut sum = 0.0;
        for pair in gen.generate_pairs() {
          let answer = reference_haversine(pair, EARTH_RADIUS);
          sum += answer;

          bin_writer(answer)?;
          json_writer(pair)?;
        }
        let mean = sum / gen.number as f64;

        if !gen.no_json {}

        println!("{}\nExpected sum: {mean}", gen.summary());

        Ok(())
      }
    }
  }
}

fn bytes_of<T>(t: &T) -> &[u8] {
  unsafe { std::slice::from_raw_parts((t as *const T).cast::<u8>(), core::mem::size_of::<T>()) }
}
