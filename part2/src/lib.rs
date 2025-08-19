mod generate;
use std::path::PathBuf;

use compute::{reference_haversine, EARTH_RADIUS};
use generate::{GenerationArgs, GenerationMethod};

mod compute;

mod json;

/*********************
*  Argument Parsing  *
*********************/
use clap::{builder::ValueParser, CommandFactory, Parser, Subcommand};
use json::parse_json;
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
  Verify {
    /// Path of file to verify
    file: PathBuf,
  },
}

impl Args {
  pub fn execute(&self) -> std::io::Result<()> {
    use Command::*;
    match &self.command {
      None => Self::command().print_help(),
      Some(Generate(gen)) => gen.execute(),
      Some(Verify { file }) => {
        let json_path = file.with_extension("json");
        let answer_path = file.with_extension("answer");
        assert!(json_path.exists() && json_path.is_file());
        assert!(answer_path.exists() && answer_path.is_file());
        let json = std::fs::read_to_string(json_path)?;
        let answer = std::fs::read(answer_path)?;
        let answers: Vec<f64> =
          answer.chunks_exact(std::mem::size_of::<f64>()).map(|b| f64::from_le_bytes(b.try_into().unwrap())).collect();
        let json_val = parse_json(&json).unwrap_or_else(|e| panic!("Could not parse JSON:\n{}", e.to_string()));

        let json::Value::Object(map) = json_val else { panic!("expected map") };
        let pairs = map.get("pairs").expect("expected \"pairs\" entry");
        let json::Value::Array(pairs) = pairs else { panic!("pairs entry should be array") };
        for (pair, answer) in pairs.iter().zip(answers) {
          let json::Value::Object(pair) = pair else { panic!("expected map") };
          let json::Value::Number(json::Number::F64(x0)) = pair.get("x0").expect("x0 entry required") else { panic!() };
          let json::Value::Number(json::Number::F64(y0)) = pair.get("y0").expect("y0 entry required") else { panic!() };
          let json::Value::Number(json::Number::F64(x1)) = pair.get("x1").expect("x1 entry required") else { panic!() };
          let json::Value::Number(json::Number::F64(y1)) = pair.get("y1").expect("y1 entry required") else { panic!() };
          let computed = reference_haversine([[*x0, *y0], [*x1, *y1]], EARTH_RADIUS);
          println!("Expected: {answer}\nComputed: {computed}");
          assert_eq!(answer, computed);
        }

        Ok(())
      }
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
