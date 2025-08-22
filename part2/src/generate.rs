use std::{
  io::{BufWriter, Seek, Write},
  path::PathBuf,
};

use crate::{
  chunked,
  compute::{reference_haversine, Pair, PairIter, EARTH_RADIUS},
  dir_name_parser, non_zero_usize_parser, progress,
};

use clap::{Args, ValueEnum};
use rand::{Rng, SeedableRng};

macros::generation_method_enum!(Uniform, Cluster);

/// Generate random Haversine pairs
#[derive(Debug, Args)]
pub struct GenerationArgs {
  /// Random sampling method
  method: GenerationMethod,
  /// Seed of the random number generator (u64)
  seed: u64,
  /// Number of pairs to generate (usize)
  #[clap(value_parser = non_zero_usize_parser())]
  number: usize,
  /// Ommit JSON output file
  #[clap(long)]
  no_json: bool,
  /// Ommit answer binary file
  #[clap(long)]
  no_answers: bool,
  /// Output dir to save JSON/answer file to
  #[clap(long, default_value = "data/", value_parser = dir_name_parser())]
  output_dir: PathBuf,
  /// Number of pairs processed in a batch
  #[clap(long, default_value_t = 64_000, value_parser = non_zero_usize_parser())]
  batch_size: usize,
}

impl GenerationArgs {
  pub fn execute(&self) -> std::io::Result<()> {
    let mut json_writer = if !self.no_json {
      let json_name: String = self.file_name("json");
      let json_path = self.output_dir.join(json_name);
      let mut json_writer = BufWriter::new(std::fs::File::create(json_path)?);
      json_writer.write_all("{\"pairs\":[\n".as_bytes())?;
      Some(json_writer)
    } else {
      None
    };

    let mut answer_writer = if !self.no_answers {
      let answer_name: String = self.file_name("answer");
      let answer_path = self.output_dir.join(answer_name);
      let answer_writer = BufWriter::new(std::fs::File::create(answer_path)?);
      Some(answer_writer)
    } else {
      None
    };

    let mut sum = 0.;
    let mut pi = 0;
    for chunk in chunked(self.generate_pairs(), self.batch_size) {
      progress(self.number, pi, 20);
      pi += chunk.len();
      let json: Vec<_> = chunk.iter().map(|p| pair_to_json(p)).collect();
      let answers: Vec<_> = chunk.iter().map(|&p| reference_haversine(p, EARTH_RADIUS)).collect();
      sum += answers.iter().sum::<f64>();

      if let Some(w) = &mut json_writer {
        w.write_all(json.join(",\n").as_bytes())?;
        w.write_all(",\n".as_bytes())?;
      }

      if let Some(w) = &mut answer_writer {
        let bytes: &[u8] = unsafe {
          std::slice::from_raw_parts(answers.as_ptr() as *const u8, answers.len() * std::mem::size_of::<f64>())
        };
        w.write_all(bytes)?;
      }
    }
    let mean = sum / self.number as f64;
    progress(self.number, pi, 20);

    if let Some(w) = &mut json_writer {
      w.seek_relative(-2)?; // Overwrite trailing comma
      w.write_all("\n]}".as_bytes())?;
    }

    println!("{}\nExpected sum: \x1b[1m{mean}\x1b[0m", self.summary());

    Ok(())
  }

  pub fn generate_pairs<'a>(&'a self) -> PairIter {
    let Self { method, seed, number, .. } = *self;
    match method {
      GenerationMethod::Uniform => generate_uniform_pairs(seed, number).into_iter(),
      GenerationMethod::Cluster => generate_cluster_pairs(seed, number).into_iter(),
    }
  }

  pub fn file_name(&self, extention: &str) -> String {
    let Self { method, seed, number, .. } = *self;
    format!(
      "data_{}_{seed}_{number}{}{extention}",
      method.to_lowercase_string(),
      [".", ""][extention.is_empty() as usize],
    )
  }

  pub fn summary(&self) -> String {
    let Self { method, seed, number, .. } = *self;
    let red = "\x1b[32m";
    let reset = "\x1b[0m";
    let bold = "\x1b[1m";

    format!(
      "Method: {bold}{red}{}{reset}\nRandom seed: {seed}\nPair count: {number}",
      format!("{method:?}").to_lowercase()
    )
  }
}

fn generate_uniform_pairs<'a>(seed: u64, number: usize) -> PairIter {
  let mut rng = rand_chacha::ChaCha8Rng::seed_from_u64(seed);

  Box::new((0..number).map(move |_| {
    let x0 = rng.random_range(-180. ..180.);
    let x1 = rng.random_range(-180. ..180.);
    let y0 = rng.random_range(-90. ..90.);
    let y1 = rng.random_range(-90. ..90.);
    [[x0, y0], [x1, y1]]
  }))
}

fn generate_cluster_pairs<'a>(seed: u64, number: usize) -> PairIter {
  let mut rng = rand_chacha::ChaCha8Rng::seed_from_u64(seed);

  let x0r = rng.random_range(20. ..180.);
  let x1r = rng.random_range(20. ..180.);
  let y0r = rng.random_range(10. ..90.);
  let y1r = rng.random_range(10. ..90.);

  Box::new((0..number).map(move |_| {
    let x0 = rng.random_range(-x0r..x0r);
    let x1 = rng.random_range(-x1r..x1r);
    let y0 = rng.random_range(-y0r..y0r);
    let y1 = rng.random_range(-y1r..y1r);
    [[x0, y0], [x1, y1]]
  }))
}

pub fn pair_to_json([[x0, y0], [x1, y1]]: &Pair) -> String {
  format!(r#"  {{"x0":{x0:>20}, "y0":{y0:>20}, "x1":{x1:>20}, "y1":{y1:>20}}}"#)
}

mod macros {
  macro_rules! generation_method_enum {($($variant:ident),+ $(,)?) => {
  #[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
  pub enum GenerationMethod {
    $($variant),+
  }

  impl GenerationMethod {
    fn to_lowercase_string(&self) -> String {
      format!("{self:?}").to_lowercase()
    }

    pub fn lowercase_options() -> Vec<String> {
      vec![$(GenerationMethod::$variant.to_lowercase_string()),+]
    }
  }
  };}
  pub(crate) use generation_method_enum;
}
