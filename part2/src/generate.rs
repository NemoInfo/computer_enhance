use crate::compute::{Pair, PairIter};

use clap::{Args, ValueEnum};
use rand::{Rng, SeedableRng};

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
pub enum GenerationMethod {
  Uniform,
  Cluster,
}

/// Generate random Haversine pairs
#[derive(Debug, Args)]
pub struct GenerationArgs {
  /// Random sampling method
  method: GenerationMethod,
  /// Seed of the random number generator (u64)
  seed: u64,
  /// Number of pairs to generate (usize)
  pub number: usize,
  /// Ommit json output file
  #[clap(long)]
  pub no_json: bool,
  /// Ommit answer binary file
  #[clap(long)]
  pub no_answers: bool,
}

impl GenerationArgs {
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
      format!("{method:?}").to_lowercase(),
      [".", ""][extention.is_empty() as usize],
    )
  }

  pub fn summary(&self) -> String {
    let Self { method, seed, number, .. } = *self;
    format!("Method: {}\nRandom seed: {seed}\nPair count: {number}", format!("{method:?}").to_lowercase())
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

#[allow(unused)]
fn generate_cluster_pairs<'a>(seed: u64, number: usize) -> PairIter {
  let mut rng = rand_chacha::ChaCha8Rng::seed_from_u64(seed);

  Box::new((0..number).map(move |_| todo!()))
}

pub fn pair_to_json([[x0, y0], [x1, y1]]: &Pair) -> String {
  format!(r#"  {{"x0":{x0:>20}, "y0":{y0:>20}, "x1":{x1:>20}, "y1":{y1:>20}}},"#)
}

//pub fn vec_pairs_to_json(pairs: &VecPairs) -> String {
//  let mut res = vec![r#"{"pairs":["#.into()];
//
//  for [[x0, y0], [x1, y1]] in pairs {
//    res.push(format!(r#"  {{"x0":{x0:>20}, "y0":{y0:>20}, "x1":{x1:>20}, "y1":{y1:>20}}},"#));
//  }
//  if pairs.len() > 0 {
//    res[pairs.len()].pop(); // Remove trailing comma
//  }
//
//  res.push("]}".into());
//  res.join("\n")
//}
