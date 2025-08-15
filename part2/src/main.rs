use clap::Parser;

fn main() -> std::io::Result<()> {
  haversine::Args::parse().execute()
}
