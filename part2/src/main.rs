use clap::Parser;

fn main() -> std::io::Result<()> {
  let args = haversine::Args::parse();
  args.execute()
}
