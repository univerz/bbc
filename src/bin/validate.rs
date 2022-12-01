use anyhow::Result;
use argh::FromArgs;
use std::io;

#[derive(FromArgs, Debug)]
/// Let's simulate
pub struct Args {
    #[argh(subcommand)]
    cmd: SubCommand,
}

#[derive(FromArgs, Debug)]
#[argh(subcommand)]
enum SubCommand {
    CPS(CPS),
}

#[derive(FromArgs, Debug)]
/// Closed Position Set
#[argh(subcommand, name = "cps")]
struct CPS {
    /// max segment size
    #[argh(positional)]
    pub max_segment_size: usize,
}

fn main() -> Result<()> {
    let args: Args = argh::from_env();
    match args.cmd {
        SubCommand::CPS(cps) => {
            io::stdin()
                .lines()
                .try_for_each(|line| bbc::skelet_cps::CPSCertif::validate(&line?, cps.max_segment_size))?;
        }
    }
    Ok(())
}

// cat .data/test_certif | cargo run --release --bin validate -- cps 2
