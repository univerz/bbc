use anyhow::{anyhow, Result};
use argh::FromArgs;

use bbc::machine::Machine;

#[derive(FromArgs, Debug)]
/// Let's simulate
pub struct Args {
    /// machine to load
    #[argh(positional)]
    pub machine: String,
    /// whether or not to push a few newlines at the end
    #[argh(switch, short = 's')]
    scroll: bool,
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
    /// segment size
    #[argh(option, default = "bbc::skelet_cps::SegmentSizes([0,0,0])")]
    pub segment_sizes: bbc::skelet_cps::SegmentSizes,
    /// max segment size
    #[argh(option, default = "20")]
    pub max_segment_size: usize,
}

fn main() -> Result<()> {
    let args: Args = argh::from_env();
    let machine: Machine = args.machine.parse().unwrap();
    match args.cmd {
        SubCommand::CPS(cps) => {
            let ret = if !cps.segment_sizes.0.contains(&0) {
                bbc::skelet_cps::CPS::prove_size(&machine, cps.segment_sizes).map_err(|e| anyhow!("{e:?}"))
            } else {
                bbc::skelet_cps::CPS::prove(&machine, cps.max_segment_size).ok_or_else(|| anyhow!("undecided"))
            }?;
            println!("{ret}");
        }
    }
    // let mut bfs = Bfs::from_str(&args.machine);
    // let res = bfs.run();
    // println!("{res:#?}");
    if args.scroll {
        (0..70).for_each(|_| println!());
    }
    Ok(())
}

// cargo run --release --bin tui --features=ui_tui -- 1RB0LD_1LC1RC_1LA0RC_---0LE_0RB1LD cps --segment-size 1,2,3
// remove --features=ui_tui to skip most of the (debug) output
