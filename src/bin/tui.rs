use anyhow::{anyhow, Result};
use argh::FromArgs;
use std::result::Result as StdResult;

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
    #[argh(option, default = "[0, 0, 0]", from_str_fn(segment_size_from_str))]
    pub segment_sizes: bbc::skelet_cps::SegmentSizes,
    /// max segment size
    #[argh(option, default = "20")]
    pub max_segment_size: usize,
}

fn segment_size_from_str(s: &str) -> StdResult<bbc::skelet_cps::SegmentSizes, String> {
    let err = || format!("{s:?} should look like `1,2,3` where numbers are length of left, right & middle segment");
    s.split(",")
        .map(|i| i.parse::<usize>())
        .collect::<StdResult<Vec<_>, _>>()
        .map_err(|_| err())?
        .try_into()
        .map_err(|_| err())
}

fn main() -> Result<()> {
    let args: Args = argh::from_env();
    let machine = Machine::from(&args.machine);
    match args.cmd {
        SubCommand::CPS(cps) => {
            let ret = if !cps.segment_sizes.contains(&0) {
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
