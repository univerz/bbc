use anyhow::Result;
use argh::FromArgs;
use bbc::{machine::Machine, ProverResult};
use rayon::prelude::*;
use std::{
    io,
    panic::RefUnwindSafe,
    sync::mpsc::{self, Receiver},
    thread,
};

#[derive(FromArgs, Debug)]
/// Let's simulate
pub struct Args {
    /// number of threads; 0 == rayon default (==max)
    #[argh(option, default = "0")]
    threads: usize,
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

    rayon::ThreadPoolBuilder::new().num_threads(args.threads).build_global()?;

    // stdin lock is not Send...
    let (sender, receiver) = mpsc::channel();
    thread::spawn(move || io::stdin().lines().try_for_each(|line| sender.send(line.unwrap())));

    match args.cmd {
        SubCommand::CPS(cps) => {
            do_it(receiver, |machine| bbc::skelet_cps::CPS::prove(&machine, cps.max_segment_size));
        }
    }
    Ok(())
}

pub fn do_it<F: Fn(Machine) -> R + RefUnwindSafe + Send + Sync, R: Into<ProverResult>>(
    receiver: Receiver<String>,
    f: F,
) {
    receiver.into_iter().par_bridge().for_each(|line| {
        let machine = Machine::from(&line);
        let ret = ProverResult::catch(|| f(machine));
        println!("{ret},{line}");
    });
}

// cat .data/bb5_undecided.std.txt | cargo run --release --bin prove -- cps 2
