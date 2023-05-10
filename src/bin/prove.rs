use anyhow::{anyhow, Result};
use argh::FromArgs;
use bbc::{machine::Machine, ProverCount, ProverResult};
use crossbeam_channel::Receiver;
use rayon::prelude::*;
use std::{io, panic::RefUnwindSafe, thread};

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
    pub max_segment_size: u8,
}

fn main() -> Result<()> {
    let args: Args = argh::from_env();

    rayon::ThreadPoolBuilder::new().num_threads(args.threads).build_global()?;

    // stdin lock is not Send...
    let (sender, receiver) = crossbeam_channel::bounded(2usize.pow(12));
    thread::spawn(move || io::stdin().lines().try_for_each(|line| sender.send(line.unwrap())));

    let ret = match args.cmd {
        SubCommand::CPS(cps) => {
            let segment_space = bbc::skelet_cps::size2space(cps.max_segment_size);
            do_it(receiver, move |machine| bbc::skelet_cps::CPS::prove(&machine, &segment_space))?
        }
    };

    eprintln!("done {ret:?}");
    Ok(())
}

pub fn do_it<F: Fn(Machine) -> R + RefUnwindSafe + Send + Sync + 'static, R: Into<ProverResult>>(
    receiver: Receiver<String>,
    f: F,
) -> Result<ProverCount> {
    let (tx_done, rx_done) = crossbeam_channel::bounded(2usize.pow(18));

    let t = thread::spawn(move || {
        receiver.into_iter().par_bridge().for_each(|machine_str| {
            let machine: Machine = machine_str.parse().unwrap();
            let res = ProverResult::catch(|| f(machine));
            // it's faster to print from one thread
            tx_done.send((res, machine_str)).unwrap();
        });
    });

    let mut cnt = ProverCount::default();
    while let Ok((res, machine)) = rx_done.recv() {
        println!("{res},{machine}");
        cnt.update(res);
    }

    t.join().map_err(|_| anyhow!("join error"))?;
    Ok(cnt)
}

// cat .data/bb5_undecided.std.txt | cargo run --release --bin prove -- cps 2
