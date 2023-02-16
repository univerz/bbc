use argh::FromArgs;
use bbc::machine::{Direction, Head, Machine};
use bbc::ui_dbg;
use color_eyre::eyre::Result;
use derivative::Derivative;
use owo_colors::OwoColorize;
use std::{fmt, str::FromStr};

#[derive(Debug)]
pub enum Err {
    Halt,
    StepLimit,
    Unreachable,
}

// #[derive(Clone, Copy, Debug, PartialEq)]
// enum Item {
//     S(u8),
//     Exp { block: u8, exp: usize, visited: bool },
//     Unreachable,
// }
#[derive(Derivative)]
#[derivative(PartialEq)]
#[derive(Clone, Copy, Debug)]
enum Item {
    S(u8),
    Exp {
        block: u8,
        exp: usize,
        #[derivative(PartialEq = "ignore")] // compression
        visited: bool,
    },
    Unreachable,
}

type Tape = Vec<Item>;

type RefBlocks<'a> = &'a [&'a [Item]];

#[derive(Clone, Debug)]
struct Configuration {
    tape: [Tape; 2],
    head: Head,
    sim_step: usize,
}

impl Configuration {
    pub fn new() -> Configuration {
        Configuration {
            tape: [Vec::new(), Vec::new()],
            head: Head { state: 0, direction: Direction::Right },
            sim_step: 0,
        }
    }

    // TODO: convert base symbol & orient to  usize to save casts
    fn pop_symbol(tape: &mut Tape, blocks: RefBlocks) -> Result<u8, Err> {
        while let Some(last) = tape.pop() {
            match last {
                Item::S(symbol) => return Ok(symbol),
                Item::Exp { block, exp, visited: _ } => {
                    if exp > 1 {
                        tape.push(Item::Exp { block, exp: exp - 1, visited: true });
                        Self::compress(tape, blocks);
                    }
                    tape.extend_from_slice(blocks[block as usize])
                }
                Item::Unreachable => return Err(Err::Unreachable),
            }
        }
        Ok(0)
    }

    fn push_symbol(symbol: u8, tape: &mut Tape, blocks: RefBlocks) {
        tape.push(Item::S(symbol));
        Self::compress(tape, blocks);
    }

    fn compress(tape: &mut Tape, blocks: RefBlocks) {
        while let Some(removed) = blocks.iter().enumerate().find_map(|(idx, &block)| {
            tape.ends_with(block).then(|| {
                tape.truncate(tape.len() - block.len());
                idx as u8
            })
        }) {
            if let Some(Item::Exp { block, exp, visited }) = tape.last_mut() {
                if *block == removed {
                    *exp = exp.checked_add(1).unwrap();
                    *visited = true;
                    continue;
                }
            }
            tape.push(Item::Exp { block: removed, exp: 1, visited: true })
        }
    }

    fn leap(&mut self) -> bool {
        // return false;

        // ? A> (110)^2n -> (011)^2n A>
        if self.head.state == 0 && self.head.direction == Direction::Right {
            if let Some(&Item::Exp { block: 0, exp: mut item_exp, visited: _ }) = self.tape[1].last() {
                // we want (110)^n n>=2 to avoid degenerate n.div_mod == 0,1 case
                if item_exp > 1 {
                    self.tape[1].pop();
                    if item_exp % 2 == 1 {
                        item_exp -= 1;
                        self.tape[1].push(Item::Exp { block: 0, exp: 1, visited: false });
                    }
                    if let Some(Item::Exp { block: 0, exp, visited }) = self.tape[0].last_mut() {
                        *exp = exp.checked_add(item_exp).unwrap();
                        *visited = true;
                        ui_dbg!("\tleap a merge");
                        return true;
                    }
                    self.tape[0].push(Item::Exp { block: 0, exp: item_exp, visited: true });
                    ui_dbg!("\tleap a");
                    return true;
                }
            }
        }

        // (011)^a <C 10 (110)^b -> <C 10 (110)^(a+b)
        if self.head.state == 2 && self.head.direction == Direction::Left {
            if let Some(&Item::Exp { block: 0, exp: item_exp, visited: _ }) = self.tape[0].last() {
                // TODO: special case for <C 1 _ should speed it up
                // or even better - accelerate whole left bouncer part that looks like 01 ^ >  & < 10 ^
                let [ltape, rtape] = &mut self.tape;
                if let [.., Item::Exp { block: 0, exp, visited }, Item::S(0), Item::S(1)] = rtape.as_mut_slice() {
                    ltape.pop();
                    *exp = exp.checked_add(item_exp).unwrap();
                    *visited = true;
                    ui_dbg!("\tleap c");
                    return true;
                }
            }
        }

        false
    }

    /// runs until head leaves the tape, split & return new conf, C (`<S ABC` -> `<S AB` & `C`) & end conf string if in tui mode
    fn run(mut self, machine: &Machine, blocks: RefBlocks, cfg: Config) -> Result<(), Err> {
        while self.sim_step < cfg.sim_step_limit {
            if !self.leap() {
                let symbol = Self::pop_symbol(&mut self.tape[self.head.direction.idx()], blocks)?;
                let trans = machine.get_transition(symbol, self.head.state).ok_or(Err::Halt)?;
                self.head = trans.head;
                Self::push_symbol(trans.symbol, &mut self.tape[self.head.direction.opp_idx()], blocks);
            }
            self.sim_step += 1;
            if self.sim_step % cfg.print_mod == 0 {
                println!("{}", self);
            }
        }
        return Err(Err::StepLimit);
    }
}

impl fmt::Display for Configuration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn fmt_symbol(item: &Item, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match *item {
                Item::S(s) => write!(f, "{}", s.italic()),
                Item::Exp { block, exp, visited } => {
                    if block > 0 {
                        write!(f, " {}", ((block + b'a') as char).yellow().bold())?;
                    } else {
                        // write!(f, " {}^{} ", ((block + b'a') as char).bright_white(), exp.bold())
                        write!(f, " {}", ((block + b'a') as char))?;
                    }
                    // if visited { write!(f, "^{} ", exp.bold()) } else { write!(f, "^{} ", exp) }
                    let exp_ = if visited { format!("{}", exp.bold()) } else { format!("{}", exp) };
                    if exp > 1_000_000_000 { write!(f, "^{} ", exp_.bright_white()) } else { write!(f, "^{} ", exp_) }
                }
                Item::Unreachable => write!(f, " {} ", '!'.bright_red()),
            }
        }

        write!(f, "{}:  ", self.sim_step)?;
        self.tape[0].iter().try_for_each(|item| fmt_symbol(item, f))?;
        write!(f, " {} ", self.head.bright_green().bold())?;
        self.tape[1].iter().rev().try_for_each(|item| fmt_symbol(item, f))
    }
}

fn raw_parse(s: &str) -> Result<(Configuration, usize)> {
    let mut conf = Configuration::new();
    let mut dir = Direction::Left.idx();

    for token in s.split_whitespace() {
        if token.contains('<') {
            dir += 1;
            conf.head.direction = Direction::Left;
            conf.head.state = token.chars().skip(1).next().unwrap() as u8 - b'A';
            continue;
        }
        if token.contains('>') {
            dir += 1;
            conf.head.direction = Direction::Right;
            conf.head.state = token.chars().next().unwrap() as u8 - b'A';
            continue;
        }
        if let Some((block, exp)) = token.split_once("^") {
            conf.tape[dir].push(Item::Exp {
                block: block.chars().next().unwrap() as u8 - b'a',
                exp: exp.parse()?,
                visited: false,
            });
            continue;
        }
        if token == "!" {
            conf.tape[dir].push(Item::Unreachable);
            continue;
        }
        conf.tape[dir].extend(token.chars().map(|symbol| Item::S(symbol as u8 - b'0')))
    }
    Ok((conf, dir))
}

fn parse(s: &str) -> Result<Tape> {
    let (conf, dir) = raw_parse(s)?;
    assert_eq!(dir, Direction::Left.idx());

    Ok(conf.tape.into_iter().next().unwrap())
}

impl FromStr for Configuration {
    type Err = color_eyre::Report;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let (mut conf, dir) = raw_parse(s)?;
        assert_eq!(dir, Direction::Right.idx());
        conf.tape[dir].reverse();

        Ok(conf)
    }
}

#[derive(FromArgs, Debug)]
/// Let's simulate
struct Args {
    /// simulation step limit, 10^n
    #[argh(positional)]
    sim_step_limit: u32,
    /// how often to print configuration, 10^n
    #[argh(positional)]
    print_mod: u32,
    /// starting configuration
    #[argh(option, default = "Configuration::new()")]
    conf: Configuration,
    // #[argh(subcommand)]
    // cmd: SubCommand,
}

// #[derive(FromArgs, Debug)]
// #[argh(subcommand)]
// enum SubCommand {
//     Run,
//     Explore(Explore),
// }

// #[derive(FromArgs, Debug)]
// /// Closed Position Set
// #[argh(subcommand, name = "explore")]
// struct Explore {
//     /// initial configuration
//     #[argh(option, default = "bbc::skelet_cps::SegmentSizes([0,0,0])")]
//     pub segment_sizes: bbc::skelet_cps::SegmentSizes,
//     /// max segment size
//     #[argh(option, default = "20")]
//     pub max_segment_size: usize,
// }

#[derive(Clone, Copy, Debug)]
struct Config {
    sim_step_limit: usize,
    print_mod: usize,
}

fn main() -> Result<()> {
    color_eyre::install()?;

    let args: Args = argh::from_env();

    let machine = Machine::from("1RB1RD_1LC0RC_1RA1LD_0RE0LB_---1RC");

    // let cfg = Config { sim_step_limit: 10usize.checked_pow(14).unwrap(), ui_mod: 10usize.pow(9) };
    // let cfg = Config { sim_step_limit: 200, print_mod: 1 };
    // let cfg = Config { sim_step_limit: 3195500000, ui_mod: 500000 };
    let cfg = Config {
        sim_step_limit: 10usize.checked_pow(args.sim_step_limit).unwrap(),
        print_mod: 10usize.checked_pow(args.print_mod).unwrap(),
    };

    // dbg!(cfg);
    println!("{}", args.conf);

    let block_a = parse("011")?;
    let block_b = parse("11 a^15282 01 a^20689")?;
    let block_c = parse("a^144285 01 a^6153 01 a^3077 01 a^601 01 a^61653 01")?;
    let mut block_d = parse("a^144285 10 a^6153 10 a^3077 10 a^601 10 a^61653 10")?;
    block_d.reverse();

    let blocks = vec![block_a.as_slice(), block_b.as_slice(), block_c.as_slice(), block_d.as_slice()];
    // dbg!(&blocks);

    let ret = args.conf.run(&machine, &blocks, cfg);
    dbg!(&ret);

    Ok(())
    // TODO: splitnut to ma medzerach & nahradit ^ za mod & printnut len ak % 2 != 0, skompresovat do unikatnych blokov
    // spravit regulerne parse aby som sa mohol  mrknut ako to vyzera od nejakeho bodu ala akcelerovanie tej pravej strany
    // ale najprv to porovnat s mateonovym kompress vystupom
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn parse_block() -> Result<()> {
        let mut b = parse("a^601 10 a^61653 10")?;
        b.reverse();
        assert_eq!(b, parse("01 a^61653 01 a^601 ")?);
        Ok(())
    }

    #[test]
    fn parse_conf() -> Result<()> {
        for inp in ["1111 a^7 01 a^3 11 b^14 11 b^20 11 a^137 0101 C> 0 a^3117 10 a^141880 !", " 0 <A 1"] {
            let conf: Configuration = inp.parse()?;
            assert_eq!(
                inp.split_whitespace().collect::<String>(),
                String::from_utf8(strip_ansi_escapes::strip(conf.to_string())?)?
                    .split_once(':')
                    .unwrap()
                    .1
                    .split_whitespace()
                    .collect::<String>()
            );
        }
        Ok(())
    }
}

// impl<'a> bbc::ui::Display1<RefBlocks<'a>> for Configuration {
//     fn fmt(&self, blocks: RefBlocks, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         fn fmt_symbol(item: &Item, blocks: RefBlocks, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//             match item {
//                 Item::S(s) => write!(f, "{s}"),
//                 Item::Exp { block, exp } => write!(f, " {}^{} ", (block + b'a') as char, exp),
//                 Item::Unreachable => write!(f, " | "),
//             }
//         }

//         write!(f, "{}:  ", self.sim_step)?;
//         self.tape[0].iter().try_for_each(|item| fmt_symbol(item, blocks, f))?;
//         write!(f, " {} ", self.head)?;
//         self.tape[1].iter().rev().try_for_each(|item| fmt_symbol(item, blocks, f))
//     }
// }
