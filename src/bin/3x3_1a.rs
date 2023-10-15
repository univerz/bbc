use argh::FromArgs;
use bbc::machine::{Direction, Head, Machine};
use color_eyre::eyre::Result;
use derivative::Derivative;
use owo_colors::OwoColorize;
use std::collections::VecDeque;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::{
    fmt,
    io::{self, Write},
    str::FromStr,
};
use termion::{event::Key, input::TermRead, raw::IntoRawMode, screen::IntoAlternateScreen};

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Err {
    Halt,
    StepLimit,
    Interesting,
    Unreachable,
}

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
                        Self::compress(tape, blocks)?;
                    }
                    tape.extend_from_slice(blocks[block as usize]) // TODO: visited
                }
                Item::Unreachable => return Err(Err::Unreachable),
            }
        }
        Ok(0)
    }

    fn push_symbol(symbol: u8, tape: &mut Tape, blocks: RefBlocks) -> Result<(), Err> {
        tape.push(Item::S(symbol));
        Self::compress(tape, blocks)
    }

    fn compress(tape: &mut Tape, blocks: RefBlocks) -> Result<(), Err> {
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
            tape.push(Item::Exp { block: removed, exp: 1, visited: true });
            // if removed == 4 {
            //     return Err(Err::Interesting);
            // }
        }
        Ok(())
    }

    fn leap(&mut self, _blocks: RefBlocks) -> Result<bool, Err> {
        // return Ok(false);
        //
        let [ltape, rtape] = &mut self.tape;

        use Item::*;

        if self.head.state == 0 && self.head.direction == Direction::Left {
            // c^n <A b^m -> <A b^(n+m)    cargo run --bin 3x3_1a 8 0 --conf "! c^5 <A b^6 !"
            if let [.., Exp { block: 2, exp: exp_c, .. }] = ltape.as_slice() {
                if let [.., Exp { block: 1, exp: exp_b, .. }] = rtape.as_mut_slice() {
                    *exp_b += exp_c;
                    ltape.pop();
                    return Ok(true);
                }
            }
        }
        if self.head.state == 2 && self.head.direction == Direction::Left {
            // c^n <C 1 b^m -> <C 1 b^(n+m)    cargo run --bin 3x3_1a 8 0 --conf "! c^5 <C 1 b^6 !"
            if let [.., Exp { block: 2, exp: exp_c, .. }] = ltape.as_slice() {
                if let [.., Exp { block: 1, exp: exp_b, .. }, S(1)] = rtape.as_mut_slice() {
                    *exp_b += exp_c;
                    ltape.pop();
                    return Ok(true);
                }
            }
        }
        if self.head.state == 0 && self.head.direction == Direction::Right {
            // c^n A> b^m -> c^(n+m) A>    cargo run --bin 3x3_1a 8 0 --conf "! c^5 A> b^6 !"
            if let [.., Exp { block: 2, exp: exp_c, .. }] = ltape.as_mut_slice() {
                if let [.., Exp { block: 1, exp: exp_b, .. }] = rtape.as_slice() {
                    *exp_c += exp_b;
                    rtape.pop();
                    return Ok(true);
                }
            }
        }
        if self.head.state == 0 && self.head.direction == Direction::Right {
            // c^n 2 A> b^m -> c^(n+m) 2 A>    cargo run --bin 3x3_1a 8 0 --conf "! c^5 2 A> b^6 !"
            if let [.., Exp { block: 2, exp: exp_c, .. }, S(2)] = ltape.as_mut_slice() {
                if let [.., Exp { block: 1, exp: exp_b, .. }] = rtape.as_slice() {
                    *exp_c += exp_b;
                    rtape.pop();
                    return Ok(true);
                }
            }
        }
        if self.head.state == 1 && self.head.direction == Direction::Right {
            // b^n B> b^m -> b^(n+m) B>    cargo run --bin 3x3_1a 8 0 --conf "! b^5 B> b^6 !"
            if let [.., Exp { block: 1, exp: exp_to, .. }] = ltape.as_mut_slice() {
                if let [.., Exp { block: 1, exp: exp_from, .. }] = rtape.as_slice() {
                    *exp_to += exp_from;
                    rtape.pop();
                    return Ok(true);
                }
            }
        }

        Ok(false)
    }

    fn run(&mut self, machine: &Machine, blocks: RefBlocks, cfg: Config) -> Result<(), Err> {
        while self.sim_step < cfg.sim_step_limit {
            if !self.leap(blocks)? {
                let symbol = Self::pop_symbol(&mut self.tape[self.head.direction.idx()], blocks)?;
                let trans = machine.get_transition(self.head, symbol).ok_or(Err::Halt)?;
                self.head = trans.head;
                Self::push_symbol(trans.symbol, &mut self.tape[self.head.direction.opp_idx()], blocks)?;
            }

            let show = false;
            // self
            // .tape
            // .iter()
            // .any(|tape| tape.iter().any(|item| matches!(item, Item::Exp{ block, ..} if *block > 0)));

            self.sim_step += 1;
            if show || self.sim_step & ((1 << cfg.print_mod) - 1) == 0 {
                if show {
                    print!("{}", "# ".bright_red());
                }
                println!("{self}");
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
        if token.ends_with(":") {
            conf.sim_step = token[0..token.len() - 1].parse().unwrap();
            continue;
        }
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
    /// simulation step limit, 2^n
    #[argh(positional)]
    sim_step_limit: u32,
    /// how often to print configuration, 2^n
    #[argh(positional)]
    print_mod: u8,
    /// starting configuration
    #[argh(option, default = "Configuration::new()")]
    conf: Configuration,
    /// tui mode
    #[argh(switch, short = 't')]
    tui: bool,
}

#[derive(Clone, Copy, Debug)]
struct Config {
    sim_step_limit: usize,
    print_mod: u8,
}

// new run:               cargo run --release --bin on1 60 30
// explore configuration: cargo run --release --bin on1 8 0 --conf "!  A> a^4 1010 a^2 11"
// explore in tui:        cargo run --release --bin on1 8 0 --conf "a^61652 10 E>  d^11104" --tui
fn main() -> Result<()> {
    color_eyre::install()?;

    // return transcode();

    let machine: Machine = "1RB2RA1LC_2LC1RB2RB_---2LA1LA".parse().unwrap();
    let args: Args = argh::from_env();
    let cfg = Config { sim_step_limit: 2usize.checked_pow(args.sim_step_limit).unwrap(), print_mod: args.print_mod };
    let mut conf = args.conf;
    // dbg!(cfg);
    println!("{}", conf);

    let block_a = parse("12")?;
    let block_b = parse("11")?;
    let block_c = parse("22")?;

    let blocks = vec![block_a.as_slice(), block_b.as_slice(), block_c.as_slice()];
    // dbg!(&blocks);

    if args.tui {
        tui(conf, &machine, &blocks, cfg)?;
    } else {
        let ret = conf.run(&machine, &blocks, cfg);
        println!("{conf}");
        dbg!(&ret);
    }

    Ok(())
}

fn tui(mut conf: Configuration, machine: &Machine, blocks: RefBlocks, mut cfg: Config) -> Result<()> {
    let stdin = io::stdin();
    let mut screen = io::stdout().into_raw_mode()?.into_alternate_screen()?;
    write!(screen, "{}", termion::cursor::Hide).unwrap();

    let mut speed = cfg.print_mod;
    cfg.print_mod = 63; // do not print inside conf::run

    let mut keys = stdin.keys();
    let mut state: Result<(), Err> = Ok(());
    let mut history: VecDeque<(u8, Result<(), Err>, Configuration)> = VecDeque::new();
    loop {
        write!(
            screen,
            "{}{}q: quit, j: next step, k: previous, h: slow down, l: speed up; current step speed: 2^{} == {}\r\n",
            termion::clear::All,
            termion::cursor::Goto(1, 1),
            speed,
            (1 << speed).bright_white()
        )?;
        write!(screen, "history size: {}, speed stack ('a' + speed):\r\n\r\n", history.len().bright_white(),)?;
        history.iter().take(100).rev().try_for_each(|(speed, _, _)| write!(screen, "{}", (speed + b'a') as char))?;
        write!(screen, "\r\n\r\nstate: {:?}\r\n\r\n{}", state.bright_white(), conf)?;
        screen.flush()?;

        match keys.next().unwrap().unwrap() {
            Key::Char('q') => break,
            Key::Char('j') if state.is_ok() || state == Err(Err::StepLimit) => {
                let step = 1 << speed;
                cfg.sim_step_limit = conf.sim_step + step;
                state = conf.run(machine, blocks, cfg);
                if history.len() > 1_000_000 {
                    history.pop_back();
                }
                history.push_front((speed, state, conf.clone()));
            }
            Key::Char('k') => {
                if let Some((_, s, c)) = history.pop_front() {
                    state = s;
                    conf = c;
                }
            }
            Key::Char('h') => speed = speed.saturating_sub(1),
            Key::Char('l') => speed = (speed + 1).min(30),
            _ => {}
        }
    }
    write!(screen, "{}", termion::cursor::Show).unwrap();
    Ok(())
}

#[allow(unused)]
fn transcode() -> Result<()> {
    let file = File::open("/home/univerz/projects/bbc/no1_3_21")?;
    let lines = BufReader::new(file).lines();

    for line in lines {
        let line = String::from_utf8(strip_ansi_escapes::strip(line?))?;
        // dbg!(&line);
        let conf: Configuration = line.parse()?;
        // dbg!(&conf);

        fn fmt_symbol(item: &Item) {
            match *item {
                Item::S(s) => print!("{}", s.italic()),
                Item::Exp { block, exp, visited } => {
                    if block > 0 {
                        print!(" {}", ((block + b'a') as char).yellow().bold());
                        let exp_ = if visited { format!("{}", exp.bold()) } else { format!("{}", exp) };
                        if exp > 1_000_000_000 {
                            print!("^{} ", exp_.bright_white());
                        } else {
                            print!("^{} ", exp_);
                        }
                    } else {
                        if exp % 2 == 1 {
                            print!("p");
                        }
                    }
                    // if visited { write!(f, "^{} ", exp.bold()) } else { write!(f, "^{} ", exp) }
                }
                Item::Unreachable => print!(" {} ", '!'.bright_red()),
            }
        }

        print!("{}:  ", conf.sim_step);
        conf.tape[0].iter().for_each(|item| fmt_symbol(item));
        print!(" {} ", conf.head.bright_green().bold());
        conf.tape[1].iter().rev().for_each(|item| fmt_symbol(item));
        println!();
    }

    Ok(())
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
        for inp in ["0: 1111 a^7 01 a^3 11 b^14 11 b^20 11 a^137 0101 C> 0 a^3117 10 a^141880 !", "0: 0 <A 1"] {
            let conf: Configuration = inp.parse()?;
            assert_eq!(
                inp.split_whitespace().collect::<String>(),
                String::from_utf8(strip_ansi_escapes::strip(conf.to_string()))?.split_whitespace().collect::<String>()
            );
        }
        Ok(())
    }
}
