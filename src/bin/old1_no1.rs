use bbc::ui_dbg;
use color_eyre::Result;
use std::fmt;

use bbc::machine::{Direction, Head, Machine};

#[derive(Debug)]
pub enum Err {
    Halt,
    StepLimit,
    TotalStepLimit,
    ConfLimit,
}

type SimSymbol = usize;

#[derive(Clone, Debug)]
struct Configuration {
    tape: [Vec<SimSymbol>; 2],
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
    fn pop_symbol(tape: &mut Vec<usize>) -> u8 {
        if let Some(last) = tape.pop() {
            if last < 2 {
                last as u8
            } else {
                if last > 2 {
                    tape.push(last - 1);
                }
                tape.push(0);
                tape.push(1);
                1
            }
        } else {
            0
        }
    }

    fn push_symbol(symbol: u8, tape: &mut Vec<usize>) {
        if symbol == 1 {
            if tape.ends_with(&[0, 1]) {
                //  _ 01 + 1 -> _011
                tape.pop();
                tape.pop();
                if let Some(last) = tape.last_mut() {
                    // 011 01 + 1 -> 011^2
                    if *last > 1 {
                        *last = last.checked_add(1).unwrap();
                        return;
                    }
                }
                tape.push(2);
            } else if tape.as_slice() == &[1] {
                // _ 1 + 1 -> 011
                tape.pop();
                tape.push(2);
            } else {
                tape.push(1)
            }
        } else {
            tape.push(0);
        }
    }

    fn leap(&mut self) -> bool {
        // return false;

        // ? A> (110)^2n -> (011)^2n A>
        if self.head.state == 0
            && self.head.direction == Direction::Right
            && self.tape[1].last().map(|i| *i > 2).unwrap_or(false)
        // >2 because we want (110)^n n>=2 to avoid degenerate n.div_mod == 0,1 case
        {
            let last = self.tape[1].pop().unwrap();
            let remainder = 1 - last % 2; // last == exp+1
            if remainder == 1 {
                self.tape[1].push(2);
            }
            if let Some(rlast) = self.tape[0].last_mut() {
                if *rlast > 1 {
                    *rlast = rlast.checked_add(last - remainder - 1).unwrap();
                    ui_dbg!("\taaaaaaaaaaaaaaaaaa comp");
                    return true;
                }
            }
            self.tape[0].push(last - remainder); // last is >= 2
            ui_dbg!("\taaaaaaaaaaaaaaaaaa");
            return true;
        }
        // (011)^a <C 10 (110)^b -> <C 10 (110)^(a+b)
        if self.head.state == 2
            && self.head.direction == Direction::Left
            && self.tape[0].last().map(|a| *a > 1).unwrap_or(false)
            && self.tape[1].len() >= 3
        {
            // TODO: special case for <C 1 _ should speed it up considerably
            // or even better - accelerate whole left bouncer part that looks like 01 ^ >  & < 10 ^
            let [ltape, rtape] = &mut self.tape;
            let last3 = rtape.len() - 3;
            match &mut rtape[last3..] {
                [b, 0, 1] if *b > 1 => {
                    let a = ltape.pop().unwrap();
                    *b = b.checked_add(a - 1).unwrap();
                    ui_dbg!("\tcccc");
                    return true;
                }
                _ => (),
            }
        }
        false
    }

    /// runs until head leaves the tape, split & return new conf, C (`<S ABC` -> `<S AB` & `C`) & end conf string if in tui mode
    fn run(mut self, machine: &Machine, cfg: Config) -> Result<(), Err> {
        while self.sim_step < cfg.sim_step_limit {
            if !self.leap() {
                let symbol = Self::pop_symbol(&mut self.tape[self.head.direction.idx()]);
                let trans = machine.get_transition(symbol, self.head.state).ok_or(Err::Halt)?;
                self.head = trans.head;
                Self::push_symbol(trans.symbol, &mut self.tape[self.head.direction.opp_idx()]);
            }
            self.sim_step += 1;
            if self.sim_step % cfg.ui_mod == 0 {
                println!("{}", self);
            }
        }
        return Err(Err::StepLimit);
    }
}

impl fmt::Display for Configuration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn fmt_symbol(i: usize, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            // if i < 2 { write!(f, "{}", i) } else { write!(f, " (011)^{} ", i - 1) }
            if i < 2 { write!(f, "{}", i) } else { write!(f, " ^{} ", i - 1) }
        }

        write!(f, "{}:  ", self.sim_step)?;
        self.tape[0].iter().try_for_each(|&symbol| fmt_symbol(symbol, f))?;
        write!(f, " {} ", self.head)?;
        self.tape[1].iter().rev().try_for_each(|&symbol| fmt_symbol(symbol, f))
    }
}

#[derive(Clone, Copy, Debug)]
struct Config {
    sim_step_limit: usize,
    ui_mod: usize,
}

fn main() -> Result<()> {
    let machine = Machine::from("1RB1RD_1LC0RC_1RA1LD_0RE0LB_---1RC");
    let conf = Configuration::new();

    // let cfg = Config { sim_step_limit: 10usize.checked_pow(14).unwrap(), ui_mod: 10usize.pow(9) };
    // let cfg = Config { sim_step_limit: 10000000000, ui_mod: 100000 };
    let cfg = Config { sim_step_limit: 10usize.checked_pow(9).unwrap(), ui_mod: 10usize.pow(8) };

    let ret = conf.run(&machine, cfg);
    dbg!(&ret);

    Ok(())
    // TODO: splitnut to ma medzerach & nahradit ^ za mod & printnut len ak % 2 != 0, skompresovat do unikatnych blokov
    // spravit regulerne parse aby som sa mohol  mrknut ako to vyzera od nejakeho bodu ala akcelerovanie tej pravej strany
    // ale najprv to porovnat s mateonovym kompress vystupom
}
