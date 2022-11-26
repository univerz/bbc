use itertools::Itertools;
use std::{convert::TryInto, fmt};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, parse_display::Display, parse_display::FromStr)]
pub enum Direction {
    #[display("L")]
    Left = 0,
    #[display("R")]
    Right = 1,
}

impl Direction {
    #[inline(always)]
    /// do not use `opp().idx()`, it's slower than `opp_idx()`
    pub fn opp(self) -> Direction {
        if self == Direction::Left { Direction::Right } else { Direction::Left }
    }

    #[inline(always)]
    pub fn opp_idx(self) -> usize {
        1 - self as usize
    }

    #[inline(always)]
    pub fn idx(self) -> usize {
        self as usize
    }
}

impl Into<Direction> for usize {
    fn into(self) -> Direction {
        if self == Direction::Left.idx() { Direction::Left } else { Direction::Right }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Head {
    pub state: u8,
    pub direction: Direction,
}

impl fmt::Display for Head {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.direction == Direction::Left {
            write!(f, "<{}", (self.state + b'A') as char)
        } else {
            write!(f, "{}>", (self.state + b'A') as char)
        }
    }
}

#[derive(Copy, Clone)]
pub struct Transition {
    pub symbol: u8,
    pub head: Head,
}

impl Transition {
    pub fn define() -> Transition {
        Transition { symbol: 0, head: Head { state: 0, direction: Direction::Left } } // 0LA
    }

    pub fn first() -> Transition {
        Transition { symbol: 1, head: Head { state: 1, direction: Direction::Right } } // 1RB
    }

    pub fn last() -> Transition {
        Transition { symbol: 1, head: Head { state: 25, direction: Direction::Right } } // 1RZ
    }
}

impl fmt::Display for Transition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}{}{}",
            (self.symbol + b'0') as char,
            if self.head.direction == Direction::Left { 'L' } else { 'R' },
            (self.head.state + b'A') as char
        )
    }
}

impl fmt::Debug for Transition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

// TODO: benchmark that version with dynamic symbol count
#[derive(Clone)]
pub struct Machine {
    machine: Vec<[Option<Transition>; 2]>,
}

impl Machine {
    #[inline]
    pub fn get_transition(&self, symbol: u8, state: u8) -> Option<Transition> {
        self.machine[state as usize][symbol as usize]
    }

    pub fn from(machine: &str) -> Machine {
        fn trans(trans: &[u8]) -> Option<Transition> {
            let [mut symbol, mut direction, mut state]: [u8; 3] = trans.try_into().unwrap();
            match symbol as char {
                'A'..='Z' => (state, symbol, direction) = (symbol, direction, state), // marxen's format B1R -> 1RB
                '-' => return None,
                _ => (),
            }
            Some(Transition {
                symbol: symbol - b'0',
                head: Head {
                    direction: if direction == b'L' { Direction::Left } else { Direction::Right },
                    state: state - b'A',
                },
            })
        }

        let machine = machine.trim();
        let it: Box<dyn Iterator<Item = &[u8]>> = if machine.contains("_") {
            // new format
            Box::new(machine.split("_").map(|row| row.as_bytes().chunks(3)).flatten())
        } else if machine.contains(" ") {
            // classic & ligocki double space
            Box::new(machine.split_whitespace().map(str::as_bytes))
        } else {
            // compact bbchallenge.org
            Box::new(machine.as_bytes().chunks(3))
        };

        Machine { machine: it.map(trans).tuples().map(|(s0, s1)| [s0, s1]).collect() }
    }

    #[inline(always)]
    pub fn states(&self) -> u8 {
        self.machine.len() as u8
    }

    #[inline(always)]
    pub fn symbols(&self) -> u8 {
        2
    }
}

impl fmt::Display for Machine {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.machine.iter().enumerate().try_for_each(|(idx, state)| {
            if idx != 0 {
                write!(f, "_")?;
            }
            state.iter().try_for_each(|t| t.map(|t| write!(f, "{}", t)).unwrap_or_else(|| write!(f, "---")))
        })
    }
}
