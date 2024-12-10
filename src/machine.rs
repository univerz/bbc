use std::{fmt, str::FromStr};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, parse_display::Display)]
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

impl Head {
    pub fn new() -> Head {
        Head { state: 0, direction: Direction::Left }
    }

    #[inline]
    fn state_char(&self) -> char {
        (self.state + b'A') as char
    }
}

impl fmt::Display for Head {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.direction == Direction::Left {
            write!(f, "<{}", self.state_char())
        } else {
            write!(f, "{}>", self.state_char())
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Transition {
    pub symbol: u8,
    pub head: Head,
}

impl Transition {
    pub fn first() -> Transition {
        Transition { symbol: 1, head: Head { state: b'B' - b'A', direction: Direction::Right } } // 1RB
    }

    #[inline]
    pub fn last() -> Transition {
        Transition { symbol: 1, head: Head { state: b'Z' - b'A', direction: Direction::Right } } // 1RZ
    }
}

impl fmt::Display for Transition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}{}", (self.symbol + b'0') as char, self.head.direction, self.head.state_char())
    }
}

impl fmt::Debug for Transition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Machine {
    machine: Vec<Option<Transition>>,
    pub states: u8,
    pub symbols: u8,
}

impl Machine {
    pub fn first(states: u8, symbols: u8) -> Machine {
        let mut machine = vec![None; (states * symbols) as usize];
        machine[0] = Some(Transition::first());
        Machine { machine, states, symbols }
    }

    #[inline]
    fn idx(&self, state: u8, symbol: u8) -> usize {
        (state * self.symbols + symbol) as usize
    }

    #[inline]
    pub fn get_transition(&self, head: Head, symbol: u8) -> Option<Transition> {
        self.machine[self.idx(head.state, symbol)]
    }
}

impl FromStr for Machine {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut states = 1;
        let it = s.trim().as_bytes().into_iter().map(|b| *b);
        let machine: Vec<_> = it
            .filter(|&b| {
                if b == b'_' {
                    states += 1;
                    false
                } else {
                    true
                }
            })
            .array_chunks()
            .map(|[symbol, direction, state]| {
                (symbol != b'-').then(|| {
                    let direction = if direction == b'L' { Direction::Left } else { Direction::Right };
                    Transition { symbol: symbol - b'0', head: Head { direction, state: state - b'A' } }
                })
            })
            .collect();
        let symbols = machine.len() as u8 / states;
        assert_eq!((states * symbols) as usize, machine.len());

        Ok(Machine { machine, symbols, states })
    }
}

impl fmt::Display for Machine {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut delim = 0;
        self.machine.iter().try_for_each(|t| {
            if delim == self.symbols {
                write!(f, "_")?;
                delim = 0;
            }
            delim += 1;
            t.map(|t| write!(f, "{}", t)).unwrap_or_else(|| write!(f, "---"))
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn machine() {
        let data = [" 1RB1LC_1RC1RB_1RD0LE_1LA1LD_1RZ0LA", "1RB2LA1RA2LB2LA_0LA2RB3RB4RA1RH ", "1RB2LB1RH_2LA2RB1LB"];
        for s in data {
            let m: Machine = s.parse().unwrap();
            assert_eq!(s.trim(), &m.to_string());
        }
    }
}
