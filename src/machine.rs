use itertools::Itertools;
use std::{convert::TryInto, fmt};

pub type Orientation = u8;

// TODO
// #[repr(usize)]
// enum Orientation {
//     Left = 0,
//     Right = 1,
// }

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Head {
    pub state: u8,
    pub orient: u8,
}

impl Head {
    pub fn new() -> Head {
        Head { state: 0, orient: 0 }
    }

    #[inline(always)]
    pub fn orient(&self) -> usize {
        self.orient as usize
    }

    #[inline(always)]
    pub fn op_orient(&self) -> usize {
        1 - self.orient as usize
    }
}

impl fmt::Display for Head {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.orient == 0 {
            write!(f, "<{}", (self.state + 'A' as u8) as char)
        } else {
            write!(f, "{}>", (self.state + 'A' as u8) as char)
        }
    }
}

#[derive(Copy, Clone)]
pub struct Transition {
    pub symbol: u8,
    pub head: Head,
}

impl Transition {
    pub fn undefined() -> Transition {
        Transition { symbol: u8::MAX, head: Head::new() }
    }

    pub fn is_undefined(&self) -> bool {
        self.symbol == u8::MAX
    }

    pub fn define() -> Transition {
        Transition { symbol: 0, head: Head { state: 0, orient: 0 } } // 0LA
    }

    pub fn first() -> Transition {
        Transition { symbol: 1, head: Head { state: 1, orient: 1 } } // 1RB
    }

    pub fn last() -> Transition {
        Transition { symbol: 1, head: Head { state: 25, orient: 1 } } // 1RZ
    }
}

impl fmt::Display for Transition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.is_undefined() {
            write!(
                f,
                "{}{}{}",
                (self.symbol + '0' as u8) as char,
                if self.head.orient == 0 { 'L' } else { 'R' },
                (self.head.state + 'A' as u8) as char
            )
        } else {
            write!(f, "---")
        }
    }
}

impl fmt::Debug for Transition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

// TODO: benchmark that version with dynamic symbol count & Option
#[derive(Clone)]
pub struct Machine {
    machine: Vec<[Transition; 2]>,
}

impl Machine {
    #[inline]
    pub fn get_transition(&self, symbol: u8, state: u8) -> Option<Transition> {
        let trans = self.machine[state as usize][symbol as usize];
        (!trans.is_undefined()).then_some(trans)
    }

    pub fn from(machine: &str) -> Machine {
        fn trans(trans: &[u8]) -> Transition {
            let [mut symbol, mut orient, mut state]: [u8; 3] = trans.try_into().unwrap();
            match symbol as char {
                'A'..='Z' => (state, symbol, orient) = (symbol, orient, state), // marxen's format B1R -> 1RB
                // '-' => (symbol, orient, state) = ('1' as u8, 'R' as u8, 'Z' as u8),
                '-' => return Transition::undefined(),
                _ => (),
            }
            Transition {
                symbol: symbol - '0' as u8,
                head: Head { orient: if orient == 'L' as u8 { 0 } else { 1 }, state: state - 'A' as u8 },
            }
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
            state.iter().try_for_each(|t| write!(f, "{}", t))
        })
    }
}
