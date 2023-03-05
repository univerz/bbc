#![feature(result_contains_err)]
#![feature(option_result_contains)]

use argh::FromArgs;
use bbc::machine::{Direction, Machine};
use color_eyre::eyre::Result;
use itertools::Itertools;
use owo_colors::OwoColorize;
use std::cmp;
use std::collections::VecDeque;
use std::fmt;
use std::fs::File;
use std::io::{self, BufRead, BufReader, Write};
use std::str::FromStr;
use termion::{event::Key, input::TermRead, raw::IntoRawMode, screen::IntoAlternateScreen};

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Err {
    Halt,
    StepLimit,
    Interesting,
    UnknownTransition,
    Unreachable,
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum Item {
    /// left: `011 01`, right: `110 10`
    D,
    /// 011
    P,
    /// 0: `011 0111 011`, 1: `011 0 01`, 2: `01111 011`, 3: `01 01`
    C(u8),
    /// `(011 011)^n`
    X(usize),
    /// 1-run-length encoding; `L(2332)` == `011 0111 0111 011`
    L(u16),
    /// 0/a: `2 x^7640 D x^10344 ``
    /// 1/b: `D x^72142 D x^3076 D x^1538 D x^300 D x^30826`
    /// 2/c: `1D x^72141 1D x^3075 1D x^1537 1D x^299 1D x^30825`
    E {
        block: u8,
        exp: usize,
    },
    Unreachable,
}

type Tape = Vec<Item>;

type RefBlocks<'a> = &'a [&'a [Item]];

#[derive(Clone, Debug)]
struct Configuration {
    ltape: Tape,
    rtape: Tape,
    /// `<C 10` | `A>`
    dir: Direction,
    sim_step: usize,

    // Stats
    num_uni_cycles: usize,
    num_strides: usize,
}

// >  xCC      ->  {2332}    >
//   {2332} <  ->  {2301}  x >
//   {2301} <  ->  {252}     >
//   {252}  <  ->    PDx     >
//
// x C1 C  <   ->  x {432}   >
// x {432} <   ->  x {401} x >
// x {401} <   ->  x {62}    >
// x {62}  <   ->  x {31}  x >
// x {31}  <   ->    P C1 D  >

impl Configuration {
    pub fn new() -> Configuration {
        // $ cargo run --release --bin on2 5 0
        // ...
        // `28:  11001 <C 1011` == `C1 < P`

        Configuration {
            ltape: vec![Item::C(1)],
            rtape: vec![Item::P],
            dir: Direction::Right,
            sim_step: 0,
            num_uni_cycles: 0,
            num_strides: 0,
        }
    }

    fn naive_accel_idxs(&self) -> Option<Vec<usize>> {
        if self.dir != Direction::Right {
            return None;
        }
        if !self.rtape.first().contains(&&Item::P) || self.rtape.last().contains(&&Item::C(3)) {
            return None; // disable `3` on last position (so all 3s are in a valid position in the middle of a window)
        }
        if !self
            .rtape
            .iter()
            .skip(1)
            .all(|item| matches!(item, Item::D | Item::X(_) | Item::C(3) | Item::E { block: 1, exp: _ }))
        {
            return None;
        }

        let mut min_exp = 1;
        let to_exp_idxs: Option<Vec<usize>> = self
            .rtape
            .as_slice()
            .windows(3)
            .enumerate()
            .rev()
            .filter_map(|(idx, w)| match w {
                [Item::X(_), Item::C(3), Item::X(exp_from)] => {
                    if *exp_from > min_exp {
                        min_exp = min_exp.checked_shl(2)?;
                        Some(Some(idx))
                    } else {
                        Some(None)
                    }
                }
                [_, Item::C(3), _] => Some(None), // if there is a `3` then it should be in a valid position
                _ => None,
            })
            .collect();
        if min_exp == 1 { None } else { to_exp_idxs }
    }

    fn accelerate(&mut self) -> bool {
        // return false;

        let to_exp_idxs = self.naive_accel_idxs();

        if let Some(idxs) = to_exp_idxs {
            let mut move_exp_value = 1;
            for idx in idxs {
                if let Some(Item::X(exp)) = self.rtape.get_mut(idx) {
                    *exp = exp.checked_add(move_exp_value << 1).unwrap();
                } else {
                    unreachable!()
                }
                if let Some(Item::X(exp)) = self.rtape.get_mut(idx + 2) {
                    *exp -= move_exp_value
                } else {
                    unreachable!()
                }
                move_exp_value = move_exp_value << 2;
            }
            self.dir = Direction::Left;

            self.num_strides += 1;
            true
        } else {
            false
        }
    }

    // Count number of "strides" (applications of accelerate()) we could perform before a
    // collision (considering only the right half of the tape).
    fn count_accel_strides(&self) -> usize {
        if !self
            .rtape
            .iter()
            .skip(1)
            .all(|item| matches!(item, Item::D | Item::X(_) | Item::C(3) | Item::E { block: 1, exp: _ }))
        {
            return 0;
        }

        let mut min_exp = 1;
        let max_reps: Option<usize> = self
            .rtape
            .as_slice()
            .windows(3)
            .rev()
            .filter_map(|w| match w {
                [Item::X(_), Item::C(3), Item::X(exp_from)] => {
                    let this_reps = *exp_from / min_exp;
                    min_exp = min_exp.checked_shl(2)?;
                    Some(this_reps)
                }
                [_, Item::C(3), _] => Some(0), // if there is a `3` then it should be in a valid position
                _ => None,
            })
            .min();
        if let Some(reps) = max_reps {
            return reps;
        } else {
            // TODO: Maybe this should actually be usize::MAX? Like if there are no Cs on the right, there's no limit, each stride does nothing to right. Correct?
            return 0;
        }
    }

    // Apply multiple "strides" (applications of accelerate()) only to right hand side of tape.
    // Used for @uni-cycle acceleration.
    fn apply_multiple_strides(&mut self, num_strides: usize, idxs: Vec<usize>) {
        let mut move_exp_value = num_strides;
        for idx in idxs {
            if let Some(Item::X(exp)) = self.rtape.get_mut(idx) {
                *exp = exp.checked_add(move_exp_value << 1).unwrap();
            } else {
                unreachable!()
            }
            if let Some(Item::X(exp)) = self.rtape.get_mut(idx + 2) {
                *exp -= move_exp_value
            } else {
                unreachable!()
            }
            move_exp_value = move_exp_value << 2;
        }
    }

    // Attempt to apply the @uni-cycle
    fn try_uni_cycle(&mut self) -> bool {
        match (self.dir, self.ltape.as_slice(), self.rtape.as_slice()) {
            // Example config:
            //   84719:  ! a^1 1 x^7640 D x^10345 3 x^7639 D x^10347 3 x^7635 D x^10355 1 x^7618 D x^10389 2 x^7550 D x^10524 0 x^7279 D x^11066 3 x^6197 D x^13231 1 x^1866 DD x^7713 0 x^95 2D x^598586766 1D >  x^300 D x^30826  b^8 D x^42804942 D x^3076 D x^1538 D x^300 D x^21397226 D x^13012670 D x^2139716 D x^1069858 D x^213964 D x^21621178 D x^3440996 D x^1720498 D x^344092 D x^1414318 D x^223068 D x^211854560 3 x^673806909 P
            #[rustfmt::skip]
            (Direction::Right,
                [.., Item::E { block: 0, .. },
                    Item::C(1), Item::X(7640), Item::D, Item::X(10345),
                    Item::C(3), Item::X(7639), Item::D, Item::X(10347),
                    Item::C(3), Item::X(7635), Item::D, Item::X(10355),
                    Item::C(1), Item::X(7618), Item::D, Item::X(10389),
                    Item::C(2), Item::X(7550), Item::D, Item::X(10524),
                    Item::C(0), Item::X(7279), Item::D, Item::X(11066),
                    Item::C(3), Item::X(6197), Item::D, Item::X(13231),
                    Item::C(1), Item::X(1866), Item::D, Item::D, Item::X(7713),
                    Item::C(0), Item::X(95),
                    Item::C(2), Item::D, Item::X(big_count),
                    Item::C(1), Item::D],
                [.., Item::E { block: 1, .. },
                    Item::X(30826), Item::D, Item::X(300)]) => {
                // Each cycle reduces big_count by UNI_CYCLE_REDUCE and strides UNI_CYCLE_STRIDE times.
                const UNI_CYCLE_REDUCE : usize = 53946;
                const UNI_CYCLE_STRIDE : usize = 53946 * 4 - 5;
                // max cycles before big_count is too small.
                let max_cycles_left = *big_count / UNI_CYCLE_REDUCE;
                // max_strides is max times we can stride before there is a crash on right side.
                let max_strides = self.count_accel_strides();
                let max_cycles_right = max_strides / UNI_CYCLE_STRIDE;
                // We cycle until one of the two above is imminent.
                let num_cycles = cmp::min(max_cycles_left, max_cycles_right);
                // println!("max_cycles_left: {:?}  /  max_cycles_right: {:?}  /  num_cycles: {:?}", max_cycles_left, max_cycles_right, num_cycles);
                if num_cycles > 0 {
                    // Apply updates to left half of tape.
                    // Note: we must do a second, mutable match to satisfy the borrowing logic.
                    match (self.ltape.as_mut_slice(), self.rtape.as_mut_slice()) {
                        ([.., Item::E { block: 0, exp: a_count },
                            Item::C(1), Item::X(7640), Item::D, Item::X(10345),
                            Item::C(3), Item::X(7639), Item::D, Item::X(10347),
                            Item::C(3), Item::X(7635), Item::D, Item::X(10355),
                            Item::C(1), Item::X(7618), Item::D, Item::X(10389),
                            Item::C(2), Item::X(7550), Item::D, Item::X(10524),
                            Item::C(0), Item::X(7279), Item::D, Item::X(11066),
                            Item::C(3), Item::X(6197), Item::D, Item::X(13231),
                            Item::C(1), Item::X(1866), Item::D, Item::D, Item::X(7713),
                            Item::C(0), Item::X(95),
                            Item::C(2), Item::D, Item::X(big_count),
                            Item::C(1), Item::D],
                        [.., Item::E { block: 1, exp: b_count },
                            Item::X(30826), Item::D, Item::X(300)]) => {
                            *big_count -= num_cycles * UNI_CYCLE_REDUCE;
                            *a_count += num_cycles;
                            *b_count += num_cycles;
                        }
                        _ => unreachable!()
                    }
                    // Apply updates to right half of tape.
                    let to_exp_idxs : Vec<usize> = self.naive_accel_idxs().unwrap();
                    self.apply_multiple_strides(num_cycles * UNI_CYCLE_STRIDE, to_exp_idxs);
                    self.num_uni_cycles += 1;
                    return true;
                }
                return false;
            }
            _ => {
                // We are not in a @uni-cycle.
                return false;
            }
        }
    }

    fn step(&mut self) -> Result<(), Err> {
        #[inline(always)]
        fn push_or_merge_x(tape: &mut Tape, new_exp: usize) {
            if let Some(Item::X(exp)) = tape.last_mut() {
                *exp = exp.checked_add(new_exp).unwrap();
            } else {
                tape.push(Item::X(new_exp));
            }
        }
        #[inline(always)]
        fn pop_n(tape: &mut Tape, n: usize) {
            tape.truncate(tape.len() - n)
        }
        macro_rules! pop_x_truncate {
            ($tape:tt, $exp:tt) => {
                *$exp -= 1;
                if *$exp == 0 {
                    self.$tape.pop();
                }
            };
            ($tape:tt, $exp:tt, $extra:tt) => {
                *$exp -= 1;
                let remove = if *$exp == 0 { $extra + 1 } else { $extra };
                pop_n(&mut self.$tape, remove);
            };
        }

        use Direction::*;
        match (self.dir, self.ltape.as_mut_slice(), self.rtape.as_mut_slice()) {
            // NEW `end < 3x` -> `1 > DP` // $ cargo run --release --bin on2 4 0 --conf "! 00 <C 10 1010 110 110 !" ... 15:   ! a^1 001 A> a^1 10110 !
            (Left, [], [.., Item::X(exp), Item::C(3)]) => {
                pop_x_truncate!(rtape, exp, 1);
                self.rtape.push(Item::P);
                self.rtape.push(Item::D);
                self.ltape.push(Item::C(1));
                self.dir = Right;
            }
            // NEW `x > end` -> ` < 3xP` // $  cargo run --release --bin on2 8 0 --conf "! 011011 A> 00000000 !"
            (Right, [.., Item::X(exp)], []) => {
                pop_x_truncate!(ltape, exp);
                self.rtape.push(Item::P);
                self.rtape.push(Item::X(1));
                self.rtape.push(Item::C(3));
                self.dir = Left;
            }
            // NEW `D > end` -> `< x` // $ cargo run --release --bin on2 4 0 --conf "! 011 01 A> 000 !" ... 6:   <C 10 a^2 !
            (Right, [.., Item::D], []) => {
                self.ltape.pop();
                self.rtape.push(Item::X(1)); // || test Item::P + Item::P
                self.dir = Left;
            }

            // `> D33` -> `P0 >` // $ cargo run --release --bin on2 8 0 --conf "! A> 11010 1010 1010 !"
            (Right, _, [.., Item::C(3), Item::C(3), Item::D]) => {
                pop_n(&mut self.rtape, 3);
                self.ltape.push(Item::P);
                self.ltape.push(Item::C(0));
            }
            // `> D3` -> `xP >`
            (Right, _, [.., Item::C(3), Item::D]) => {
                pop_n(&mut self.rtape, 2);
                push_or_merge_x(&mut self.ltape, 1);
                self.ltape.push(Item::P);
            }

            // `x < ` -> `< x` // x now needs merge in this direction because of acceleration
            (Left, [.., Item::X(exp)], _) => {
                push_or_merge_x(&mut self.rtape, *exp);
                self.ltape.pop();
            }
            // `(D | P | 3) < ` -> `< (D | P | 3)`
            (Left, [.., Item::D | Item::P | Item::C(3)], _) => {
                let item = self.ltape.pop().unwrap();
                self.rtape.push(item);

                use Item::*;
                // if self.ltape.ends_with(&[D, X(72142), D, X(3076), D, X(1538), D, X(300), D, X(30826)]) {
                if self.rtape.ends_with(&[X(30826), D, X(300), D, X(1538), D, X(3076), D, X(72142), D]) {
                    pop_n(&mut self.rtape, 10);
                    if let Some(Item::E { block: 1, exp }) = self.rtape.last_mut() {
                        *exp = exp.checked_add(1).unwrap();
                    } else {
                        self.rtape.push(Item::E { block: 1, exp: 1 })
                    }
                    // return Err(Err::Interesting);
                }
            }
            // needs to be after previous case (c != 3)
            // `0 <` -> `1x >` | `1 < ` -> `2 >` | `2 <` -> `3x >`
            (Left, [.., Item::C(c)], _) => {
                *c += 1;
                if *c != 2 {
                    self.ltape.push(Item::X(1));
                }
                self.dir = Right;
            }

            // CHANGED `x > 3` -> `0 >` // from `> x^n 3` -> `x^(n-1) 0 >`
            (Right, [.., Item::X(exp)], [.., Item::C(3)]) => {
                let test_a = *exp == 10345; // --conf "2 x^7640 D x^10344 2 x^7640 D x^10344 1 x^7640 D x^10345 3 x^7639 D x^10347 3 < ! "
                pop_x_truncate!(ltape, exp);
                if test_a && self.ltape.ends_with(&[Item::C(2), Item::X(7640), Item::D, Item::X(10344)]) {
                    pop_n(&mut self.ltape, 4);
                    if let Some(Item::E { block: 0, exp }) = self.ltape.last_mut() {
                        *exp = exp.checked_add(1).unwrap();
                    } else {
                        self.ltape.push(Item::E { block: 0, exp: 1 });
                    }
                }
                self.ltape.push(Item::C(0));
                self.rtape.pop();
            }

            // `0 > 3` (== `> x33`) -> `L(2332) >` // $ cargo run --release --bin on2 8 0 --conf "! 011 0111 011 A> 1010 !"
            (Right, [.., Item::C(0)], [.., Item::C(3)]) => {
                self.ltape.pop();
                self.ltape.push(Item::L(2332));
                self.rtape.pop();
            }
            // `L(2332) <` -> `L(2301) x >` // $ cargo run --release --bin on2 8 0 --conf "! 01101110111 a^1 <C 10 !"
            (Left, [.., Item::L(2332)], _) => {
                self.ltape.pop();
                self.ltape.push(Item::L(2301));
                self.ltape.push(Item::X(1));
                self.dir = Right;
            }
            // `L(2301) <` -> `L(252) >` // $ cargo run --release --bin on2 8 0 --conf "! 0110111001 <C 10 !"
            (Left, [.., Item::L(2301)], _) => {
                self.ltape.pop();
                self.ltape.push(Item::L(252));
                self.dir = Right;
            }
            // `L(252) <` -> `PDx >` // $ cargo run --release --bin on2 8 0 --conf "! 011011111 a^1  <C 10 !"
            (Left, [.., Item::L(252)], _) => {
                self.ltape.pop();
                self.ltape.push(Item::P);
                self.ltape.push(Item::D);
                self.ltape.push(Item::X(1));
                self.dir = Right;
            }
            // `> PD3x` -> `L(2301) D > P` // $ cargo run --release --bin on2 5 0 --conf "! A> 110 11010 1010 110110 !" ... 31:   !  a^2 1001 a^1 01 A> 110 !
            (Right, _, [.., Item::X(exp), Item::C(3), Item::D, Item::P]) => {
                pop_x_truncate!(rtape, exp, 3);
                self.rtape.push(Item::P);
                self.ltape.push(Item::L(2301));
                self.ltape.push(Item::D);
            }
            // `> PDDx` -> `21D > ` // $ cargo run --release --bin on2 8 0 --conf "! A> 110 11010 11010 110110 !" ... 63:   !  a^1 11 a^2 001 a^1 01 A>  !
            (Right, _, [.., Item::X(exp), Item::D, Item::D, Item::P]) => {
                pop_x_truncate!(rtape, exp, 3);
                self.ltape.push(Item::C(2));
                self.ltape.push(Item::C(1));
                self.ltape.push(Item::D);
            }
            // `2 > 3` (== `13 <`) -> `L(432) >` // $ cargo run --release --bin on2 8 0 --conf "! 011 0111 011 A> 1010 !"
            (Right, [.., Item::C(2)], [.., Item::C(3)]) => {
                self.ltape.pop();
                self.ltape.push(Item::L(432));
                self.rtape.pop();
            }
            // `L(432) <` -> `L(401) x >` // $ cargo run --release --bin on2 8 0 --conf "! 011110111 a^1 <C 10 !"
            (Left, [.., Item::L(432)], _) => {
                self.ltape.pop();
                self.ltape.push(Item::L(401));
                self.ltape.push(Item::X(1));
                self.dir = Right;
            }
            // `L(401) <` -> `L(62) >` // $ cargo run --release --bin on2 8 0 --conf "! 01111001 <C 10 !"
            (Left, [.., Item::L(401)], _) => {
                self.ltape.pop();
                self.ltape.push(Item::L(62));
                self.dir = Right;
            }
            // `L(62) <` -> `L(31) x >` // $ cargo run --release --bin on2 8 0 --conf "! 0111111 a^1 <C 10 !"
            (Left, [.., Item::L(62)], _) => {
                self.ltape.pop();
                self.ltape.push(Item::L(31));
                self.ltape.push(Item::X(1));
                self.dir = Right;
            }
            // `x L(31) <` -> `P1D >` // $ cargo run --release --bin on2 8 0 --conf "! 011011 011101 <C 10 !"
            (Left, [.., Item::X(exp), Item::L(31)], _) => {
                pop_x_truncate!(ltape, exp, 1);
                self.ltape.push(Item::P);
                self.ltape.push(Item::C(1));
                self.ltape.push(Item::D);
                self.dir = Right;
            }

            // `> P x^n` -> `x^n > P`
            (Right, _, [.., Item::X(exp), Item::P]) => {
                push_or_merge_x(&mut self.ltape, *exp);
                pop_n(&mut self.rtape, 2);
                self.rtape.push(Item::P)
            }
            // `> PDP` -> `1D >`
            (Right, _, [.., Item::P, Item::D, Item::P]) => {
                pop_n(&mut self.rtape, 3);
                self.ltape.push(Item::C(1));
                self.ltape.push(Item::D)
            }
            // `> PDx` -> `1D > P`
            (Right, _, [.., Item::X(exp), Item::D, Item::P]) => {
                pop_x_truncate!(rtape, exp, 2);
                self.rtape.push(Item::P);
                self.ltape.push(Item::C(1));
                self.ltape.push(Item::D);
            }
            // `> P3x` -> `< PDP`
            (Right, _, [.., Item::X(exp), Item::C(3), Item::P]) => {
                pop_x_truncate!(rtape, exp, 2);
                self.rtape.push(Item::P);
                self.rtape.push(Item::D);
                self.rtape.push(Item::P);
                self.dir = Left;
            }
            // `> P end` -> `< P`
            (Right, _, [Item::P]) => {
                self.dir = Left;
            }
            // CHANGED `> PP` -> `x >` // from `> PP end`
            (Right, _, [.., Item::P, Item::P]) => {
                pop_n(&mut self.rtape, 2);
                push_or_merge_x(&mut self.ltape, 1);
            }
            // `> D` -> `D >`
            (Right, _, [.., Item::D]) => {
                self.rtape.pop();
                self.ltape.push(Item::D);
            }
            // `> x` -> `x >`
            (Right, _, [.., Item::X(exp)]) => {
                // compression here no longer works with acceleration
                // let test_b = *exp == 30826; // --conf "2 > D x^598979953 PDP x^72142 D x^3076 D x^1538 D x^300 D x^30826 D x^42804942 D x^213427271 3 x^670661487 P"
                push_or_merge_x(&mut self.ltape, *exp);
                // use Item::*;
                // if test_b && self.ltape.ends_with(&[D, X(72142), D, X(3076), D, X(1538), D, X(300), D, X(30826)]) {
                //     pop_n(&mut self.ltape, 10);
                //     if let Some(Item::E { block: 1, exp }) = self.ltape.last_mut() {
                //         *exp = exp.checked_add(1).unwrap();
                //     } else {
                //         self.ltape.push(Item::E { block: 1, exp: 1 })
                //     }
                //     // return Err(Err::Interesting);
                // }
                self.rtape.pop();
            }
            // `> b` -> `b >`
            (Right, _, [.., Item::E { block: 1, exp: move_exp }]) => {
                if let Some(Item::E { block: 1, exp }) = self.ltape.last_mut() {
                    *exp = exp.checked_add(*move_exp).unwrap();
                } else {
                    self.ltape.push(Item::E { block: 1, exp: *move_exp })
                }
                self.rtape.pop();
            }
            // `b < ` -> `< b`
            (Left, [.., Item::E { block: 1, exp: move_exp }], _) => {
                self.rtape.push(Item::E { block: 1, exp: *move_exp });
                self.ltape.pop();
            }
            // `c^n < ` -> `c^(n-1) expanded-c <`
            (Left, [.., Item::E { block: 2, exp }], _) => {
                *exp -= 1;
                if *exp == 0 {
                    self.ltape.pop();
                }
                use Item::*;
                let e = [C(1), D, X(72141), C(1), D, X(3075), C(1), D, X(1537), C(1), D, X(299), C(1), D, X(30825)];
                self.ltape.extend_from_slice(&e); // NUDO: use extend() if Items gets bigger / allocates
            }
            (Left, [.., Item::Unreachable], _) | (Right, _, [.., Item::Unreachable]) => {
                return Err(Err::Unreachable);
            }
            // `> P b` -> c > P // --conf "! > P   D x^72142 D x^3076 D x^1538 D x^300 D x^30826   D !"
            (Right, _, [.., Item::E { block: 1, exp: move_exp }, Item::P]) => {
                // -> "! 1D x^72141 1D x^3075 1D x^1537 1D x^299 1D x^30825  > P !"
                self.ltape.push(Item::E { block: 2, exp: *move_exp });
                pop_n(&mut self.rtape, 2);
                self.rtape.push(Item::P);
            }

            _ => return Err(Err::UnknownTransition),
        }
        Ok(())
    }

    fn gen_step(&mut self, cfg: Config) -> Result<(), Err> {
        if cfg.accel_uni_cycles {
            if self.try_uni_cycle() {
                return Ok(());
            }
        }
        if self.accelerate() {
            return Ok(());
        }
        self.step()
    }

    fn run(&mut self, _machine: &Machine, _blocks: RefBlocks, cfg: Config) -> Result<(), Err> {
        while self.sim_step < cfg.sim_step_limit {
            self.gen_step(cfg)?;
            self.sim_step += 1;
            if self.sim_step & ((1 << cfg.print_mod) - 1) == 0 {
                println!("{self}");
            }
        }
        return Err(Err::StepLimit);
    }
}

const DSP_ROTATE_TAPE: bool = false;
const DSP_HIDE_X: bool = false;
// const DSP_ROTATE_TAPE: bool = true;
// const DSP_HIDE_X: bool = true;

impl fmt::Display for Item {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Item::D => write!(f, "D"),
            Item::P => write!(f, "P"),
            Item::C(s) => write!(f, "{}", s.italic().bold()),
            Item::X(exp) => {
                if DSP_HIDE_X {
                    return Ok(());
                }
                if exp > 1_000_000_000 { write!(f, " x^{} ", exp.bright_white()) } else { write!(f, " x^{} ", exp) }
            }
            Item::L(r) => write!(f, " L({r}) "),
            Item::E { block, exp } => write!(f, " {}^{} ", ((block + b'a') as char).yellow().bold(), exp),
            Item::Unreachable => write!(f, " {} ", '!'.bright_red()),
        }
    }
}

impl fmt::Display for Configuration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} ", self.sim_step.bright_white())?;
        write!(f, "({}, {}):  ", self.num_strides, self.num_uni_cycles)?;
        if DSP_ROTATE_TAPE {
            self.rtape.iter().try_for_each(|item| write!(f, "{item}"))?;
            write!(f, " {} ", if self.dir == Direction::Left { '<' } else { '>' }.bright_green().bold())?;
            self.ltape.iter().rev().try_for_each(|item| write!(f, "{item}"))
        } else {
            self.ltape.iter().try_for_each(|item| write!(f, "{item}"))?;
            write!(f, " {} ", if self.dir == Direction::Left { '<' } else { '>' }.bright_green().bold())?;
            self.rtape.iter().rev().try_for_each(|item| write!(f, "{item}"))
        }
    }
}

fn raw_parse(s: &str) -> Result<(Configuration, Direction)> {
    let mut conf = Configuration {
        ltape: Tape::new(),
        rtape: Tape::new(),
        dir: Direction::Right,
        sim_step: 0,
        num_strides: 0,
        num_uni_cycles: 0,
    };
    let mut active_tape_dir = Direction::Left;
    let mut tape = &mut conf.ltape;

    for token in s.split_whitespace() {
        if token.ends_with(":") {
            conf.sim_step = token[0..token.len() - 1].parse().unwrap();
            continue;
        }
        if token == "<" {
            assert_eq!(active_tape_dir, Direction::Left);
            active_tape_dir = Direction::Right;
            tape = &mut conf.rtape;

            conf.dir = Direction::Left;
            continue;
        }
        if token == ">" {
            assert_eq!(active_tape_dir, Direction::Left);
            active_tape_dir = Direction::Right;
            tape = &mut conf.rtape;

            conf.dir = Direction::Right;
            continue;
        }
        if let Some((block, exp)) = token.split_once("^") {
            if block == "x" {
                tape.push(Item::X(exp.parse()?));
            } else {
                tape.push(Item::E { block: block.chars().next().unwrap() as u8 - b'a', exp: exp.parse()? });
            }
            continue;
        }
        if let Some(encoded) = token.strip_prefix("L(") {
            tape.push(Item::L(encoded[..(encoded.len() - 1)].parse()?));
            continue;
        }
        tape.extend(token.chars().map(|symbol| match symbol {
            'D' => Item::D,
            'P' => Item::P,
            '0'..='9' => Item::C(symbol as u8 - b'0'),
            'x' => Item::X(1),
            '!' => Item::Unreachable,
            _ => unreachable!(),
        }));
    }
    Ok((conf, active_tape_dir))
}

// fn parse(s: &str) -> Result<Tape> {
//     let (conf, dir) = raw_parse(s)?;
//     assert_eq!(dir, Direction::Left);

//     Ok(conf.ltape)
// }

impl FromStr for Configuration {
    type Err = color_eyre::Report;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let (mut conf, dir) = raw_parse(s)?;
        assert_eq!(dir, Direction::Right);
        conf.rtape.reverse();

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

    #[argh(switch, short = 'x', description = "interpret step limit and print limit as 2^n?")]
    exponential_steps: bool,
    #[argh(switch, short = 'c', description = "accelerate @uni-cycles")]
    accel_uni_cycles: bool,
}

#[derive(Clone, Copy, Debug)]
struct Config {
    sim_step_limit: usize,
    print_mod: u8,
    accel_uni_cycles: bool,
}

// new run:               cargo run --release --bin no1 60 30
// explore configuration: cargo run --release --bin no1 8 0 --conf "2 x^7640 D x^10344 2 x^7640 D x^10344 1 x^7640 D x^10345 3 x^7639 D x^10347 3 < ! "
// explore in tui:        cargo run --release --bin no1 8 0 --conf "1 > P" --tui
fn main() -> Result<()> {
    color_eyre::install()?;

    // return transcode();

    let machine = Machine::from("1RB1RD_1LC0RC_1RA1LD_0RE0LB_---1RC");
    let args: Args = argh::from_env();
    let sim_step_limit = if args.exponential_steps {
        2usize.checked_pow(args.sim_step_limit).unwrap()
    } else {
        args.sim_step_limit.try_into().unwrap()
    };
    let cfg =
        Config { sim_step_limit: sim_step_limit, print_mod: args.print_mod, accel_uni_cycles: args.accel_uni_cycles };
    let mut conf = args.conf;
    // dbg!(cfg);
    println!("{}", conf);

    let blocks = Vec::new();

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
            Key::Char('j') if state.is_ok() || state.contains_err(&Err::StepLimit) => {
                history.push_front((speed, state, conf.clone()));
                let step = 1 << speed;
                cfg.sim_step_limit = conf.sim_step + step;
                state = conf.run(machine, blocks, cfg);
                if history.len() > 1_000_000 {
                    history.pop_back();
                }
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
    let file = File::open("/home/univerz/projects/bbc/.data/no1/no2_21")?;
    let lines = BufReader::new(file).lines();

    for line in lines {
        let line = String::from_utf8(strip_ansi_escapes::strip(line?)?)?;
        // dbg!(&line);
        let conf: Configuration = line.parse()?;
        // dbg!(&conf);

        fn fmt_symbol(item: &Item) -> String {
            match *item {
                Item::X(_) => String::new(),
                // Item::X(_) => format!("."),
                Item::E { block, exp: _ } => format!("{}", ((block + b'a') as char).yellow().bold()),
                _ => format!("{item}"),
            }
        }

        let ltake = 500usize.saturating_sub(conf.rtape.len());
        let mut conf = format!(
            "{} {} {} {}",
            conf.sim_step,
            conf.rtape.iter().map(|item| fmt_symbol(item)).join(""),
            conf.dir.bright_green().bold(),
            conf.ltape.iter().rev().take(ltake).map(|item| fmt_symbol(item)).join("")
        );
        println!("{}", conf);

        // print!("{}:  ", conf.sim_step);
        // conf.ltape.iter().for_each(|item| fmt_symbol(item));
        // print!(" {} ", conf.dir.bright_green().bold());
        // conf.rtape.iter().rev().for_each(|item| fmt_symbol(item));
        // println!();
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    // #[test]
    // fn parse_block() -> Result<()> {
    //     let mut b = parse("! x23 x^7640 DP x^10344 L(69)")?;
    //     b.reverse();
    //     assert_eq!(b, parse("L(69) x^10344 PD  x^7640 32x!")?);
    //     Ok(())
    // }

    #[test]
    fn parse_conf() -> Result<()> {
        for inp in ["0:  2 x^3 P a^4 DD x^167 31 x^17 L(432)  >  3 x^70 P", "0: 0 < 1 !"] {
            let conf: Configuration = inp.parse()?;
            assert_eq!(
                inp.split_whitespace().collect::<String>(),
                String::from_utf8(strip_ansi_escapes::strip(conf.to_string())?)?.split_whitespace().collect::<String>()
            );
        }
        Ok(())
    }
}
