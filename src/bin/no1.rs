#![feature(result_contains_err)]
#![feature(option_result_contains)]

use argh::FromArgs;
use bbc::machine::Direction;
use color_eyre::eyre::Result;
use derivative::Derivative;
use itertools::Itertools;
use owo_colors::OwoColorize;
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

type Exp = u128;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Item {
    /// left: `011 01`, right: `110 10`
    D,
    /// 011
    P,
    /// 0: `011 0111 011`, 1: `011 0 01`, 2: `01111 011`, 3: `01 01`
    C(u8),
    /// `(011 011)^n`
    X(Exp),
    /// 1-run-length encoding; `L(2332)` == `011 0111 0111 011`
    L(u16),
    /// 0/a: `2 x^7640 D x^10344 ``
    /// 1/b: `D x^72142 D x^3076 D x^1538 D x^300 D x^30826`
    /// 2/c: `1D x^72141 1D x^3075 1D x^1537 1D x^299 1D x^30825`
    E {
        block: u8,
        exp: Exp,
    },
    Unreachable,
}

type Tape = Vec<Item>;

mod block {
    use super::Item::{self, *};
    pub const LEFT_A: [Item; 4] = [C(2), X(7640), D, X(10344)];
    pub const LEFT_B: [Item; 10] = [D, X(72142), D, X(3076), D, X(1538), D, X(300), D, X(30826)];
    pub const RIGHT_B: [Item; 10] = [X(30826), D, X(300), D, X(1538), D, X(3076), D, X(72142), D];
    pub const LEFT_C: [Item; 15] =
        [C(1), D, X(72141), C(1), D, X(3075), C(1), D, X(1537), C(1), D, X(299), C(1), D, X(30825)];
}

#[derive(Clone, Debug, Default)]
struct SimStats {
    // Number of times an "increment" of counter config happens.
    // Precisely this counts the number of times that `> R -> < R` occurs (including extrapolating
    // number of times that transition would have occurred during counter acceleration (strides)).
    num_counter_increments: Exp,
    // **Incomplete** Number of base TM steps ... currently does not include steps from any accelerations.
    num_tm_steps: Exp,
    // Number of times accelerate() is applied (Counter acceleration).
    num_strides: u64,
    // Number of times try_uni_cycle() is applied (@uni-cycle accleration).
    num_uni_cycles: u64,
    // Number of times a new `a^1` block is created.
    num_a_create: u64,
    // Number of times a new `c^n` block is created (from `>P b^n -> c^n >P`).
    num_c_create: u64,

    // Collisions
    // Number of collisions that have occurred (only non-@uni-cycle ones for now).
    num_collisions: Exp,
    last_collision_incr: Exp,
    // log2(collision_time + 1)
    log2_collision_times_hist: [Exp; 20],
}

#[derive(Derivative)]
#[derivative(PartialEq)]
#[derive(Clone, Debug)]
struct Configuration {
    ltape: Tape,
    rtape: Tape,
    /// `<C 10` | `A>`
    dir: Direction,
    sim_step: usize,

    #[derivative(PartialEq = "ignore")]
    stats: SimStats,
}

impl SimStats {
    fn record_collision(&mut self) {
        self.num_collisions += 1;

        let collision_time = self.num_counter_increments - self.last_collision_incr;
        let log_collision_time = (collision_time + 1).ilog2();
        let index: usize = log_collision_time.clamp(0, 19).try_into().unwrap();
        self.log2_collision_times_hist[index] += 1;

        self.last_collision_incr = self.num_counter_increments;
    }
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

struct CounterAccel {
    max_apply: Exp,
    idxs: Vec<usize>,
}

impl CounterAccel {
    fn new(rtape: &[Item]) -> Option<CounterAccel> {
        if !rtape.first().contains(&&Item::P) {
            return None;
        }

        #[derive(PartialEq)]
        enum Prev {
            X,
            C,
            Other,
        }
        let mut prev = Prev::Other;
        let mut min_exp = 1;
        let mut from_exp: Exp = 0;

        // `x C x` is the only valid sequence that contains C; DCxb are allowed
        rtape.iter().enumerate().skip(1).rev().try_fold(
            CounterAccel { max_apply: Exp::MAX, idxs: Vec::new() },
            |mut acc, (idx, item)| {
                prev = match item {
                    Item::C(_) if prev == Prev::X => Prev::C,
                    Item::X(exp) => {
                        if prev == Prev::C {
                            // x(from_exp) C x(exp)
                            // Note: We don't want to take exp_from -> 0, so (exp_from - 1) / min_exp
                            acc.max_apply = acc.max_apply.min((from_exp.saturating_sub(1)) / min_exp);
                            if acc.max_apply == 0 {
                                return None;
                            }
                            min_exp = min_exp.checked_shl(2).unwrap();
                            acc.idxs.push(idx);
                        }
                        from_exp = *exp;
                        Prev::X
                    }
                    Item::D | Item::E { block: 1, exp: _ } if prev != Prev::C => Prev::Other,
                    _ => return None,
                };
                Some(acc)
            },
        )
    }

    fn apply(&self, num_strides: Exp, rtape: &mut Tape, stats: &mut SimStats) {
        assert!(self.max_apply >= num_strides);
        let mut move_exp_value = num_strides;
        self.idxs.iter().for_each(|&idx| {
            if let Some(Item::X(exp)) = rtape.get_mut(idx) {
                *exp = exp.checked_add(move_exp_value.checked_shl(1).unwrap()).unwrap();
            } else {
                unreachable!()
            }
            if let Some(Item::X(exp)) = rtape.get_mut(idx + 2) {
                *exp -= move_exp_value
            } else {
                unreachable!()
            }
            move_exp_value = move_exp_value.checked_shl(2).unwrap();
        });
        // There are 4 counter increments for every time the final C moves left.
        stats.num_counter_increments = stats.num_counter_increments.checked_add(move_exp_value).unwrap();
    }
}

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
            stats: Default::default(),
        }
    }

    // Attempt to apply the @uni-cycle https://www.sligocki.com/2023/02/25/skelet-1-wip.html
    fn try_uni_cycle(&mut self, counter: &CounterAccel) -> bool {
        match (self.ltape.as_mut_slice(), self.rtape.as_mut_slice()) {
            // Example config:
            //   84719:  ! a^1 1 x^7640 D x^10345 3 x^7639 D x^10347 3 x^7635 D x^10355 1 x^7618 D x^10389 2 x^7550 D x^10524 0 x^7279 D x^11066 3 x^6197 D x^13231 1 x^1866 DD x^7713 0 x^95 2D x^598586766 1D >  x^300 D x^30826  b^8 D x^42804942 D x^3076 D x^1538 D x^300 D x^21397226 D x^13012670 D x^2139716 D x^1069858 D x^213964 D x^21621178 D x^3440996 D x^1720498 D x^344092 D x^1414318 D x^223068 D x^211854560 3 x^673806909 P
            #[rustfmt::skip]
            (
                [.., Item::E { block: 0, exp: a_count },
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
                [.., Item::E { block: 1, exp: b_count }, Item::X(30826), Item::D, Item::X(300)]
            ) => {
                // Each cycle reduces big_count by UNI_CYCLE_REDUCE and counter UNI_CYCLE_STRIDE times.
                const UNI_CYCLE_REDUCE : Exp = 53946;
                const UNI_CYCLE_STRIDE : Exp = 53946 * 4 - 5;
                // max cycles before big_count is too small.
                let max_cycles_left = *big_count / UNI_CYCLE_REDUCE;
                // max cycles before there is a crash on right side.
                let max_cycles_right = counter.max_apply / UNI_CYCLE_STRIDE;
                // We cycle until one of the two above is imminent.
                let num_cycles = max_cycles_left.min(max_cycles_right);
                if num_cycles > 0 {
                    // Apply updates to left half of tape.
                    *big_count -= num_cycles.checked_mul(UNI_CYCLE_REDUCE).unwrap();
                    *a_count = a_count.checked_add(num_cycles).unwrap();
                    *b_count = b_count.checked_add(num_cycles).unwrap();

                    // Apply updates to right half of tape.
                    let num_strides = num_cycles.checked_mul(UNI_CYCLE_STRIDE).unwrap();
                    counter.apply(num_strides, &mut self.rtape, &mut self.stats);

                    return true;
                }
            }
            _ => (), // We are not in a @uni-cycle.
        }
        false
    }

    fn step(&mut self) -> Result<(), Err> {
        #[inline(always)]
        fn push_or_merge_x(tape: &mut Tape, new_exp: Exp) {
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
        // transitions-test-begin
        match (self.dir, self.ltape.as_mut_slice(), self.rtape.as_mut_slice()) {
            // `end < 3x` -> `1 > DP`
            (Left, [], [.., Item::X(exp), Item::C(3)]) => {
                pop_x_truncate!(rtape, exp, 1);
                self.rtape.push(Item::P);
                self.rtape.push(Item::D);
                self.ltape.push(Item::C(1));
                self.dir = Right;
                self.stats.num_tm_steps += 15;
            }
            // `x > end` -> `< 3xP`
            (Right, [.., Item::X(exp)], []) => {
                pop_x_truncate!(ltape, exp);
                self.rtape.push(Item::P);
                self.rtape.push(Item::X(1));
                self.rtape.push(Item::C(3));
                self.dir = Left;
                self.stats.num_tm_steps += 79; // x A> $ -(79)-> <C10 C3 x R
            }
            // `D > end` -> `< x`
            (Right, [.., Item::D], []) => {
                self.ltape.pop();
                self.rtape.push(Item::X(1)); // || test Item::P + Item::P
                self.dir = Left;
                self.stats.num_tm_steps += 8;
            }

            // `> D33` -> `P0 >`
            (Right, _, [.., Item::C(3), Item::C(3), Item::D]) => {
                pop_n(&mut self.rtape, 3);
                self.ltape.push(Item::P);
                self.ltape.push(Item::C(0));
                self.stats.record_collision();
                self.stats.num_tm_steps += 15;
            }
            // `> D3` -> `xP >`
            (Right, _, [.., Item::C(3), Item::D]) => {
                pop_n(&mut self.rtape, 2);
                push_or_merge_x(&mut self.ltape, 1);
                self.ltape.push(Item::P);
                self.stats.record_collision();
                self.stats.num_tm_steps += 11; // > D3 -(7)-> D > 3 -(4)-> 011 01 1011 A>
            }

            // `x^n <` -> `< x^n`
            (Left, [.., Item::X(exp)], _) => {
                let exp_const = *exp;
                push_or_merge_x(&mut self.rtape, *exp);
                self.ltape.pop();
                self.stats.num_tm_steps += 6 * exp_const; // 011 <C10 -(3)-> <C10 110
            }
            // `D <` -> `< D`
            (Left, [.., Item::D], _) => {
                let item = self.ltape.pop().unwrap();
                self.rtape.push(item);

                if self.rtape.ends_with(&block::RIGHT_B) {
                    pop_n(&mut self.rtape, block::RIGHT_B.len());
                    if let Some(Item::E { block: 1, exp }) = self.rtape.last_mut() {
                        *exp = exp.checked_add(1).unwrap();
                    } else {
                        self.rtape.push(Item::E { block: 1, exp: 1 })
                    }
                }
                self.stats.num_tm_steps += 9; // 011 01 <C10 -(6)-> 011 <C10 10 -(3)-> <C10 110 10
            }
            // `P <` -> `< P`
            (Left, [.., Item::P], _) => {
                let item = self.ltape.pop().unwrap();
                self.rtape.push(item);
                self.stats.num_tm_steps += 3; // 011 <C10 -(3)-> <C10 110
            }
            // `3 <` -> `< 3`
            (Left, [.., Item::C(3)], _) => {
                let item = self.ltape.pop().unwrap();
                self.rtape.push(item);
                self.stats.num_tm_steps += 12; // 01 <C10 -(6)-> <C10 10
            }
            // `0 <` -> `1x >`
            (Left, [.., Item::C(c @ 0)], _) => {
                *c += 1;
                self.ltape.push(Item::X(1));
                self.dir = Right;
                self.stats.num_tm_steps += 17; // 111 011 <C10 -(3)-> 111 <C10 110 -(14)-> 01 011 011 A>
            }
            // `1 <` -> `2 >`
            (Left, [.., Item::C(c @ 1)], _) => {
                *c += 1;
                self.dir = Right;
                self.stats.num_tm_steps += 11; // 001 <C10 -(11)-> 11011 A>
            }
            // `2 <` -> `3x >`
            (Left, [.., Item::C(c @ 2)], _) => {
                *c += 1;
                self.ltape.push(Item::X(1));
                self.dir = Right;
                self.stats.num_tm_steps += 17; // 111 011 <C10 -(3)-> 111 <C10 110 -(14)-> 01 011 011 A>
            }

            // `x > 3` -> `0 >` // changed from `> x^n 3` -> `x^(n-1) 0 >`
            (Right, [.., Item::X(exp)], [.., Item::C(3)]) => {
                let test_a = *exp == 10345; // --conf "2 x^7640 D x^10344 2 x^7640 D x^10344 1 x^7640 D x^10345 3 x^7639 D x^10347 3 < ! "
                pop_x_truncate!(ltape, exp);
                if test_a && self.ltape.ends_with(&block::LEFT_A) {
                    pop_n(&mut self.ltape, block::LEFT_A.len());
                    if let Some(Item::E { block: 0, exp }) = self.ltape.last_mut() {
                        *exp = exp.checked_add(1).unwrap();
                    } else {
                        self.ltape.push(Item::E { block: 0, exp: 1 });
                        self.stats.num_a_create += 1;
                    }
                }
                self.ltape.push(Item::C(0));
                self.rtape.pop();
                self.stats.num_tm_steps += 4; // A> 1010 -(4)-> 1011 A>
            }

            // `0 > 3` (== `> x33`) -> `L(2332) >`
            (Right, [.., Item::C(0)], [.., Item::C(3)]) => {
                self.ltape.pop();
                self.ltape.push(Item::L(2332));
                self.rtape.pop();
                self.stats.record_collision();
                self.stats.num_tm_steps += 4; // A> 1010 -(4)-> 1011 A>
            }
            // `L(2332) <` -> `L(2301) x >`
            (Left, [.., Item::L(2332)], _) => {
                self.ltape.pop();
                self.ltape.push(Item::L(2301));
                self.ltape.push(Item::X(1));
                self.dir = Right;
                self.stats.num_tm_steps += 17;
            }
            // `L(2301) <` -> `L(252) >`
            (Left, [.., Item::L(2301)], _) => {
                self.ltape.pop();
                self.ltape.push(Item::L(252));
                self.dir = Right;
                self.stats.num_tm_steps += 11;
            }
            // `L(252) <` -> `PDx >`
            (Left, [.., Item::L(252)], _) => {
                self.ltape.pop();
                self.ltape.push(Item::P);
                self.ltape.push(Item::D);
                self.ltape.push(Item::X(1));
                self.dir = Right;
                self.stats.num_tm_steps += 17;
            }
            // `> PD3x` -> `L(2301) D > P`
            (Right, _, [.., Item::X(exp), Item::C(3), Item::D, Item::P]) => {
                pop_x_truncate!(rtape, exp, 3);
                self.rtape.push(Item::P);
                self.ltape.push(Item::L(2301));
                self.ltape.push(Item::D);
                self.stats.num_tm_steps += 31;
            }
            // `> PDDx` -> `21D >`
            (Right, _, [.., Item::X(exp), Item::D, Item::D, Item::P]) => {
                pop_x_truncate!(rtape, exp, 3);
                self.ltape.push(Item::C(2));
                self.ltape.push(Item::C(1));
                self.ltape.push(Item::D);
                self.stats.num_tm_steps += 77;
            }
            // `2 > 3` (== `13 <`) -> `L(432) >`
            (Right, [.., Item::C(2)], [.., Item::C(3)]) => {
                self.ltape.pop();
                self.ltape.push(Item::L(432));
                self.rtape.pop();
                self.stats.record_collision();
                self.stats.num_tm_steps += 4; // A> 1010 -(4)-> 1011 A>
            }
            // `L(432) <` -> `L(401) x >`
            (Left, [.., Item::L(432)], _) => {
                self.ltape.pop();
                self.ltape.push(Item::L(401));
                self.ltape.push(Item::X(1));
                self.dir = Right;
                self.stats.num_tm_steps += 17; // 111 011 <C10 -(3)-> 111 <C10 110 -(14)-> 01 011 011 A>
            }
            // `L(401) <` -> `L(62) >`
            (Left, [.., Item::L(401)], _) => {
                self.ltape.pop();
                self.ltape.push(Item::L(62));
                self.dir = Right;
                self.stats.num_tm_steps += 11; // 001 <C10 -(11)-> 11011 A>
            }
            // `L(62) <` -> `L(31) x >`
            (Left, [.., Item::L(62)], _) => {
                self.ltape.pop();
                self.ltape.push(Item::L(31));
                self.ltape.push(Item::X(1));
                self.dir = Right;
                self.stats.num_tm_steps += 17; // 111 011 <C10 -(3)-> 111 <C10 110 -(14)-> 01 011 011 A>
            }
            // `x L(31) <` -> `P1D >`
            (Left, [.., Item::X(exp), Item::L(31)], _) => {
                pop_x_truncate!(ltape, exp, 1);
                self.ltape.push(Item::P);
                self.ltape.push(Item::C(1));
                self.ltape.push(Item::D);
                self.dir = Right;
                self.stats.num_tm_steps += 17; // 111 01 <C10 -(6)-> 111 <C10 10 -(4)-> 01 A> 110 10 -(7)-> 01 011 01 A>
            }

            // `> P x^n` -> `x^n > P`
            (Right, _, [.., Item::X(exp), Item::P]) => {
                let exp_const = *exp;
                push_or_merge_x(&mut self.ltape, *exp);
                pop_n(&mut self.rtape, 2);
                self.rtape.push(Item::P);
                self.stats.num_tm_steps += 10 * exp_const; // A> 110110 -(10)-> 011011 A>
            }
            // `> PDP` -> `1D >`
            (Right, _, [.., Item::P, Item::D, Item::P]) => {
                pop_n(&mut self.rtape, 3);
                self.ltape.push(Item::C(1));
                self.ltape.push(Item::D);
                self.stats.num_tm_steps += 27; // A> 110 110 10 110 -(10)-> 011 011 A> 10 110 -(6)-> 011 0 111 <C10 10 -(4)-> 011 001 A> 110 10 -(7)-> 011 001 011 01 A>
            }
            // `> PDx` -> `1D > P`
            (Right, _, [.., Item::X(exp), Item::D, Item::P]) => {
                pop_x_truncate!(rtape, exp, 2);
                self.rtape.push(Item::P);
                self.ltape.push(Item::C(1));
                self.ltape.push(Item::D);
                self.stats.num_tm_steps += 27; // A> 110 110 10 110 -(10)-> 011 011 A> 10 110 -(6)-> 011 0 111 <C10 10 -(4)-> 011 001 A> 110 10 -(7)-> 011 001 011 01 A>
            }
            // `> P3x` -> `< PDP`
            (Right, _, [.., Item::X(exp), Item::C(3), Item::P]) => {
                pop_x_truncate!(rtape, exp, 2);
                self.rtape.push(Item::P);
                self.rtape.push(Item::D);
                self.rtape.push(Item::P);
                self.dir = Left;
                self.stats.num_tm_steps += 19; // A> 110 10 10 110 -(7)-> 011 01 A> 10 110 -(6)-> 011 011 <C10 10 -(6)-> <C10 110 110 10
            }
            // `> P end` -> `< P`
            (Right, _, [Item::P]) => {
                self.dir = Left;
                self.stats.num_counter_increments = self.stats.num_counter_increments.checked_add(1).unwrap();
                self.stats.num_tm_steps += 9; // A> 110 $ -(6)-> 011 <C10 $ -(3)-> <C10 110 $
            }
            // `> PP` -> `x >` // changed from `> PP end`
            (Right, _, [.., Item::P, Item::P]) => {
                pop_n(&mut self.rtape, 2);
                push_or_merge_x(&mut self.ltape, 1);
                self.stats.num_tm_steps += 10; // A> 110110 -(5)-> 011 B> 110 -(5)-> 011011 A>
            }
            // `> D` -> `D >`
            (Right, _, [.., Item::D]) => {
                self.rtape.pop();
                self.ltape.push(Item::D);
                self.stats.num_tm_steps += 7; // A> 11010 -(5)-> 011 B> 10 -(2)-> 01101 A>
            }
            // `> x^n` -> `x^n >`
            (Right, _, [.., Item::X(exp)]) => {
                let exp_const = *exp;
                push_or_merge_x(&mut self.ltape, *exp);
                self.rtape.pop();
                self.stats.num_tm_steps += 10 * exp_const; // A> 110110 -(10)-> 011011 A>
            }
            // `> b^n` -> `b^n >`
            (Right, _, [.., Item::E { block: 1, exp: move_exp }]) => {
                let move_exp_const = *move_exp;
                if let Some(Item::E { block: 1, exp }) = self.ltape.last_mut() {
                    *exp = exp.checked_add(*move_exp).unwrap();
                } else {
                    self.ltape.push(Item::E { block: 1, exp: *move_exp })
                }
                self.rtape.pop();
                // Copied from test.
                self.stats.num_tm_steps += 1_078_855 * move_exp_const;
            }
            // `b^n <` -> `< b^n`
            (Left, [.., Item::E { block: 1, exp: move_exp }], _) => {
                let move_exp_const = *move_exp;
                self.rtape.push(Item::E { block: 1, exp: *move_exp });
                self.ltape.pop();
                // Copied from test.
                self.stats.num_tm_steps += 647_337 * move_exp_const;
            }
            // `c^n <` -> `c^(n-1) expanded-c <`
            (Left, [.., Item::E { block: 2, exp }], _) => {
                *exp -= 1;
                if *exp == 0 {
                    self.ltape.pop();
                }
                self.ltape.extend_from_slice(&block::LEFT_C);
                // 0 TM steps
            }
            // `b^n > 3` -> `b^(n-1) expanded-b > 3`
            (Right, [.., Item::E { block: 1, exp }], [.., Item::C(3)]) => {
                *exp -= 1;
                if *exp == 0 {
                    self.ltape.pop();
                }
                self.ltape.extend_from_slice(&block::LEFT_B);
                // 0 TM steps
            }
            (Left, [.., Item::Unreachable], _) | (Right, _, [.., Item::Unreachable]) => {
                return Err(Err::Unreachable);
            }
            // `> P b^n` -> `c^n > P`
            (Right, _, [.., Item::E { block: 1, exp: move_exp }, Item::P]) => {
                let move_exp_const = *move_exp;
                self.ltape.push(Item::E { block: 2, exp: *move_exp });
                pop_n(&mut self.rtape, 2);
                self.rtape.push(Item::P);
                self.stats.num_c_create += 1;
                // Copied from test.
                self.stats.num_tm_steps += 1_078_905 * move_exp_const;
            }

            _ => return Err(Err::UnknownTransition),
        }
        // transitions-test-end
        Ok(())
    }

    fn gen_step(&mut self, cfg: Config) -> Result<(), Err> {
        if self.dir == Direction::Right && (cfg.accel_uni_cycles || cfg.accel_counters) {
            if let Some(counter) = CounterAccel::new(&self.rtape) {
                if cfg.accel_uni_cycles {
                    if self.try_uni_cycle(&counter) {
                        self.stats.num_uni_cycles += 1;
                        return Ok(());
                    }
                }

                if cfg.accel_counters && !counter.idxs.is_empty() {
                    counter.apply(1, &mut self.rtape, &mut self.stats);
                    self.dir = Direction::Left;
                    return Ok(());
                }
            }
        }

        self.step()
    }

    fn run(&mut self, cfg: Config) -> Result<(), Err> {
        while self.sim_step < cfg.sim_step_limit {
            let old_a = self.stats.num_a_create;
            let old_c = self.stats.num_c_create;
            let _old_coll = self.stats.num_collisions;
            self.gen_step(cfg)?;
            self.sim_step += 1;

            // Print logic
            // if self.stats.num_collisions > _old_coll {
            //     println!("{self}");
            // }
            if cfg.print_only_cycles {
                if self.stats.num_a_create != old_a || self.stats.num_c_create != old_c {
                    println!("{self}");
                }
            } else {
                if self.sim_step & ((1 << cfg.print_mod) - 1) == 0 {
                    println!("{self}");
                }
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
        write!(f, "{}: ", self.sim_step.bright_white())?;
        if DSP_ROTATE_TAPE {
            self.rtape.iter().try_for_each(|item| write!(f, "{item}"))?;
            write!(f, " {} ", if self.dir == Direction::Left { '<' } else { '>' }.bright_green().bold())?;
            self.ltape.iter().rev().try_for_each(|item| write!(f, "{item}"))?;
        } else {
            self.ltape.iter().try_for_each(|item| write!(f, "{item}"))?;
            write!(f, " {} ", if self.dir == Direction::Left { '<' } else { '>' }.bright_green().bold())?;
            self.rtape.iter().rev().try_for_each(|item| write!(f, "{item}"))?;
        }
        write!(f, "   {}", self.stats)
    }
}

impl fmt::Display for SimStats {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Stats({}, {}, {}, {}, {}, {}, {})",
            self.num_counter_increments.blue(),
            self.num_tm_steps.blue(),
            self.num_collisions.blue(),
            self.num_strides.blue(),
            self.num_uni_cycles.blue(),
            self.num_a_create.blue(),
            self.num_c_create.blue(),
        )
    }
}

fn raw_parse(s: &str) -> Result<(Configuration, Direction)> {
    let mut conf = Configuration {
        ltape: Tape::new(),
        rtape: Tape::new(),
        dir: Direction::Right,
        sim_step: 0,
        stats: Default::default(),
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

    #[argh(switch, short = 'x', description = "interpret step limit directly (not as 2^n)?")]
    non_exponential_steps: bool,
    #[argh(switch, description = "disable counter acceleration")]
    no_accel_counters: bool,
    #[argh(switch, description = "disable @uni-cycles acceleration")]
    no_accel_uni_cycles: bool,
    #[argh(switch, short = 'a', description = "only print when a new a^1 block or c-block is produced.")]
    print_only_cycles: bool,
}

#[derive(Clone, Copy, Debug)]
struct Config {
    sim_step_limit: usize,
    print_mod: u8,
    accel_counters: bool,
    accel_uni_cycles: bool,
    print_only_cycles: bool,
}

// new run:               cargo run --release --bin no1 60 30
// explore configuration: cargo run --release --bin no1 8 0 --conf "2 x^7640 D x^10344 2 x^7640 D x^10344 1 x^7640 D x^10345 3 x^7639 D x^10347 3 < ! "
// explore in tui:        cargo run --release --bin no1 8 0 --conf "1 > P" --tui
fn main() -> Result<()> {
    color_eyre::install()?;

    let args: Args = argh::from_env();
    let sim_step_limit = if args.non_exponential_steps {
        args.sim_step_limit.try_into().unwrap()
    } else {
        2usize.checked_pow(args.sim_step_limit).unwrap()
    };
    let cfg = Config {
        sim_step_limit,
        print_mod: args.print_mod,
        accel_counters: !args.no_accel_counters,
        accel_uni_cycles: !args.no_accel_uni_cycles,
        print_only_cycles: args.print_only_cycles,
    };
    let mut conf = args.conf;
    // dbg!(cfg);
    println!("{}", conf);

    if args.tui {
        tui(conf, cfg)?;
    } else {
        let ret = conf.run(cfg);
        println!("{conf}");
        dbg!(&ret);
    }

    Ok(())
}

fn tui(mut conf: Configuration, mut cfg: Config) -> Result<()> {
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
                state = conf.run(cfg);
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
    use bbc::machine::{Head, Machine};
    use color_eyre::eyre::{Context, ContextCompat};
    use itertools::repeat_n;
    use std::fmt::Write;

    use super::*;

    // TODO: test compressions
    // TODO: test final configuration

    // cargo test transitions -- --nocapture
    #[test]
    fn transitions() {
        let lines: Vec<&str> = include_str!("no1.rs")
            .lines()
            .skip_while(|l| !l.contains("transitions-test-begin"))
            .take_while(|l| !l.contains("transitions-test-end"))
            .map(|l| l.trim())
            .collect();

        let impls: Vec<&&str> = lines
            .iter()
            .filter(|l| (l.starts_with("(Left, ") || l.starts_with("(Right, ")) && !l.contains("Unreachable"))
            .collect();
        assert!(!impls.is_empty(), "no transition implementations found");

        let rules: Vec<(_, _)> = lines
            .iter()
            .filter_map(|l| {
                fn parse(l: &str, r: &str, msg: &'static str) -> String {
                    r.split('`').skip(1).next().with_context(|| format!("{l:?}")).expect(msg).to_owned()
                }
                l.splitn(2, " -> ")
                    .collect_tuple()
                    .map(|(from, to)| (parse(l, from, "invalid from rule"), parse(l, to, "invalid to rule")))
            })
            .collect();
        assert_eq!(
            impls.len(),
            rules.len(),
            "implementations & rule descriptions do not match {:#?}",
            rules.iter().zip_longest(impls.iter()).map(|i| format!("{i:?}")).collect::<Vec<_>>()
        );

        for (from, to) in rules {
            println!("\n* {from:?} -> {to:?}");
            let (from, to) = from
                .strip_prefix("end")
                .map(|from| (from.to_string(), to.to_string()))
                .unwrap_or_else(|| (format!("! {from}"), format!("! {to}")));
            let (from, to) = from
                .strip_suffix("end")
                .map(|from| (from.to_string(), to.to_string()))
                .unwrap_or_else(|| (format!("{from} !"), format!("{to} !")));

            if from.contains("^n") {
                if to.contains("expanded-") {
                    let (block, to) = if to.contains("expanded-b") {
                        ("b", to.replace("expanded-b", &block2str(&block::LEFT_B)))
                    } else {
                        assert!(to.contains("expanded-c"));
                        ("c", to.replace("expanded-c", &block2str(&block::LEFT_C)))
                    };
                    for (exp_n, exp_n1) in [("^1", ""), ("^2", &format!("{block}^1"))] {
                        test_fixed_n(from.replace("^n", exp_n), to.replace(&format!("{block}^(n-1)"), exp_n1), false);
                    }
                } else {
                    let is_xn_transfer = from.contains("x^n");
                    test_fixed_n(from.replace("^n", "^1"), to.replace("^n", "^1"), is_xn_transfer);
                    test_fixed_n(from.replace("^n", "^2"), to.replace("^n", "^2"), is_xn_transfer);
                }
            } else {
                test_fixed_n(from, to, false);
            }
        }
    }

    fn test_fixed_n(from: String, to: String, skip_pop_x: bool) {
        let from: Configuration = from.parse().with_context(|| from).unwrap();
        let mut to: Configuration = to.parse().with_context(|| to).unwrap();

        to.sim_step = 1;

        test_conf(&from, &to);

        // merge x
        for dir in [Direction::Left, Direction::Right] {
            let (mut from_clone, mut to_clone) = (from.clone(), to.clone());
            let from_tape = if dir == Direction::Left { &mut from_clone.ltape } else { &mut from_clone.rtape };
            let to_tape = if dir == Direction::Left { &mut to_clone.ltape } else { &mut to_clone.rtape };
            match to_tape.as_mut_slice() {
                [Item::Unreachable, Item::X(exp @ 1), ..] => {
                    *exp = 11;
                    from_tape.insert(1, Item::X(10));
                    test_conf(&from_clone, &to_clone);
                }
                _ => (),
            }
        }

        if skip_pop_x {
            return;
        }

        // pop x
        for dir in [Direction::Left, Direction::Right] {
            let (mut from_clone, mut to_clone) = (from.clone(), to.clone());
            let from_tape = if dir == Direction::Left { &mut from_clone.ltape } else { &mut from_clone.rtape };
            let to_tape = if dir == Direction::Left { &mut to_clone.ltape } else { &mut to_clone.rtape };
            match from_tape.as_mut_slice() {
                [Item::Unreachable, Item::X(exp @ 1), ..] => {
                    *exp = 11;
                    to_tape.insert(1, Item::X(10));
                    test_conf(&from_clone, &to_clone);
                }
                _ => (),
            }
        }
    }

    fn test_conf(from: &Configuration, to: &Configuration) {
        println!("\t`{}` -> `{}`", strip_stats_and_steps(from.to_string()), strip_stats_and_steps(to.to_string()));

        let cfg = Config {
            sim_step_limit: 1,
            print_mod: 63,
            accel_counters: true,
            accel_uni_cycles: true,
            print_only_cycles: false,
        };
        let mut conf = from.clone();
        assert_eq!(conf.run(cfg), Err(Err::StepLimit), "conf run failed");
        assert_eq!(
            conf,
            *to,
            "conf {} -!-> {}",
            strip_stats_and_steps(from.to_string()),
            strip_stats_and_steps(to.to_string())
        );
        println!("\t\tsim conf ok");

        let mut start: RawConf = from.into();
        let end: RawConf = to.into();
        let debug = if start.tape.iter().map(|t| t.len()).sum::<usize>() < 100 { true } else { false };

        let valid = start.run(end, 10_000_000, debug);
        assert!(
            valid,
            "raw conf {} -!-> {}",
            strip_stats_and_steps(from.to_string()),
            strip_stats_and_steps(to.to_string())
        );
        assert_eq!(conf.stats.num_tm_steps, start.steps);
        println!("\t\traw conf ok");
    }

    type RawTape = Vec<u8>;

    struct RawConf {
        tape: [RawTape; 2],
        head: Head,
        steps: Exp,
    }

    impl RawConf {
        fn run(&mut self, end: RawConf, steps_limit: Exp, debug: bool) -> bool {
            let machine = Machine::from("1RB1RD_1LC0RC_1RA1LD_0RE0LB_---1RC");
            if debug {
                println!("\t   ->   {end}");
            }
            loop {
                if debug {
                    println!("\t\t{self}");
                }
                if self.head == end.head {
                    // TODO: use expected steps as stop signal?
                    let tapes_match = self.tape.iter().zip(end.tape.iter()).all(|(s, e)| {
                        e.strip_suffix(s.as_slice()).map(|zeros| zeros.iter().all(|i| *i == 0)).unwrap_or(false)
                    });
                    if tapes_match {
                        return true;
                    }
                }
                if self.steps == steps_limit {
                    return false;
                }

                let symbol = self.tape[self.head.direction.idx()].pop().unwrap_or(0);
                assert_ne!(symbol, u8::MAX, "reached unreachable symbol `!`");
                let trans = machine.get_transition(symbol, self.head.state).unwrap();
                self.head = trans.head;
                self.tape[self.head.direction.opp_idx()].push(trans.symbol);

                self.steps = self.steps.checked_add(1).unwrap();
            }
        }
    }

    impl From<&Configuration> for RawConf {
        fn from(value: &Configuration) -> Self {
            use Direction::*;

            fn extend_from_item(raw_tape: &mut RawTape, item: &Item, side: Direction) {
                let mut extend =
                    |bytes: &[u8]| raw_tape.extend(bytes.into_iter().filter_map(|b| (*b != b' ').then(|| *b - b'0')));
                match *item {
                    Item::D if side == Left => extend(b"01101"),
                    Item::D if side == Right => extend(b"01011"),
                    Item::P => extend(b"011"),
                    Item::C(0) if side == Left => extend(b"011 0111 011"),
                    Item::C(1) if side == Left => extend(b"011 0 01"),
                    Item::C(2) if side == Left => extend(b"01111 011"),
                    Item::C(3) => extend(b"01 01"),
                    Item::X(exp) => (0..exp.try_into().unwrap()).for_each(|_| extend(b"011 011")),
                    Item::L(run) if side == Left => run.to_string().bytes().for_each(|c| {
                        raw_tape.push(0);
                        raw_tape.extend(repeat_n(1, (c - b'0') as usize))
                    }),
                    Item::E { block, exp } => {
                        let tape: &[Item] = match block {
                            0 => &block::LEFT_A,
                            1 if side == Left => &block::LEFT_B,
                            1 if side == Right => &block::RIGHT_B,
                            2 => &block::LEFT_C,
                            _ => unimplemented!(),
                        };
                        (0..exp.try_into().unwrap()).for_each(|_| extend_from_tape(raw_tape, tape, side))
                    }
                    Item::Unreachable => raw_tape.push(u8::MAX),
                    _ => unimplemented!(),
                }
            }

            fn extend_from_tape(raw_tape: &mut RawTape, tape: &[Item], side: Direction) {
                tape.iter().for_each(|item| extend_from_item(raw_tape, item, side))
            }
            let mut ltape = Vec::new();
            extend_from_tape(&mut ltape, &value.ltape, Left);
            let mut rtape = Vec::new();
            extend_from_tape(&mut rtape, &value.rtape, Right);

            let head = match value.dir {
                Left => {
                    rtape.extend([0, 1]);
                    Head { state: 2, direction: value.dir } // <C 10
                }
                Right => {
                    Head { state: 0, direction: value.dir } // A>
                }
            };

            RawConf { tape: [ltape, rtape], head, steps: 0 }
        }
    }

    impl fmt::Display for RawConf {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            fn write_symbol(f: &mut fmt::Formatter<'_>, i: &u8) -> fmt::Result {
                f.write_char(if *i == u8::MAX { '!' } else { (i + b'0') as char })
            }

            write!(f, "{}: ", self.steps)?;
            self.tape[0].iter().try_for_each(|i| write_symbol(f, i))?;
            write!(f, " {} ", self.head)?;
            self.tape[1].iter().rev().try_for_each(|i| write_symbol(f, i))
        }
    }

    fn block2str(block: &[Item]) -> String {
        String::from_utf8(strip_ansi_escapes::strip(block.iter().join("")).unwrap()).unwrap()
    }

    #[test]
    fn parse_conf() -> Result<()> {
        for inp in ["0:  2 x^3 P a^4 DD x^167 31 x^17 L(432)  >  3 x^70 P", "0: 0 < 1 !"] {
            let conf: Configuration = inp.parse()?;
            let strip_colors = String::from_utf8(strip_ansi_escapes::strip(conf.to_string())?)?;
            assert_eq!(
                inp.split_whitespace().collect::<String>(),
                strip_stats_and_steps(strip_colors).split_whitespace().collect::<String>()
            );
        }
        Ok(())
    }

    fn strip_stats_and_steps(s: String) -> String {
        s.split(":").skip(1).next().unwrap().split("Stats").next().unwrap().trim().to_owned()
    }
}
