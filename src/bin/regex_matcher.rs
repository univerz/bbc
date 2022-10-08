// quick ~rewrite of Frans's code; enhanced so it should handle (a*b*)* to simulate (a|b)*
// https://github.com/FransFaase/SymbolicTM

use anyhow::{Context, Result};
use bbc::machine::{Machine, Transition};
use hashbrown::HashMap;
use itertools::Itertools;

#[derive(Clone, Copy, Debug, PartialEq, parse_display::Display, parse_display::FromStr)]
pub enum Rep {
    #[display("+")]
    NonZero,
    #[display("*")]
    Any,
    #[display("@")]
    Infinite,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Item {
    Symbol(u8),
    Any,
    Rep(Tape, Rep),
}

impl Item {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, orient: usize) -> std::fmt::Result {
        match self {
            Item::Symbol(s) => write!(f, "{}", (s + '0' as u8) as char),
            Item::Any => write!(f, "."),
            Item::Rep(tape, rep) => {
                if tape.len() > 1 {
                    write!(f, "(")?;
                }
                fmt_tape(&tape, f, orient)?;
                if tape.len() > 1 {
                    write!(f, ")")?;
                }
                write!(f, "{}", rep)
            }
        }
    }
}

type Tape = Vec<Item>;
type TapeRef<'a> = &'a [Item];

fn fmt_tape(t: TapeRef<'_>, f: &mut std::fmt::Formatter<'_>, orient: usize) -> std::fmt::Result {
    let mut it: Box<dyn Iterator<Item = _>> = if orient == 1 { Box::new(t.iter().rev()) } else { Box::new(t.iter()) };
    it.try_for_each(|i| i.fmt(f, orient))
}

struct P<'a>(TapeRef<'a>);
impl<'a> std::fmt::Display for P<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fmt_tape(self.0, f, 0)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Pattern {
    state: u8,
    symbol: u8, // == head
    /// 0 == left tape; item closes to head is _last_ => right tape needs to be reversed in parsing & printing
    tape: [Tape; 2],
}

impl std::fmt::Display for Pattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: ", (self.state + 'A' as u8) as char)?;
        fmt_tape(&self.tape[0], f, 0)?;
        write!(f, "  {}  ", (self.symbol + '0' as u8) as char)?;
        fmt_tape(&self.tape[1], f, 1)
    }
}

impl std::str::FromStr for Pattern {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let pattern = pattern_parser::pattern(s)?;
        // let original = s.split_whitespace().join(" ");
        // let parsed = pattern.to_string().replace("  ", " ");
        // assert_eq!(original, parsed);
        Ok(pattern)
    }
}

peg::parser! {
    grammar pattern_parser() for str {
        rule whitespace() = quiet!{[' ' | '\t']+}

        rule symbol() -> u8 = s:$(['0'..='9']) { s.parse().unwrap() }

        rule item(orient: usize) -> Item = precedence!{
            s:symbol() r:$(['+'|'*'|'@']?) { let item = Item::Symbol(s); if r.is_empty() { item } else { Item::Rep(vec![item], r.parse::<Rep>().unwrap()) } }
            "." r:$(['+'|'*'|'@']?) { let item = Item::Any; if r.is_empty() { item } else { Item::Rep(vec![item], r.parse::<Rep>().unwrap()) } }
            "(" t:tape(orient) ")" r:$(['+'|'*'|'@']) { Item::Rep(t, r.parse::<Rep>().unwrap()) }
        }

        pub rule tape(orient: usize) -> Tape
            = items:(item(orient)+) { let mut items = items; if orient == 1 { items.reverse() }; items }

        pub rule pattern() -> Pattern
            = state:(['A'..='Z']) ":" whitespace() left:tape(0) whitespace() s:symbol() whitespace() right:tape(1) {
                Pattern { state: state as u8 - 'A' as u8, symbol: s, tape: [left, right] }
            }
    }
}

/// @returns `Some(a.remainder)` if `partial` && prefix of `a` matches `b`
/// @returns `Some(&[])` if !partial && `a` matches `b`
fn tmatch<'a, 'b>(a: TapeRef<'a>, b: TapeRef<'b>, partial: bool, aplus: bool) -> Option<TapeRef<'a>> {
    // println!("\t{} -> {}", P(a), P(b));

    // `->` == matches; `'` == `X -> X'`
    let ret = match (a.split_last(), b.split_last()) {
        // we want hard return for end conditions - no need to backtrack
        (_, None) if partial => Some(a), // `b` could be nested pattern and ends early
        (None, None) if !partial => return Some(&[]),
        (None, _) => return None,
        // `_A -> _A'`
        (Some((Item::Symbol(sa), aa)), Some((Item::Symbol(sb), bb))) if sa == sb => tmatch(aa, bb, partial, false),
        (Some((Item::Any | Item::Symbol(_), aa)), Some((Item::Any, bb))) => tmatch(aa, bb, partial, false),
        // `_A[eq_rep] -> _A'[eq_rep]`
        // `_A+ -> _A'[+*@]`
        (Some((Item::Rep(ta, ra), aa)), Some((Item::Rep(tb, rb), bb)))
            if (ra == rb || *ra == Rep::NonZero || (*ra == Rep::Any && aplus))
                && tmatch(&ta, &tb, false, false).is_some() =>
        {
            tmatch(aa, bb, partial, false)
        }
        // `_ -> _B*`
        (Some(_), Some((Item::Rep(_, Rep::Any), bb))) => tmatch(a, bb, partial, false),
        _ => None,
    };
    if ret.is_some() {
        return ret;
    }
    if let Some((Item::Rep(tb, _), bb)) = b.split_last() {
        // println!("before");
        return tmatch(a, tb, true, false)
            .map(|xa| {
                if
                /* a.len() > 0 && */
                a.len() == xa.len() {
                    return None;
                }

                // `_(AB)*AB -> _(A'B')+`
                // println!("after");
                let ret = tmatch(xa, b, partial, true);
                // println!("...{}", ret.is_some());
                if ret.is_some() {
                    return ret;
                }
                // `_AB -> _(A'B')[+*@]`
                tmatch(xa, bb, partial, false)
            })
            .flatten();
    }
    None
}

fn can_absorb(a: TapeRef<'_>, b: TapeRef<'_>) -> bool {
    // println!("can_absorb {} -> {}", P(a), P(b));

    // `A -> (B)[*+@]`
    if let Some((Item::Rep(tb, rb), bb)) = b.split_last() {
        // `A -> B` || `can_absorb(A, B)` || `A -> CB* & can_absorb(A, C)`
        tmatch(a, tb, false, false).is_some() || can_absorb(a, tb) || (*rb == Rep::Any && can_absorb(a, bb))
    } else {
        false
    }
}

fn tape_match(a: TapeRef<'_>, b: TapeRef<'_>) -> bool {
    // println!("{} -> {}", P(a), P(b));

    if tmatch(a, b, false, false).is_some() {
        return true;
    }

    (1..a.len()).rev().any(|s| {
        // remove absorbable part of `a` ~ `(AB+)*B -> (AB+)*`
        let (a1, a2) = a.split_at(s);

        can_absorb(a2, b) && tmatch(a1, b, false, false).is_some()
    })
}

fn matches(pat: &Pattern, pats: &Patterns, _matched_from: &mut Matched, rec: usize) -> bool {
    if rec == 0 {
        println!("        {pat}");
    } else {
        println!("     {rec}> {pat}");
    }
    if let Some((idx, _)) = pats.iter().find_position(|p| pat == *p) {
        //matched_from.entry(idx).or_default().push((true, ))
        println!("\t\t{idx} - exact match");
        return true;
    }
    let found = pats.iter().find_position(|p| {
        pat.state == p.state
            && pat.symbol == p.symbol
            && pat.tape.iter().zip(p.tape.iter()).all(|(tape, t)| tape_match(&tape, &t))
    });
    if let Some((idx, _)) = found {
        //matched_from.entry(idx).or_default().push((false, ))
        println!("\t\t{idx} - match");
        return true;
    }

    // `BA* -> B'A+ && B'`
    for tidx in 0..=1 {
        if let Some(Item::Rep(_, Rep::Any)) = pat.tape[tidx].last() {
            let mut pat = pat.clone();
            let mut last = pat.tape[tidx].pop().unwrap();
            let ret = matches(&pat, pats, _matched_from, rec + 1);
            if ret {
                if let Item::Rep(_, r) = &mut last {
                    *r = Rep::NonZero
                } else {
                    unreachable!()
                };
                pat.tape[tidx].push(last);
                if matches(&pat, pats, _matched_from, rec + 1) {
                    return true;
                }
            }
        }
    }

    println!("\t\t!!! NOT MATCHED !!!");
    false
}

/// explores all possibilities how to pop symbol from the new pattern's tape, pushes new symbol & tests if these new patterns are already in set
fn explore(
    mut pat: Pattern,
    trans: Transition,
    pats: &Patterns,
    matched_from: &mut Matched,
    mut orig_pat_len: Option<usize>,
) -> Result<bool> {
    let tidx = trans.head.orient as usize;
    loop {
        // println!("{i} {orig_pat_len:?} {}", P(&pat.tape[tidx]));
        match pat.tape[tidx].last_mut().context("empty tape")? {
            // `_0` || `_1`
            Item::Symbol(s) => {
                pat.symbol = *s;
                pat.tape[tidx].pop();
                break Ok(matches(&pat, pats, matched_from, 0));
            }
            // `_. -> _0 && _1`
            Item::Any => {
                pat.tape[tidx].pop();
                break Ok(SYMBOLS.iter().all(|s| {
                    pat.symbol = *s;
                    matches(&pat, pats, matched_from, 0)
                }));
            }
            // `A+ -> A*A` || `A@ -> A@A`
            Item::Rep(tape, rep @ (Rep::NonZero | Rep::Infinite)) => {
                let tape = tape.clone();
                if *rep == Rep::NonZero {
                    *rep = Rep::Any;
                }
                pat.tape[tidx].extend(tape);
                continue;
            }
            // `_BA* -> _B && _BA*A`
            Item::Rep(tape, Rep::Any) => {
                let tape = tape.clone();
                // this prevents recursive testing of original pattern in case of nested *
                // 0@(0*(11)*)*
                //     0@
                //         0@0
                //     0@(0*(11)*)*0*(11)*
                //         0@(0*(11)*)*0*
                //             0@(0*(11)*)*
                if !orig_pat_len.map(|l| l == pat.tape[tidx].len() - 1).unwrap_or(false) {
                    let mut pat_short = pat.clone(); // TODO: optimize
                    pat_short.tape[tidx].pop();
                    if !explore(pat_short, trans, pats, matched_from, orig_pat_len)? {
                        break Ok(false);
                    }
                    orig_pat_len = Some(pat.tape[tidx].len())
                }
                pat.tape[tidx].extend(tape);
                continue;
            }
        }
    }
}

pub type Patterns = Vec<Pattern>;
// true if exact match
pub type Matched = HashMap<usize, Vec<(bool, usize)>>;

const SYMBOLS: [u8; 2] = [0, 1];

fn main() -> Result<()> {
    // unsafe { backtrace_on_stack_overflow::enable() };
    // return test();

    let mut lines = std::io::stdin().lines();

    let machine = Machine::from(&lines.next().context("no machine line")??);
    println!("machine: {}", machine.to_string());

    let patterns: Vec<Pattern> = lines
        .filter_map(|l| l.map(|l| l.trim().to_owned()).ok())
        .filter(|l| !l.is_empty() && !l.starts_with("/"))
        .map(|l| l.parse::<Pattern>().map_err(Into::into))
        .collect::<Result<_>>()?;

    patterns.iter().enumerate().for_each(|(idx, pat)| println!("{idx}\t{pat}"));

    let mut matched_from: Matched = HashMap::new();

    patterns.iter().enumerate().try_for_each(|(idx, pat)| {
        let trans = machine.get_transition(pat.symbol, pat.state).context("undefined transition")?;
        println!("\n{idx:<4}{pat} ({trans}):");

        let mut pat = pat.clone();
        pat.state = trans.head.state;
        pat.tape[1 - trans.head.orient as usize].push(Item::Symbol(trans.symbol));
        if !explore(pat, trans, &patterns, &mut matched_from, None)? {
            // println!("\t!!! NOT MATCHED !!!")
        }

        Result::<_>::Ok(())
    })?;

    // let empty = Tape { items: vec![Item { obj: Obj::Symbol(0), rep: Rep::Infinite }] };
    // patterns.iter().enumerate().for_each(|(idx, pat)| {
    //     if !matched_from.contains_key(&idx)
    //         && !(pat.state == 0 && pat.symbol == 0 && pat.tape.iter().all(|t| *t == empty))
    //     {
    //         println!("unused pattern: {idx}\t{pat}");
    //     }
    // });

    Ok(())
}

// TMatchTest
struct TMT {
    a: Tape,
    b: Tape,
}

impl TMT {
    fn t(a: &str, b: &str) {
        let a = pattern_parser::tape(a, 0).unwrap();
        let b = pattern_parser::tape(b, 0).unwrap();
        let t = TMT { a, b };
        let m = tape_match(&t.a, &t.b);
        println!("{t} ? {m:?}");
        if !m {
            dbg!(&t.a);
            dbg!(&t.b);
        }

        assert!(m);
    }
}

impl std::fmt::Display for TMT {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fmt_tape(&self.a, f, 0)?;
        write!(f, "  ->  ")?;
        fmt_tape(&self.b, f, 0)
    }
}

#[allow(unused)]
fn test() -> Result<()> {
    TMT::t("0@0", "0@");
    TMT::t("0@0", "0@((11)+0+)*");
    TMT::t("0@((11)+0+)*0", "0@((11)+0+)*"); // can_absorb

    Ok(())
}
