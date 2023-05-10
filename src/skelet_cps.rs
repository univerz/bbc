// reimplementation of reimplementation of skelet's implementation :) https://gist.github.com/savask/1c43a0e5cdd81229f236dcf2b0611c3f

use anyhow::{Context, Result};
use hashbrown::{HashMap, HashSet};
use indexmap::IndexSet;
use itertools::{iproduct, Itertools};
use std::{
    fmt,
    str::{self, FromStr},
};

use crate::{
    interner::{ITape, InternerTape},
    machine::{Direction, Head, Machine},
    ui::{Display1, Display2},
    ui_dbg, Infinite, ProverResult,
};

#[derive(Debug)]
pub enum Err {
    Halt,
    StepLimit,
    TotalStepLimit,
    ConfLimit,
}

const FAST_MODE: bool = false;
// const FAST_MODE: bool = true;

const RUN_OLD_IMPL: bool = false;

const NOT_VISITED_0: u8 = u8::MAX;
// const NOT_VISITED_0: u8 = 0;

type Interner = InternerTape<u8>;

impl Display2<Direction, &InternerTape<u8>> for ITape {
    fn fmt(&self, direction: Direction, interner: &InternerTape<u8>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let it = interner[*self].iter();
        let mut it: Box<dyn Iterator<Item = _>> =
            if direction == Direction::Left { Box::new(it) } else { Box::new(it.rev()) };
        it.try_for_each(|symbol| write!(f, "{}", symbol2char(symbol)))
    }
}

fn symbol2char(i: &u8) -> char {
    if *i == u8::MAX { '.' } else { (i + b'0') as char }
}

fn format_conf(tape: &[u8], mut pos: usize, head: Head) -> String {
    if head.direction == Direction::Right {
        pos -= 1;
    }
    format!(
        "{}{}{}",
        tape[..pos].iter().map(symbol2char).collect::<String>(),
        head,
        tape[pos..].iter().map(symbol2char).collect::<String>(),
    )
}

fn format_conf_old(tape: &[Vec<u8>; 2], head: Head) -> String {
    format!(
        "{}{}{}",
        tape[0].iter().map(symbol2char).collect::<String>(),
        head,
        tape[1].iter().rev().map(symbol2char).collect::<String>(),
    )
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct Segment {
    itape: ITape,
    mode: u8,
}

impl Segment {
    pub fn zeros(sizes: SegmentSizes, interner: &mut Interner) -> Segments {
        let mut segment = |mode| Segment {
            itape: interner.get_or_insert(&vec![NOT_VISITED_0; sizes.0[mode] as usize]),
            mode: mode as u8,
        };
        [segment(0), segment(1), segment(2)]
    }

    pub fn empty() -> Segment {
        Segment { itape: ITape::empty(), mode: u8::MAX }
    }

    #[inline(always)]
    // 0 2 1 -> 0 1 2
    pub fn compat_mode(&self) -> u8 {
        if self.mode == 2 { 1 } else { self.mode * 2 }
    }
}

impl Display2<Direction, &Interner> for Segment {
    fn fmt(&self, mut direction: Direction, interner: &Interner, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !RUN_OLD_IMPL {
            direction = Direction::Left; // always left2right
        }
        write!(f, "{}, {}", self.itape.dsp(direction, interner), self.compat_mode())
    }
}

/// 0 == left, 1 == right, 2 == middle
type Segments = [Segment; 3];
/// 0 == left, 1 == right, 2 == middle
#[derive(Clone, Copy, Debug)]
pub struct SegmentSizes(pub [u8; 3]);

impl fmt::Display for SegmentSizes {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}-{}-{}", self.0[0], self.0[1], self.0[2])
    }
}

impl FromStr for SegmentSizes {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let err = || {
            format!("{s:?} should look like `1,2,3` (`1-2-3`) where numbers are length of left, right & middle segment")
        };
        let separator = if s.contains(',') { ',' } else { '-' };
        s.split(separator)
            .map(|i| i.parse::<u8>())
            .collect::<std::result::Result<Vec<_>, _>>()
            .map_err(|_| err())?
            .try_into()
            .map(|s| SegmentSizes(s))
            .map_err(|_| err())
    }
}

type SegmentSpace<'a> = &'a [SegmentSizes];

pub fn size2space(max_segment_size: u8) -> Vec<SegmentSizes> {
    // let a = max_segment_size; vec![SegmentSizes([a, a, a])]
    // (11..=14).map(|a| SegmentSizes([a, a, a])).collect()
    if FAST_MODE {
        (1..=max_segment_size).map(|a| SegmentSizes([a, a, a])).collect()
    } else {
        iproduct!(1..=max_segment_size, 1..=max_segment_size, 1..=max_segment_size)
            .map(|(a, b, c)| SegmentSizes([a, b, c]))
            .sorted_by_key(|s| s.0[0] + s.0[1] + s.0[2])
            .collect()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct Configuration {
    tape: Segments,
    head: Head,
}

impl Configuration {
    pub fn new(zeros: Segments) -> Configuration {
        // 0..0 0..0 A> 0..0
        Configuration { tape: zeros, head: Head { state: 0, direction: Direction::Right } }
    }

    /// runs until head leaves the tape, split & return new conf, C (`<S ABC` -> `<S AB` & `C`) & end conf string if in tui mode
    fn run(
        mut self,
        machine: &Machine,
        total_step_limit: &mut usize,
        local_limit: usize,
        interner: &mut Interner,
    ) -> Result<Option<(Configuration, Segment, String)>, Err> {
        if RUN_OLD_IMPL {
            return self.run_old(machine, total_step_limit, local_limit, interner);
        }

        let mut tape: Vec<u8> = Vec::new();
        let mut pos = usize::MAX;
        tape.extend_from_slice(&interner[self.tape[0].itape]);
        if self.head.direction == Direction::Left {
            pos = tape.len() - 1; // 0< 2 1
        }
        tape.extend_from_slice(&interner[self.tape[2].itape]);
        if pos == usize::MAX {
            pos = tape.len(); // 0 2 >1 
        }
        tape.extend_from_slice(&interner[self.tape[1].itape]);
        let mut history: HashSet<Vec<u8>> = HashSet::new();

        loop {
            let mut symbol = tape[pos];
            if symbol == NOT_VISITED_0 {
                symbol = 0;
            }
            let trans = machine.get_transition(self.head, symbol).ok_or(Err::Halt)?;
            if trans.head.state >= machine.states {
                return Err(Err::Halt);
            }
            self.head = trans.head;
            tape[pos] = trans.symbol;

            pos += 2 * self.head.direction as usize;
            ui_dbg!("\t\t{}", format_conf(&tape, pos, self.head)); // think before moving this
            if pos == 0 || pos > tape.len() {
                break;
            }
            pos -= 1;

            let mut new_tape = tape.clone();
            // `symbols + 1` because of NOT_VISITED_0 == u8::MAX symbol (overflow)
            new_tape[pos] += (machine.symbols + 1) * (self.head.state + 1);
            // ui_dbg!("\t\t{} {} {} {:?}", tape.iter().map(symbol2char).collect::<String>(), self.head, pos, new_tape);
            let new = history.insert(new_tape);
            if !new {
                ui_dbg!("\t\t\tcycle");
                return Ok(None);
            }

            *total_step_limit -= 1;
            if *total_step_limit == local_limit {
                return Err(Err::StepLimit);
            }
        }
        let s = if cfg!(not(feature = "ui_tui")) { String::new() } else { format_conf(&tape, pos, self.head) };

        let initial_segments = self.tape;
        let dir = self.head.direction;
        let mut end_tape = tape.as_slice();
        // init:                   0 2 1                         0    2 1
        // end:  (dir.idx == 0)  < 2 1 0out || (dir.idx() == 1)  1out 0 2 >
        let it = if dir == Direction::Left { [(2, 0), (1, 2), (0, 1)] } else { [(1, 0), (0, 2), (2, 1)] };
        it.into_iter().for_each(|(eidx, iidx)| {
            let initial_segment = initial_segments[iidx];
            let (tape, new_end_tape) = end_tape.split_at(initial_segment.itape.len());
            end_tape = new_end_tape;
            self.tape[eidx] = Segment { itape: interner.get_or_insert(tape), mode: initial_segment.mode }
        });
        let mut out_segment = Segment::empty();
        std::mem::swap(&mut out_segment, &mut self.tape[dir.idx()]);

        Ok(Some((self, out_segment, s)))
    }

    #[allow(unused)]
    fn run_old(
        mut self,
        machine: &Machine,
        total_step_limit: &mut usize,
        local_limit: usize,
        interner: &mut Interner,
    ) -> Result<Option<(Configuration, Segment, String)>, Err> {
        // TODO(perf): switch to single tape?
        let mut tape = [interner[self.tape[0].itape].to_vec(), interner[self.tape[1].itape].to_vec()];
        tape[self.head.direction.opp_idx()].extend_from_slice(&interner[self.tape[2].itape]);
        // let mut history: HashSet<(ITape, ITape, Head)> = HashSet::new();

        while let Some(mut symbol) = tape[self.head.direction.idx()].pop() {
            if symbol == NOT_VISITED_0 {
                symbol = 0;
            }
            let trans = machine.get_transition(self.head, symbol).ok_or(Err::Halt)?;
            if trans.head.state >= machine.states {
                return Err(Err::Halt);
            }
            self.head = trans.head;
            tape[self.head.direction.opp_idx()].push(trans.symbol);
            ui_dbg!("\t\t{}", format_conf_old(&tape, self.head));

            // let new = history.insert((interner.get_or_insert(&tape[0]), interner.get_or_insert(&tape[1]), self.head));
            // if !new {
            //     ui_dbg!("\t\t\tcycle");
            //     return Ok(None);
            // }

            *total_step_limit -= 1;
            if *total_step_limit == local_limit {
                return Err(Err::StepLimit);
            }
        }
        let s = if cfg!(not(feature = "ui_tui")) { String::new() } else { format_conf_old(&tape, self.head) };
        // `[] <S AB C`, last item on tape is closest to head

        // end tape \       |   0 2 1    // start idxs; ? == empty; x == last segment out
        // <==dir0 i2 i1 i0 | ? 2 1 x    // mapped to conf
        // i0 i1 i2 >==dir1 |   x 1 2 ?
        // new conf segment = end_tape position & size
        // [idx()]    = ? TBD
        // [2]        = first from top - size(idx())
        // [op_idx()] = second - size(2)
        // out        = last - size(op_idx())

        let initial_segments = self.tape;
        let dir = self.head.direction;
        let mut end_tape = tape[dir.opp_idx()].as_slice();
        let mut pop = |idx| {
            let initial_segment: Segment = initial_segments[idx];
            let (tape, new_end_tape) = end_tape.split_at(initial_segment.itape.len());
            end_tape = new_end_tape;
            Segment { itape: interner.get_or_insert(tape), mode: initial_segment.mode }
        };

        let out_segment = pop(dir.opp_idx());
        self.tape[dir.opp_idx()] = pop(2);
        self.tape[2] = pop(dir.idx());
        self.tape[dir.idx()] = Segment::empty();

        Ok(Some((self, out_segment, s)))
    }
}

impl Display1<&Interner> for Configuration {
    fn fmt(&self, interner: &Interner, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let tape = |idx, mut direction| {
            if !RUN_OLD_IMPL {
                direction = Direction::Left;
            }
            self.tape[idx as usize].itape.dsp(direction, interner)
        };
        if self.head.direction == Direction::Left {
            write!(
                f,
                "{}{}{}{}",
                tape(0, Direction::Left),
                self.head,
                tape(2, Direction::Right),
                tape(1, Direction::Right)
            )
        } else {
            write!(
                f,
                "{}{}{}{}",
                tape(0, Direction::Left),
                tape(2, Direction::Left),
                self.head,
                tape(1, Direction::Right)
            )
        }?;
        [0, 2, 1].into_iter().try_for_each(|idx| write!(f, " {}", self.tape[idx].compat_mode()))
    }
}

/// see the original source how this shoud work; to prevent reevaluation, this version stores configurations & dependent
/// edges that need to be reevaluated (eg to try to insert new edge) if there is new item added to `to`.
#[derive(Default, Clone)]
pub struct Continuation {
    to: Vec<Segment>,
    /// configurations that should be explored after new `to` Tape is added, because they used this `direction + from` before
    confs: Vec<Configuration>,
    /// conts that replaced this one on the edge & should be updated if new tape is added to `to`
    deps: Vec<Segment>,
}

impl Continuation {
    fn init(zeros: Segments) -> Continuations {
        // 0..0 -> 0..0
        let mut ret = HashMap::with_capacity(2);
        let mut do_it = |mode| {
            let zeros = zeros[mode];
            ret.insert((mode, zeros), Continuation { to: vec![zeros], confs: Vec::new(), deps: Vec::new() });
        };
        do_it(Direction::Left.idx());
        do_it(Direction::Right.idx());
        ret
    }

    fn get(conts: &mut Continuations, direction: usize, from: Segment) -> &mut Continuation {
        // TODO(perf): custom struct made from ITape fields + u8 can shrink idx size from 12 -> 8bytes (&ITape can probably fit in less space too for ctl purposes)
        conts.entry((direction, from)).or_default()
    }

    fn add_edge(
        &mut self,
        confs: &mut Configurations,
        to: Segment,
        add_edges: &mut Vec<(Segment, Segment)>,
        _dir: Direction,
        _from: Segment,
        _interner: &Interner,
    ) {
        if !self.to.contains(&to) {
            ui_dbg!("\t\tadding edge ({_dir}, {}) -> {}", _from.dsp(_dir, _interner), to.dsp(_dir, _interner));
            self.confs.iter().for_each(|source| CPS::add_conf(confs, *source, to, _interner));
            self.deps.iter().map(|from| (*from, to)).collect_into(add_edges);
            self.to.push(to);
        }
    }
}

type Continuations = HashMap<(usize, Segment), Continuation>; // `usize` is faster than `Direction`, probably because `.opp().idx()` & inlining
type Configurations = IndexSet<Configuration>;

/// ClosedPositionSet
pub struct CPS {
    conts: Continuations,
    confs: Configurations,
    pub segment_sizes: SegmentSizes,
    step_limit: usize,
    interner: Interner,

    _conf2end: Vec<String>,
}

impl CPS {
    fn add_conf(confs: &mut Configurations, mut conf: Configuration, segment: Segment, _interner: &Interner) {
        conf.tape[conf.head.direction.idx()] = segment;
        let (_idx, _new) = confs.insert_full(conf);
        ui_dbg!(
            "\t\t\tconf {} {}",
            if _new { "added" } else { "exists" },
            confs.get_index(_idx).unwrap().dsp(_interner)
        );
    }

    pub fn run(&mut self, machine: &Machine) -> Result<(), Err> {
        let CPS { conts, confs, segment_sizes: _, step_limit, interner, _conf2end } = self;
        let step_limit = *step_limit;
        let mut total_step_limit = if FAST_MODE {
            100 * step_limit
        } else {
            10 * step_limit // 274x 1RB---_1RC1RA_0RD0RB_1LE1RD_1LF0LE_0RB0RE 
        };

        let mut conf_id = 0;
        while let Some(old_conf) = confs.get_index(conf_id) {
            ui_dbg!("running conf id={conf_id} {}", old_conf.dsp(interner));
            // `<S ABC` -> `<S AB` + `C`
            let local_limit = total_step_limit.saturating_sub(step_limit);
            let Some((conf, c, _last_conf)) = old_conf.run(machine, &mut total_step_limit, local_limit, interner)? else {
                conf_id += 1;
                continue; // cycle -> we can skip this configuration
            };

            let dir = conf.head.direction;
            #[cfg(feature = "ui_tui")]
            _conf2end.push(_last_conf);

            // from -> to; accumulates new edges due to dependencies
            let mut add_edges: Vec<(Segment, Segment)> = Vec::new();
            // `ab s> c` -> `<S ABC` => copy edges starting from c into C
            ui_dbg!("\tnew edge from replace:");
            let old_c = old_conf.tape[dir.opp_idx()];
            if old_c != c {
                let old = Continuation::get(conts, dir.opp_idx(), old_c);
                ui_dbg!(
                    "\t\tadding dep {} to ({}, {})",
                    c.dsp(dir.opp(), interner),
                    dir.opp(),
                    old_c.dsp(dir.opp(), interner),
                );
                old.deps.push(c);

                let tos = old.to.clone();
                if !tos.is_empty() {
                    let cont = Continuation::get(conts, dir.opp_idx(), c);
                    tos.into_iter().for_each(|to| cont.add_edge(confs, to, &mut add_edges, dir.opp(), c, interner));
                }
            }
            ui_dbg!("\tnew edge from shift:");
            // `<S ABC` => add B -> C
            let b = conf.tape[dir.opp_idx()];
            Continuation::get(conts, dir.opp_idx(), b).add_edge(confs, c, &mut add_edges, dir.opp(), b, interner);
            ui_dbg!("\tnew edge from dependencies:");
            while let Some((from, to)) = add_edges.pop() {
                let cont = Continuation::get(conts, dir.opp_idx(), from);
                cont.add_edge(confs, to, &mut add_edges, dir.opp(), from, interner);
            }

            // `from b s> a` -> `<S ABC`; what to explore next?
            let from = confs[conf_id].tape[dir.idx()];
            ui_dbg!("\t{} -> ???", from.dsp(dir, interner));
            let cont = Continuation::get(conts, dir.idx(), from);
            assert!(!cont.to.is_empty());
            for to in cont.to.iter() {
                ui_dbg!("\t\ttesting edge {}", to.dsp(dir, interner));
                Self::add_conf(confs, conf, *to, interner);
            }

            if !cont.confs.contains(&conf) {
                ui_dbg!("\tadding conf {} to ({}, {})", conf.dsp(interner), dir, from.dsp(dir, interner));
                cont.confs.push(conf);
            }

            conf_id += 1;
            if conf_id > 1000000 {
                return Err(Err::ConfLimit);
            }
        }
        Ok(())
    }

    pub fn assert_closed(&mut self, machine: &Machine) -> Result<(), Err> {
        let lens1 = (self.conts.values().map(|cont| cont.to.len()).sum::<usize>(), self.confs.len());
        self.run(machine)?;
        let lens2 = (self.conts.values().map(|cont| cont.to.len()).sum(), self.confs.len());
        // dbg!(lens1, lens2);
        assert_eq!(lens1, lens2);
        Ok(())
    }

    pub fn new(machine: &Machine, segment_sizes: SegmentSizes) -> CPS {
        let step_limit = if FAST_MODE {
            let tape_len: usize = *segment_sizes.0.iter().max().unwrap() as usize * 3 + 5;
            10 * (machine.states as usize * machine.symbols as usize) * tape_len
        } else {
            let tape_len: usize = 17.min(*segment_sizes.0.iter().max().unwrap() as usize * 3);
            10 * (machine.states as usize * machine.symbols as usize) * (tape_len + 1) * 2usize.pow(tape_len as u32)
        };

        let mut interner = Interner::new();
        let zeros = Segment::zeros(segment_sizes, &mut interner);
        let mut confs = IndexSet::new();
        confs.insert(Configuration::new(zeros));
        CPS { conts: Continuation::init(zeros), confs, segment_sizes, step_limit, interner, _conf2end: Vec::new() }
    }

    pub fn prove_size(machine: &Machine, segment_sizes: SegmentSizes) -> Result<CPS, Err> {
        let mut cps = CPS::new(machine, segment_sizes);
        cps.run(machine)?;
        // println!("{cps}\n***************************************");
        // cps.assert_closed(machine)?;
        Ok(cps)
    }

    pub fn prove(machine: &Machine, segment_space: SegmentSpace) -> Option<CPS> {
        segment_space.iter().find_map(|sizes| Self::prove_size(machine, *sizes).ok())
    }
}

impl Into<ProverResult> for Option<CPS> {
    fn into(self) -> ProverResult {
        if let Some(cps) = self {
            ProverResult::Infinite(Infinite::CPS3(cps.segment_sizes))
        } else {
            ProverResult::Limit(format!("segment_size"))
        }
    }
}

impl fmt::Display for CPS {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (label, direction) in [("Left", Direction::Left), ("Right", Direction::Right)] {
            writeln!(f, "\n* {label} continuations:")?;
            self.conts
                .iter()
                .filter(|((o, _), _)| *o == direction.idx())
                .sorted_by_key(|((_, from), _)| from.dsp(direction, &self.interner).to_string())
                .try_for_each(|((_, from), cont)| {
                    writeln!(
                        f,
                        "\t{} --> {}",
                        from.dsp(direction, &self.interner),
                        cont.to.iter().map(|to| to.dsp(direction, &self.interner)).join(", ")
                    )
                })?;
        }
        writeln!(f, "\n* Transitions:")?;
        #[cfg(feature = "ui_tui")]
        self.confs
            .iter()
            .zip(self._conf2end.iter())
            .try_for_each(|(conf, to)| writeln!(f, "\t{} --> {to}", conf.dsp(&self.interner)))?;
        #[cfg(not(feature = "ui_tui"))]
        self.confs.iter().try_for_each(|conf| writeln!(f, "\t{}", conf.dsp(&self.interner)))?;
        let cnt_edges = self.conts.values().map(|cont| cont.to.len()).sum::<usize>();
        writeln!(f, "\n* #edges={}, #confs={}, sizes(l,r,m)={:?}", cnt_edges, self.confs.len(), self.segment_sizes)
    }
}

/// compares generated proofs with @savask's implementation
#[derive(Default, PartialEq, Debug)]
pub struct CPSCertif {
    edges: [Vec<(String, String)>; 2],
    confs: Vec<String>,
}

impl CPSCertif {
    fn from_cps(cps: Option<CPS>) -> CPSCertif {
        let mut ret = CPSCertif::default();
        if let Some(cps) = cps {
            cps.conts.iter().for_each(|((direction, from), cont)| {
                cont.to.iter().for_each(|to| {
                    let direction: Direction = (*direction).into();
                    ret.edges[direction.idx()].push((
                        from.dsp(direction, &cps.interner).to_string(),
                        to.dsp(direction, &cps.interner).to_string(),
                    ))
                })
            });
            for direction in [Direction::Left, Direction::Right] {
                ret.edges[direction.idx()].sort();
            }
            cps.confs.iter().for_each(|conf| {
                ret.confs.push(conf.dsp(&cps.interner).to_string());
            });
            ret.confs.sort();
        }
        ret
    }

    pub fn validate(line: &str, segment_space: SegmentSpace) -> Result<()> {
        let (machine, certif) = line.split_once(" ").context("invalid line")?;
        println!("testing: {machine}");

        let machine: Machine = machine.parse().unwrap();
        let certif: CPSCertif = certif.parse()?;

        let cps = CPS::prove(&machine, segment_space);
        // cps.as_ref().map(|cps| println!("{cps}"));
        let my_certif = CPSCertif::from_cps(cps);

        pretty_assertions::assert_eq!(certif, my_certif);
        Ok(())
    }
}

impl str::FromStr for CPSCertif {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut ret = CPSCertif::default();
        let mut it = s.split_whitespace();
        if it.next() == Some("Result") {
            for direction in [Direction::Left, Direction::Right] {
                let cnt: usize = it.next().context("invalid edges len")?.parse()?;
                for _ in 0..cnt {
                    let mut from_to: Vec<_> = (0..4).map(|_| it.next().unwrap().to_string()).collect();
                    if direction == Direction::Left {
                        [0, 2].into_iter().for_each(|idx| from_to[idx] = from_to[idx].chars().rev().collect())
                    }
                    ret.edges[direction.idx()].push((from_to[0..2].join(", "), from_to[2..4].join(", ")));
                }
                ret.edges[direction.idx()].sort();
            }
            let cnt: usize = it.next().context("invalid confs len")?.parse()?;
            let conf_it = it.chunks(4);
            let mut conf_it = conf_it.into_iter();
            for _ in 0..cnt {
                ret.confs.push(conf_it.next().unwrap().into_iter().join(" "));
            }
            ret.confs.sort();
        }

        Ok(ret)
    }
}
