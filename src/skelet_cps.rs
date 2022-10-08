// reimplementation of reimplementation of skelet's implementation :) https://gist.github.com/savask/1c43a0e5cdd81229f236dcf2b0611c3f

use anyhow::{Context, Result};
use hashbrown::HashMap;
use indexmap::IndexSet;
use std::{fmt, str};

use crate::{
    interner::{ITape, InternedDisplay, InternerTape},
    machine::{Head, Machine, Orientation},
    ui_dbg, ProverResult,
};

#[derive(Debug)]
pub enum Err {
    Halt,
    Loop,
    ConfLimit,
}

type Interner = InternerTape<u8>;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct Configuration {
    /// 0 == left, 1 == right, 2 == middle
    tape: [ITape; 3],
    head: Head,
}

impl Configuration {
    pub fn new(zeros: ITape) -> Configuration {
        // 0..0 0..0 A> 0..0
        Configuration { tape: [zeros, zeros, zeros], head: Head { state: 0, orient: 1 } }
    }

    /// runs until head leaves the tape, split & return new conf, C (`<S ABC` -> `<S AB` & `C`) & end conf string if in tui mode
    fn run(
        mut self,
        machine: &Machine,
        mut step_limit: usize,
        segment_size: usize,
        interner: &mut Interner,
    ) -> Result<(Configuration, ITape, String), Err> {
        // TODO(perf): switch to single tape?
        let mut tape = [interner[self.tape[0]].to_vec(), interner[self.tape[1]].to_vec()];
        tape[self.head.op_orient()].extend_from_slice(&interner[self.tape[2]]);
        fn format_conf(tape: &[Vec<u8>; 2], head: Head) -> String {
            format!(
                "{}{}{}",
                tape[0].iter().map(u8::to_string).collect::<String>(),
                head,
                tape[1].iter().rev().map(u8::to_string).collect::<String>(),
            )
        }

        while step_limit > 0 {
            if let Some(symbol) = tape[self.head.orient as usize].pop() {
                let trans = machine.get_transition(symbol, self.head.state).ok_or(Err::Halt)?;
                if trans.head.state >= machine.states() {
                    return Err(Err::Halt);
                }
                self.head = trans.head;
                tape[1 - self.head.orient as usize].push(trans.symbol);
                step_limit -= 1;
                ui_dbg!("\t\t{}", format_conf(&tape, self.head));
            } else {
                let s = if cfg!(not(feature = "ui_tui")) { String::new() } else { format_conf(&tape, self.head) };

                // `[] <S AB C`, last item on tape is closest to head
                let mut segments =
                    tape[self.head.op_orient()].chunks(segment_size).rev().map(|chunk| interner.get_or_insert(chunk));
                self.tape[self.head.orient()] = ITape::empty();
                self.tape[2] = segments.next().unwrap();
                self.tape[self.head.op_orient()] = segments.next().unwrap();
                return Ok((self, segments.next().unwrap(), s));
            };
        }
        Err(Err::Loop)
    }
}

impl InternedDisplay for Configuration {
    type I = u8;
    fn fmti(&self, interner: &Interner, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let tape = |idx, orient| self.tape[idx as usize].fmt(orient, interner);
        if self.head.orient == 0 {
            write!(f, "{}{}{}{}", tape(0, 0), self.head, tape(2, 1), tape(1, 1))
        } else {
            write!(f, "{}{}{}{}", tape(0, 0), tape(2, 0), self.head, tape(1, 1))
        }
    }
}

/// see the original source how this shoud work; to prevent reevaluation, this version stores configurations & dependent
/// edges that need to be reevaluated (eg to try to insert new edge) if there is new item added to `to`.
#[derive(Default)]
pub struct Adjacent {
    to: Vec<ITape>,
    /// configurations that should be explored after new `to` Tape is added, because they used this `orient + from` before
    confs: Vec<Configuration>,
    /// adjs that replaced this one on the edge & should be updated if new tape is added to `to`
    deps: Vec<ITape>,
}

impl Adjacent {
    fn init(zeros: ITape) -> Adjacents {
        // 0..0 -> 0..0
        (0..=1)
            .map(|orient| ((orient, zeros), Adjacent { to: vec![zeros], confs: Vec::new(), deps: Vec::new() }))
            .collect()
    }

    fn get(adjs: &mut Adjacents, orient: u8, from: ITape) -> &mut Adjacent {
        // TODO(perf): custom struct made from ITape fields + u8 can shrink idx size from 12 -> 8bytes (&ITape can probably fit in less space too for ctl purposes)
        adjs.entry((orient, from)).or_default()
    }

    fn add_adj(
        &mut self,
        confs: &mut Configurations,
        to: ITape,
        try_add: &mut Vec<(ITape, ITape)>,
        _orient: Orientation,
        _from: ITape,
        _interner: &Interner,
    ) {
        if !self.to.contains(&to) {
            ui_dbg!("\t\tadding adj ({_orient}, {}) -> {}", _from.fmt(_orient, _interner), to.fmt(_orient, _interner));
            self.confs.iter().for_each(|source| CPS::add_conf(confs, *source, to, _interner));
            self.deps.iter().map(|from| (*from, to)).collect_into(try_add);
            self.to.push(to);
        }
    }
}

type Adjacents = HashMap<(Orientation, ITape), Adjacent>;
type Configurations = IndexSet<Configuration>;

/// ClosedPositionSet
pub struct CPS {
    adjs: Adjacents,
    confs: Configurations,
    segment_size: usize,
    step_limit: usize,
    interner: Interner,

    #[cfg(feature = "ui_tui")]
    conf2end: Vec<String>,
}

impl CPS {
    fn add_conf(confs: &mut Configurations, mut conf: Configuration, tape: ITape, _interner: &Interner) {
        conf.tape[conf.head.orient()] = tape;
        let (_idx, _new) = confs.insert_full(conf);
        ui_dbg!(
            "\t\t\tconf {} {}",
            if _new { "added" } else { "exists" },
            confs.get_index(_idx).unwrap().fmt(_interner)
        );
    }

    pub fn run(&mut self, machine: &Machine) -> Result<(), Err> {
        let CPS {
            adjs,
            confs,
            segment_size,
            step_limit,
            interner,
            #[cfg(feature = "ui_tui")]
            conf2end,
        } = self;
        let (segment_size, step_limit) = (*segment_size, *step_limit);

        let mut conf_id = 0;
        while let Some(old_conf) = confs.get_index(conf_id) {
            ui_dbg!("running conf id={conf_id} {}", old_conf.fmt(interner));
            // `<S ABC` -> `<S AB` + `C`
            let (conf, c, _last_conf) = old_conf.run(machine, step_limit, segment_size, interner)?;
            let orient = conf.head.orient;
            #[cfg(feature = "ui_tui")]
            conf2end.push(_last_conf);

            // from -> to; accumulates new edges due to dependencies
            let mut try_add: Vec<(ITape, ITape)> = Vec::new();
            // `ab s> c` -> `<S ABC` => copy edges starting from c into C
            ui_dbg!("\tnew adj from replace:");
            let old_c = old_conf.tape[conf.head.op_orient()];
            if old_c != c {
                let old = Adjacent::get(adjs, 1 - orient, old_c);
                ui_dbg!(
                    "\t\tadding dep {} to ({}, {})",
                    c.fmt(1 - orient, interner),
                    1 - orient,
                    old_c.fmt(1 - orient, interner),
                );
                old.deps.push(c);

                let tos = old.to.clone();
                if !tos.is_empty() {
                    let adj = Adjacent::get(adjs, 1 - orient, c);
                    tos.into_iter().for_each(|to| adj.add_adj(confs, to, &mut try_add, 1 - orient, c, interner));
                }
            }
            ui_dbg!("\tnew adj from shift:");
            // `<S ABC` => add B -> C
            let b = conf.tape[conf.head.op_orient()];
            Adjacent::get(adjs, 1 - orient, b).add_adj(confs, c, &mut try_add, 1 - orient, b, interner);
            ui_dbg!("\tnew adj from dependencies:");
            while let Some((from, to)) = try_add.pop() {
                Adjacent::get(adjs, 1 - orient, from).add_adj(confs, to, &mut try_add, 1 - orient, from, interner);
            }

            // `from b s> a` -> `<S ABC`; what to explore next?
            let from = confs[conf_id].tape[conf.head.orient()];
            ui_dbg!("\t{} -> ???", from.fmt(orient, interner));
            let adj = Adjacent::get(adjs, orient, from);
            assert!(!adj.to.is_empty());
            for to in adj.to.iter() {
                ui_dbg!("\t\ttesting adj {}", to.fmt(orient, interner));
                Self::add_conf(confs, conf, *to, interner);
            }

            if !adj.confs.contains(&conf) {
                ui_dbg!("\tadding conf {} to ({}, {})", conf.fmt(interner), orient, from.fmt(orient, interner));
                adj.confs.push(conf);
            }

            conf_id += 1;
            if conf_id > 100000 {
                return Err(Err::ConfLimit);
            }
        }
        Ok(())
    }

    pub fn assert_closed(&mut self, machine: &Machine) -> Result<(), Err> {
        let lens1 = (self.adjs.values().map(|adj| adj.to.len()).sum::<usize>(), self.confs.len());
        self.run(machine)?;
        let lens2 = (self.adjs.values().map(|adj| adj.to.len()).sum(), self.confs.len());
        // dbg!(lens1, lens2);
        assert_eq!(lens1, lens2);
        Ok(())
    }

    pub fn new(machine: &Machine, segment_size: usize) -> CPS {
        let tape_len: usize = 18.min(segment_size * 3);
        let step_limit =
            (machine.states() as usize * machine.symbols() as usize) * (tape_len + 1) * 2usize.pow(tape_len as u32);
        let mut interner = Interner::new();
        let zeros = interner.get_or_insert(&vec![0; segment_size]);
        let mut confs = IndexSet::new();
        confs.insert(Configuration::new(zeros));
        CPS {
            adjs: Adjacent::init(zeros),
            confs,
            segment_size,
            step_limit,
            interner,
            #[cfg(feature = "ui_tui")]
            conf2end: Vec::new(),
        }
    }

    pub fn prove_segment_size(machine: &Machine, segment_size: usize) -> Result<CPS, Err> {
        let mut cps = CPS::new(machine, segment_size);
        cps.run(machine)?;
        // println!("{cps}\n***************************************");
        // cps.assert_closed(machine)?;
        Ok(cps)
    }

    pub fn prove(machine: &Machine, max_segment_size: usize) -> Option<CPS> {
        (2..=max_segment_size).find_map(|segment_size| Self::prove_segment_size(machine, segment_size).ok())
    }
}

impl Into<ProverResult> for Option<CPS> {
    fn into(self) -> ProverResult {
        if self.is_some() { ProverResult::Infinite } else { ProverResult::Limit(format!("segment_size")) }
    }
}

impl fmt::Display for CPS {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (label, orient) in [("Left", 0), ("Right", 1)] {
            writeln!(f, "\n* {label} continuations:")?;
            self.adjs.iter().filter(|((o, _), _)| *o == orient).try_for_each(|((orient, from), adj)| {
                adj.to.iter().try_for_each(|to| {
                    writeln!(f, "\t{} --> {}", from.fmt(*orient, &self.interner), to.fmt(*orient, &self.interner))
                })
            })?;
        }
        writeln!(f, "\n* Transitions:")?;
        #[cfg(feature = "ui_tui")]
        self.confs
            .iter()
            .zip(self.conf2end.iter())
            .try_for_each(|(conf, to)| writeln!(f, "\t{} --> {to}", conf.fmt(&self.interner)))?;
        #[cfg(not(feature = "ui_tui"))]
        self.confs.iter().try_for_each(|conf| writeln!(f, "\t{}", conf.fmt(&self.interner)))?;
        let cnt_adjs = self.adjs.values().map(|adj| adj.to.len()).sum::<usize>();
        writeln!(f, "\n* #adjs={}, #confs={}", cnt_adjs, self.confs.len())
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
            cps.adjs.iter().for_each(|((orient, from), adj)| {
                adj.to.iter().for_each(|to| {
                    let orient = *orient;
                    ret.edges[orient as usize]
                        .push((from.fmt(orient, &cps.interner).to_string(), to.fmt(orient, &cps.interner).to_string()))
                })
            });
            for orient in 0..=1 {
                ret.edges[orient].sort();
            }
            cps.confs.iter().for_each(|conf| {
                ret.confs.push(conf.fmt(&cps.interner).to_string());
            });
            ret.confs.sort();
        }
        ret
    }

    pub fn validate(line: &str, max_segment_size: usize) -> Result<()> {
        let (machine, certif) = line.split_once(" ").context("invalid line")?;
        println!("testing: {machine}");

        let machine = Machine::from(machine);
        let certif: CPSCertif = certif.parse()?;

        let cps = CPS::prove(&machine, max_segment_size);
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
        if it.next().contains(&"Result") {
            for orient in 0..=1 {
                let cnt: usize = it.next().context("invalid edges len")?.parse()?;
                for _ in 0..cnt {
                    let mut from_to = [it.next().unwrap().to_string(), it.next().unwrap().to_string()];
                    if orient == 0 {
                        from_to.iter_mut().for_each(|tape| *tape = tape.chars().rev().collect());
                    }
                    let [from, to] = from_to;
                    ret.edges[orient].push((from, to));
                }
                ret.edges[orient].sort();
            }
            let cnt: usize = it.next().context("invalid confs len")?.parse()?;
            for _ in 0..cnt {
                ret.confs.push(it.next().unwrap().to_string());
            }
            ret.confs.sort();
        }

        Ok(ret)
    }
}
