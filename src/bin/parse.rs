#![feature(option_result_contains)]
use ahash::HashSet;
use color_eyre::eyre::{Context, ContextCompat, Result};
use hashbrown::HashMap;
use itertools::{iproduct, Itertools};
use owo_colors::OwoColorize;
use std::{
    fs::File,
    io::{BufRead, BufReader},
};

use regex::Regex;

const CNT_CUTOFF: usize = 100;

fn main() -> Result<()> {
    color_eyre::install()?;

    let file = File::open("/home/univerz/projects/rustbb/ultim_new")?;
    let lines = BufReader::new(file).lines();

    let conf_re = Regex::new(r"(?P<steps>[0-9]+):\s(?P<ltape>[^A-Z]*)\s(?P<head>[^\s]+)\s(?P<rtape>[^A-Z]*)")?;

    type Digits = HashMap<String, HashSet<String>>;

    let mut digits: Digits = HashMap::new();
    let exp_re = Regex::new(r"}\^(?P<exp>[^\s\}]{2,}\s)")?;

    for line in lines {
        if line.contains(&"{{") {
            continue;
        }
        let line = line.wrap_err("line")?;
        let caps = conf_re.captures(&line).wrap_err_with(|| format!("invalid configuration"))?;
        // dbg!(caps);
        let tape = &caps["ltape"];
        // dbg!(&tape);

        let mut start = 0;

        for m in exp_re.find_iter(tape) {
            let block = tape[start..(m.start() + 2)].trim_start();
            let exp = &tape[(m.start() + 2)..m.end()];
            start = m.end();

            digits.entry_ref(block).or_default().insert(exp.to_string());

            // println!("{block:?} {exp}");
        }
    }

    println!("\n\nparsed digits:");
    for (digit, exps) in digits.iter() {
        // println!("\t{:?}: {exps:?}", digit.bright_white().bold())
        println!("\t{:?}: {}", digit.bright_white().bold(), exps.len())
    }

    // let keys: Vec<String> = digits.keys().cloned().collect();

    // println!("\n\nprunning:");
    // let digits: Digits = digits
    //     .into_iter()
    //     .filter_map(|(mut digit, exps)| {
    //         while let Some(prefix) = keys.iter().find(|key| digit.starts_with(*key) && digit != **key) {
    //             println!("digit {:?} removing common prefix {:?}", digit.bright_white(), prefix.green());
    //             digit = digit[prefix.len()..]
    //                 .trim_start_matches(|c: char| !c.is_whitespace() /*&& c != '}'*/)
    //                 .wrap_err("unable to strip exp")
    //                 .unwrap()
    //                 .trim_start()
    //                 .to_owned();
    //             let action = if keys.contains(&digit) {
    //                 format!("{}", "alredy in set, pruning".red().bold())
    //             } else {
    //                 String::new()
    //             };
    //             println!("\t -> {:?} // {action}", digit.bright_white().bold())
    //         }
    //         (!digit.is_empty()).then_some((digit, exps))
    //     })
    //     .collect();

    println!("\n\npruned digits:");
    let mut pruned: Vec<(String, char)> = Vec::new();
    let mut tmp: Vec<_> = digits.into_iter().collect();
    tmp.sort_by_key(|(_, exps)| exps.len());
    for (digit, exps) in tmp {
        if exps.len() > CNT_CUTOFF {
            assert!(pruned.len() < u8::MAX as usize);

            let c = (pruned.len() as u8 + b'A') as char;
            println!("{}\t{:?}: {}x", c.bright_green().bold(), digit.bright_white(), exps.len().green());
            pruned.push((digit, c));
        } else {
            println!("\t{:?}: {}x", digit, exps.len());
        }
    }

    let mut confs: HashMap<String, String> = HashMap::new();
    let file = File::open("/home/univerz/projects/rustbb/ultim_new")?;
    let lines = BufReader::new(file).lines();

    for line in lines {
        let line = line.wrap_err("line")?;
        if line.contains("{{") {
            continue;
        }
        // if !line.contains('C') {
        //     continue;
        // }
        let caps = conf_re.captures(&line).wrap_err_with(|| format!("invalid configuration"))?;

        let mut tape = caps["ltape"].trim();
        let mut replaced_tape = String::new();
        // println!("");
        loop {
            if tape.is_empty() {
                break;
            }

            let replaced = pruned.iter().any(|digit| {
                if tape.starts_with(&digit.0) {
                    let ntape =
                        tape[digit.0.len()..].trim_start_matches(|c: char| !c.is_whitespace() && c != '}').trim_start();
                    // if ntape.chars().next().unwrap_or('a').is_digit(10) {
                    //     println!("\n{tape:?}\n\t{:?} ->\nt {ntape:?}\t\n\\{:?}\n", digit.0, &tape[digit.0.len()..]);
                    // }
                    tape = ntape;

                    replaced_tape.push(digit.1);
                    true
                } else {
                    false
                }
            });
            if !replaced {
                // dbg!(&tape);
                // let idx = tape.find(" ").map(|idx| idx + 1).unwrap_or_else(|| tape.len());
                let idx = 1.min(tape.len());
                replaced_tape.push_str(&tape[..idx]);
                tape = &tape[idx..]
            }
        }

        let mut cnt = 0;
        let pos = replaced_tape
            .rfind(|c: char| {
                if c.is_ascii_alphabetic() {
                    cnt += 1;
                }
                // cnt == 3
                cnt == 1
            })
            .unwrap_or(0);
        let idx = format!("{} {}", &replaced_tape[pos..], &caps["head"]);
        let entry = confs.entry_ref(&idx);
        let is_new = matches!(entry, hashbrown::hash_map::EntryRef::Vacant(_));
        entry.or_insert(caps["steps"].to_string());

        println!("{replaced_tape:?}");
        if is_new {
            println!("\tadded {:?}", idx.bright_red().bold());
        }
    }

    // return Ok(());

    dbg!(&confs);

    let lefts: Vec<String> = confs.iter().map(|(c, _)| c[..1].to_string()).unique().collect();
    let rights: Vec<String> = confs.iter().map(|(c, _)| c[1..].to_string()).unique().collect();
    // let edges: Vec<String> = confs.iter().map(|(c, _)| c[1..c.len() - 3].to_string()).unique().collect();
    // let states: Vec<String> = confs.iter().map(|(c, _)| c[c.len() - 2..].to_string()).unique().collect();
    // dbg!((lefts, edges, states));
    for (l, r) in iproduct!(lefts, rights) {
        let idx = format!("{l}{r}");
        if !confs.contains_key(&idx) {
            println!("missing {idx:?}");
        }
    }

    Ok(())
}
