#![feature(core_intrinsics)]
#![feature(iter_advance_by)]
#![feature(let_chains)]
#![feature(option_result_contains)]
#![feature(stmt_expr_attributes)]
#![feature(iter_collect_into)]

use std::panic::UnwindSafe;

#[global_allocator]
static ALLOC: jemallocator::Jemalloc = jemallocator::Jemalloc;

// pub mod ctl;
#[allow(unused)]
mod interner;
pub mod machine;
pub mod skelet_cps;
mod ui;

#[derive(Debug, Clone, parse_display::Display, parse_display::FromStr)]
pub enum ProverResult {
    #[display("{}")]
    Halt,
    #[display("{}")]
    Infinite,
    #[display("{}(\"{0}\")")]
    Limit(String),
    #[display("{}(\"{0}\")")]
    Panic(String),
}

impl ProverResult {
    pub fn catch<F: FnOnce() -> R + UnwindSafe, R: Into<ProverResult>>(f: F) -> ProverResult {
        match std::panic::catch_unwind(|| f()) {
            Ok(ret) => ret.into(),
            Err(e) => {
                let err = e
                    .downcast_ref::<&str>()
                    .map(|s| s.to_string())
                    .or_else(|| e.downcast_ref::<String>().cloned())
                    .unwrap_or_default();
                ProverResult::Panic(err)
            }
        }
    }
}

#[derive(Debug, Default)]
pub struct ProverCount {
    pub halt: usize,
    pub infinite: usize,
    pub limit: usize,
    pub panic: usize,
    pub total: usize,
}

impl ProverCount {
    pub fn update(&mut self, state: ProverResult) {
        match state {
            ProverResult::Halt => self.halt += 1,
            ProverResult::Infinite => self.infinite += 1,
            ProverResult::Limit(_) => self.limit += 1,
            ProverResult::Panic(_) => self.panic += 1,
        }
        self.total += 1;
        if self.total & ((1 << 24) - 1) == 0 {
            eprintln!("\t{self:?}");
        }
    }
}
