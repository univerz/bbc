#![feature(core_intrinsics)]
#![feature(iter_advance_by)]
#![feature(let_chains)]
#![feature(let_else)]
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
