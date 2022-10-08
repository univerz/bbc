#[cfg(feature = "ui_tui")]
mod tui {
    #[macro_export]
    macro_rules! ui_dbg {
        ($($arg:tt)*) => {
            println!($($arg)*);
        }
    }
}

#[cfg(all(not(feature = "ui_tui")))]
mod noui {
    #[macro_export]
    macro_rules! ui_dbg {
        ($($arg:tt)*) => {};
    }
}
