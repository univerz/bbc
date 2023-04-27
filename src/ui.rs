use std::fmt;

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

pub trait Display1<P1>: Sized {
    fn fmt(&self, p1: P1, f: &mut fmt::Formatter<'_>) -> fmt::Result;

    #[inline(never)]
    fn dsp<'a>(&'a self, p1: P1) -> Dsp1<'a, Self, P1> {
        Dsp1 { obj: self, p1 }
    }
}

pub struct Dsp1<'a, T, P1> {
    pub obj: &'a T,
    pub p1: P1,
}

impl<'a, P1: Copy, T: Display1<P1>> fmt::Display for Dsp1<'a, T, P1> {
    #[inline(never)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.obj.fmt(self.p1, f)
    }
}

pub trait Display2<P1, P2>: Sized {
    fn fmt(&self, p1: P1, p2: P2, f: &mut fmt::Formatter<'_>) -> fmt::Result;

    #[inline(never)]
    fn dsp<'a>(&'a self, p1: P1, p2: P2) -> Dsp2<'a, Self, P1, P2> {
        Dsp2 { obj: self, p1, p2 }
    }
}

pub struct Dsp2<'a, T, P1, P2> {
    pub obj: &'a T,
    pub p1: P1,
    pub p2: P2,
}

impl<'a, P1: Copy, P2: Copy, T: Display2<P1, P2>> fmt::Display for Dsp2<'a, T, P1, P2> {
    #[inline(never)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.obj.fmt(self.p1, self.p2, f)
    }
}
