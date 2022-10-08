// adapted from https://github.com/CAD97/strena/blob/main/src/lib.rs

use core::{
    hash::{BuildHasher, Hash, Hasher},
    ops::{Index, Range},
};
use hashbrown::hash_map::{HashMap, RawEntryMut};
use std::fmt;

use crate::machine::Orientation;

macro_rules! index_unchecked {
    ($place:expr, $index:expr) => {
        if cfg!(debug_assertions) { &$place[$index] } else { $place.get_unchecked($index) }
    };
}

#[inline]
pub fn make_hash(builder: &impl BuildHasher, hashee: &(impl ?Sized + Hash)) -> u64 {
    let state = &mut builder.build_hasher();
    hashee.hash(state);
    state.finish()
}

#[repr(transparent)]
#[derive(Debug, Copy, Clone)]
struct Opaque<T>(T);

type Idx = u32;
const IDX_MAX: usize = core::u32::MAX as usize;

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct ITape {
    base: Idx,
    len: u16,
}

impl ITape {
    #[inline(always)]
    fn ix(self) -> Range<usize> {
        self.base as usize..self.base as usize + self.len as usize
    }

    #[inline(always)]
    pub fn empty() -> ITape {
        ITape { base: 0, len: 0 }
    }

    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn ifmt<'a, T>(self, orient: Orientation, interner: &'a InternerTape<T>) -> TapeFmt<'a, T> {
        TapeFmt { itape: self, orient, interner }
    }
}

impl Default for ITape {
    fn default() -> Self {
        ITape::empty()
    }
}

#[derive(Clone)]
pub struct InternerTape<T, S = ahash::RandomState> {
    hasher: S,
    tape_to_itape: HashMap<Opaque<ITape>, (), ()>, // not HashSet so we can use raw_entry API
    itape_to_tape: Vec<T>,
}

impl<T: Hash + PartialEq + Clone> InternerTape<T> {
    /// Creates a new empty interner.
    #[inline]
    pub fn new() -> Self {
        let mut ret = InternerTape {
            hasher: Default::default(),
            tape_to_itape: HashMap::with_hasher(()),
            itape_to_tape: Vec::new(),
        };
        ret.get_or_insert(&[]);
        ret
    }
}

impl<T, S> InternerTape<T, S> {
    /// The number of uniquely interned tapes.
    #[inline]
    pub fn len(&self) -> usize {
        self.tape_to_itape.len()
    }

    /// Returns true if the interner has no elements.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.tape_to_itape.is_empty()
    }

    #[inline]
    pub fn size(&self) -> usize {
        self.itape_to_tape.len()
    }

    #[inline]
    pub fn resolve(&self, s: ITape) -> Option<&[T]> {
        unsafe { Some(index_unchecked!(self.itape_to_tape, s.ix())) }
    }
}

impl<T, S> Index<ITape> for InternerTape<T, S> {
    type Output = [T];
    #[inline]
    fn index(&self, s: ITape) -> &[T] {
        unsafe { index_unchecked!(self.itape_to_tape, s.ix()) }
    }
}

impl<T: Hash + PartialEq + Clone, S: BuildHasher> InternerTape<T, S> {
    /// Gets the interned itape for this tape,
    /// but does not insert it if it is missing.
    #[inline]
    pub fn get(&self, s: &[T]) -> Option<ITape> {
        let InternerTape { hasher, tape_to_itape, itape_to_tape } = self;

        let hash = make_hash(hasher, s);
        let entry = tape_to_itape
            .raw_entry()
            .from_hash(hash, |&Opaque(symbol)| s == unsafe { index_unchecked!(itape_to_tape, symbol.ix()) });

        entry.map(|(&Opaque(symbol), &())| symbol)
    }

    #[inline]
    pub fn get_or_insert(&mut self, s: &[T]) -> ITape {
        let InternerTape { hasher, tape_to_itape, itape_to_tape } = self;

        let hash = make_hash(hasher, s);
        let entry = tape_to_itape
            .raw_entry_mut()
            .from_hash(hash, |&Opaque(symbol)| s == unsafe { index_unchecked!(itape_to_tape, symbol.ix()) });

        let (&mut Opaque(symbol), &mut ()) = match entry {
            RawEntryMut::Occupied(entry) => entry.into_key_value(),
            RawEntryMut::Vacant(entry) => {
                let symbol = {
                    if itape_to_tape.len().checked_add(s.len()).map(|end| end > IDX_MAX).unwrap_or(true) {
                        panic!("InternerTape overflowed")
                    }
                    let base = itape_to_tape.len() as Idx;
                    let len = s.len() as u16;
                    itape_to_tape.extend_from_slice(s);
                    ITape { base, len }
                };

                entry.insert_with_hasher(hash, Opaque(symbol), (), |&Opaque(symbol)| {
                    let s = unsafe { index_unchecked!(itape_to_tape, symbol.ix()) };
                    make_hash(hasher, s)
                })
            }
        };
        symbol
    }
}

pub trait InternedDisplay: Sized {
    type I;
    fn fmt(&self, interner: &InternerTape<Self::I>, f: &mut fmt::Formatter<'_>) -> fmt::Result;
    fn ifmt<'a>(&'a self, interner: &'a InternerTape<Self::I>) -> InternedFmt<'a, Self, Self::I> {
        InternedFmt { obj: self, interner }
    }
}

pub struct InternedFmt<'a, T, I> {
    pub obj: &'a T,
    pub interner: &'a InternerTape<I>,
}

impl<'a, T: InternedDisplay> fmt::Display for InternedFmt<'a, T, T::I> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.obj.fmt(self.interner, f)
    }
}

pub struct TapeFmt<'a, I> {
    itape: ITape,
    orient: Orientation,
    interner: &'a InternerTape<I>,
}

impl<'a, I: fmt::Display> fmt::Display for TapeFmt<'a, I> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let it = self.interner[self.itape].iter();
        let mut it: Box<dyn Iterator<Item = _>> = if self.orient == 0 { Box::new(it) } else { Box::new(it.rev()) };
        it.try_for_each(|symbol| write!(f, "{}", symbol))
    }
}
