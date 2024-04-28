//! This crate provides a bitset abstraction that has a non-allocating
//! fast-and-small case for bitsets containing only bits in the range 0..=63,
//! which it stores in a single u64. For many programs, bitsets are nearly all
//! empty or contain mostly low-numbered bits, but periodically contain higher
//! bits so need to transparently overflow. This is for such programs.
//!
//! I'm sorry for making another bitset crate. Remarkably, despite dozens of
//! bitset crates, I couldn't find one that does this in the simple/fast/obvious
//! way (using plain 1-word ops) and overflows to large bitsets when necessary.
//! There are many fixed-size-only crates, and many variable-size-only crates,
//! and two crates (smallbitvec and id-set) that almost do what I want but make
//! inexplicably poor size or representation choices for the fast-and-small
//! case, making it not-fast or not-small anyways. This crate does the simple /
//! small / fast / obvious thing you'd expect: an enum with a small u64 case and
//! a big vector-backed case.
//!
//! It delegates to `bit_set` for its vector-backed case, which is perfectly
//! fine. Like `bit_set` is does not expose (nor track) a number of "total bits"
//! in the bitset, only the number of _set_ (1-valued) bits. This is usually
//! what you want -- transparently expanding storage to accommodate the set bits
//! and implicitly treating the bitset as containing infinite zeroes past the
//! last set-bit -- but if you want to track a max limit of the zeroes, you can
//! add that yourself by embedding it in a tuple with an extra word.
//!
//! Note that this also means there is no "invert" or "not" operation, since it
//! would produce an infinite set of set-bits. But you usually only want an
//! invert operation in order to perform a "difference" operation, which is
//! provided. And if you really want an inverse-given-some-bounds operation, if
//! you track a max limit of the zeroes, you can initialize a bitset full of
//! set-bits at that limit and take the difference from it.

use bit_set::BitSet;
use bit_vec::BitVec;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Bs {
    Small(u64),
    Big(BitSet),
}

impl Default for Bs {
    fn default() -> Self {
        Bs::Small(0)
    }
}

impl From<BitSet> for Bs {
    fn from(bs: BitSet) -> Self {
        Bs::Big(bs)
    }
}

impl From<&Bs> for Vec<usize> {
    fn from(bs: &Bs) -> Self {
        match bs {
            Bs::Small(s) => {
                let mut v = Vec::new();
                for i in 0..64 {
                    if s & (1 << (63 - i)) != 0 {
                        v.push(i);
                    }
                }
                v
            }
            Bs::Big(b) => b.iter().collect(),
        }
    }
}

impl FromIterator<usize> for Bs {
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = usize>,
    {
        let mut bs = Bs::new();
        for i in iter {
            bs.insert(i);
        }
        bs
    }
}

impl Bs {
    pub fn new() -> Bs {
        Bs::default()
    }

    /// Initializes a bitset with all bits from 0..count set.
    pub fn from_count_of_set_bits(count: usize) -> Bs {
        if count == 0 {
            Bs::Small(0)
        } else if count <= 64 {
            Bs::Small(0xffff_ffff_ffff_ffff_u64.wrapping_shl(64 - count as u32))
        } else {
            Bs::Big(BitSet::from_bit_vec(BitVec::from_elem(count, true)))
        }
    }

    /// Set the bit at the given index.
    pub fn insert(&mut self, bit: usize) {
        match self {
            Bs::Small(ref mut s) => {
                if bit > 63 {
                    *self = Bs::Big(promote(*s));
                    self.insert(bit);
                } else {
                    *s |= 1 << (63 - bit);
                }
            }
            Bs::Big(ref mut b) => {
                b.insert(bit);
            }
        }
    }

    /// Reset the bitset to empty.
    pub fn clear(&mut self) {
        *self = Bs::new();
    }

    /// Shrink the bitset to the smallest number of words needed to represent the
    /// set bits.
    pub fn shrink_to_fit(&mut self) {
        match self {
            Bs::Small(_) => (),
            Bs::Big(b) => {
                b.shrink_to_fit();
            }
        }
    }

    /// Clear the bit at the given index.
    pub fn remove(&mut self, bit: usize) {
        match self {
            Bs::Small(ref mut s) => {
                if bit > 63 {
                    // If we're removing a bit that's beyond the end of the
                    // small bitset, there's nothing to do -- it's already
                    // absent.
                } else {
                    *s &= !(1 << (63 - bit));
                }
            }
            Bs::Big(ref mut b) => {
                b.remove(bit);
            }
        }
    }

    /// Returns true if the bit is set.
    pub fn contains(&self, bit: usize) -> bool {
        match self {
            Bs::Small(s) => {
                if bit > 63 {
                    false
                } else {
                    (s & (1 << (63 - bit))) != 0
                }
            }
            Bs::Big(b) => b.contains(bit),
        }
    }

    /// Sets self to the union of self and other.
    /// I.e. self = self | other.
    pub fn union_with(&mut self, other: &Bs) {
        match (&mut *self, other) {
            (Bs::Small(s), Bs::Small(o)) => {
                *s |= *o;
            }
            (Bs::Small(s), Bs::Big(ref o)) => {
                let mut self_tmp = promote(*s);
                self_tmp.union_with(o);
                *self = Bs::Big(self_tmp);
            }
            (Bs::Big(s), Bs::Small(o)) => {
                s.union_with(&promote(*o));
            }
            (Bs::Big(s), Bs::Big(o)) => {
                s.union_with(o);
            }
        }
    }

    /// Sets self to the intersection of self and other.
    /// I.e. self = self & other.
    pub fn intersect_with(&mut self, other: &Bs) {
        match (&mut *self, other) {
            (Bs::Small(s), Bs::Small(o)) => {
                *s &= *o;
            }
            (Bs::Small(s), Bs::Big(ref o)) => {
                let mut self_tmp = promote(*s);
                self_tmp.intersect_with(o);
                *self = Bs::Big(self_tmp);
            }
            (Bs::Big(s), Bs::Small(o)) => {
                s.intersect_with(&promote(*o));
            }
            (Bs::Big(s), Bs::Big(o)) => {
                s.intersect_with(o);
            }
        }
    }

    /// Sets self to the difference of self and other.
    /// I.e. self = self & !other.
    pub fn difference_with(&mut self, other: &Bs) {
        match (&mut *self, other) {
            (Bs::Small(s), Bs::Small(o)) => {
                *s &= !*o;
            }
            (Bs::Small(s), Bs::Big(ref o)) => {
                let mut self_tmp = promote(*s);
                self_tmp.difference_with(o);
                *self = Bs::Big(self_tmp);
            }
            (Bs::Big(s), Bs::Small(o)) => {
                s.difference_with(&promote(*o));
            }
            (Bs::Big(s), Bs::Big(o)) => {
                s.difference_with(o);
            }
        }
    }

    /// Sets self to the symmetric difference of self and other,
    /// i.e. self = self ^ other.
    pub fn symmetric_difference_with(&mut self, other: &Bs) {
        match (&mut *self, other) {
            (Bs::Small(s), Bs::Small(o)) => {
                *s ^= *o;
            }
            (Bs::Small(s), Bs::Big(ref o)) => {
                let mut self_tmp = promote(*s);
                self_tmp.symmetric_difference_with(o);
                *self = Bs::Big(self_tmp);
            }
            (Bs::Big(s), Bs::Small(o)) => {
                s.symmetric_difference_with(&promote(*o));
            }
            (Bs::Big(s), Bs::Big(o)) => {
                s.symmetric_difference_with(o);
            }
        }
    }

    /// Returns true if the bitset is empty.
    pub fn is_empty(&self) -> bool {
        match self {
            Bs::Small(s) => *s == 0,
            Bs::Big(b) => b.is_empty(),
        }
    }

    /// Returns true if self is a (non-strict) subset of other.
    pub fn is_subset(&self, other: &Bs) -> bool {
        match (self, other) {
            (Bs::Small(s), Bs::Small(o)) => *s & *o == *s,
            (Bs::Small(s), Bs::Big(ref o)) => promote(*s).is_subset(o),
            (Bs::Big(s), Bs::Small(o)) => s.is_subset(&promote(*o)),
            (Bs::Big(s), Bs::Big(o)) => s.is_subset(o),
        }
    }

    /// Returns true if self is a (non-strict) superset of other.
    pub fn is_superset(&self, other: &Bs) -> bool {
        other.is_subset(self)
    }

    /// Returns true if self is disjoint from other.   
    pub fn is_disjoint(&self, other: &Bs) -> bool {
        match (self, other) {
            (Bs::Small(s), Bs::Small(o)) => *s & *o == 0,
            (Bs::Small(s), Bs::Big(ref o)) => promote(*s).is_disjoint(o),
            (Bs::Big(s), Bs::Small(o)) => s.is_disjoint(&promote(*o)),
            (Bs::Big(s), Bs::Big(o)) => s.is_disjoint(o),
        }
    }

    /// Returns the number of bits set in self.
    pub fn len(&self) -> usize {
        match self {
            Bs::Small(s) => s.count_ones() as usize,
            Bs::Big(b) => b.len(),
        }
    }
}

fn promote(small: u64) -> BitSet {
    BitSet::from_bytes(&small.to_be_bytes())
}

#[cfg(test)]
extern crate quickcheck;
#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_promote() {
        let mut n_small = 0;
        for i in 0..256 {
            let mut bs = Bs::new();
            bs.insert(i);
            match bs {
                Bs::Small(s) => {
                    n_small += 1;
                    assert_eq!(promote(s).contains(i), true);
                }
                Bs::Big(b) => {
                    assert_eq!(b.contains(i), true);
                }
            }
        }
        assert_eq!(n_small, 64);
    }

    #[test]
    fn test_from_count_of_set_bits() {
        for i in 0..256 {
            let bs = Bs::from_count_of_set_bits(i);
            for j in 0..i {
                assert!(bs.contains(j));
            }
            for j in i..256 {
                assert!(!bs.contains(j));
            }
            assert_eq!(bs.len(), i);
        }
    }

    #[test]
    fn test_insert() {
        for i in 0..256 {
            let mut bs = Bs::new();
            bs.insert(i);
            assert!(bs.contains(i));
            for j in 0..256 {
                if j != i {
                    assert!(!bs.contains(j));
                }
            }
        }
    }

    #[test]
    fn test_union() {
        for i in 0..256 {
            let mut bs = Bs::new();
            bs.insert(i);
            assert!(bs.contains(i));
            for j in 0..256 {
                let mut bs2 = Bs::new();
                bs2.insert(j);
                bs.union_with(&bs2);
                assert!(bs.contains(i) && bs.contains(j));
            }
        }
    }

    #[test]
    fn test_intersection() {
        for i in 0..256 {
            let mut bs = Bs::new();
            bs.insert(i);
            assert!(bs.contains(i));
            for j in 0..256 {
                let mut bs2 = Bs::new();
                bs2.insert(j);
                bs.union_with(&bs2);
                assert!(bs.contains(i) && bs.contains(j));
            }
        }
    }

    #[quickcheck]
    fn quickcheck_set_ops(a: Vec<u8>, b: Vec<u8>) {
        let au: Vec<usize> = a.iter().cloned().map(|x| x as usize).collect();
        let bu: Vec<usize> = b.iter().cloned().map(|x| x as usize).collect();
        let bs_a: Bs = au.iter().cloned().collect();
        let bs_b: Bs = bu.iter().cloned().collect();

        let mut union = bs_a.clone();
        union.union_with(&bs_b);

        let mut intersection = bs_a.clone();
        intersection.intersect_with(&bs_b);

        let mut difference = bs_a.clone();
        difference.difference_with(&bs_b);

        let mut sym_diff = bs_a.clone();
        sym_diff.symmetric_difference_with(&bs_b);

        eprintln!(
            "{} vs {}, union={}, intersection={}, diff={}, sym_diff={}",
            a.len(),
            b.len(),
            union.len(),
            intersection.len(),
            difference.len(),
            sym_diff.len()
        );

        assert!(union.is_superset(&bs_a));
        assert!(union.is_superset(&bs_b));

        assert!(intersection.is_subset(&bs_a));
        assert!(intersection.is_subset(&bs_b));

        assert!(difference.is_subset(&bs_a));
        assert!(difference.is_disjoint(&bs_b));

        assert!(sym_diff.is_subset(&union));
        assert!(intersection.is_disjoint(&sym_diff));

        for i in au.iter() {
            for j in bu.iter() {
                assert!(union.contains(*i));
                assert!(union.contains(*j));
                if i == j {
                    assert!(union.contains(*i));
                    assert!(intersection.contains(*i));
                    assert!(!difference.contains(*i));
                    assert!(!sym_diff.contains(*i));
                }
                if bs_a.contains(*j) && bs_b.contains(*i) {
                    assert!(intersection.contains(*i));
                    assert!(intersection.contains(*j));
                }
                if !bs_a.contains(*j) && !bs_b.contains(*i) {
                    assert!(sym_diff.contains(*i));
                    assert!(sym_diff.contains(*j));
                }
                assert!(!difference.contains(*j));
                if intersection.contains(*i) {
                    assert!(!difference.contains(*i));
                    assert!(!sym_diff.contains(*i));
                }
            }
        }
    }
}
