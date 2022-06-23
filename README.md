# bs

This crate provides a bitset abstraction that has a non-allocating
fast-and-small case for bitsets containing only bits in the range 0..=63,
which it stores in a single u64. For many programs, bitsets are nearly all
empty or contain mostly low-numbered bits, but periodically contain higher
bits so need to transparently overflow. This is for such programs.

I'm sorry for making another bitset crate. Remarkably, despite dozens of
bitset crates, I couldn't find one that does this in the simple/fast/obvious
way (using plain 1-word ops) and overflows to large bitsets when necessary.
There are many fixed-size-only crates, and many variable-size-only crates,
and two crates (smallbitvec and id-set) that almost do what I want but make
inexplicably poor size or representation choices for the fast-and-small
case, making it not-fast or not-small anyways. This crate does the simple /
small / fast / obvious thing you'd expect: an enum with a small u64 case and
a big vector-backed case.

It delegates to `bit_set` for its vector-backed case, which is perfectly
fine. Like `bit_set` is does not expose (nor track) a number of "total bits"
in the bitset, only the number of _set_ (1-valued) bits. This is usually
what you want -- transparently expanding storage to accommodate the set bits
and implicitly treating the bitset as containing infinite zeroes past the
last set-bit -- but if you want to track a max limit of the zeroes, you can
add that yourself by embedding it in a tuple with an extra word.

Note that this also means there is no "invert" or "not" operation, since it
would produce an infinite set of set-bits. But you usually only want an
invert operation in order to perform a "difference" operation, which is
provided. And if you really want an inverse-given-some-bounds operation, if
you track a max limit of the zeroes, you can initialize a bitset full of
set-bits at that limit and take the difference from it.

License: MIT OR Apache-2.0
