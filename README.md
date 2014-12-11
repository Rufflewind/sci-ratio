sci-ratio [![Build status][ci]][ca]
=================================

**Quick links:** [installation](#installation), [documentation][doc],
[versioning policy][pvp].

This package provides utilities for parsing and pretty-printing exact rational
numbers with arbitrarily large exponents.  Numbers are represented using the
`SciRatio a b` type, which can be thought of as a rational number combined
with a power of ten:

```
r .^ e == r * 10 ^^ e
```

The primary advantage of this data type is that it's capable of storing
extremely large exponents without issues, although *performing arithmetic* on
them is not guaranteed to be efficient in some cases (e.g. adding numbers with
wildly differing exponents).

The parsers accept a wide variety of formats:

```hs
> readSciRational "-0.0e+3"         -- result: Just ((0 % 1) .^ 0)
> readSciRational "0.25e+2"         -- result: Just ((25 % 1) .^ 0)
> readSciRational "-1.0e-1"         -- result: Just (((-1) % 1) .^ (-1))
> readSciRational "5.0e+20/6.e0"    -- result: Just ((25 % 3) .^ 19)
> readSciRational "0xfeedface"      -- result: Just ((4277009102 % 1) .^ 0)
> readSciRational "1e99999999"      -- result: Just ((1 % 1) .^ 99999999)
```

Numbers can be pretty-printed in a compact format:

```hs
> showSciRational (-0.0e+3)         -- result: "0"
> showSciRational (0.25e+2)         -- result: "25"
> showSciRational (-1.0e-1)         -- result: "-.1"
> showSciRational (5.0e+20 / 6)     -- result: "2.5e20/3"
> showSciRational (0xfeedface)      -- result: "4277009102"
> showSciRational (1 .^ 99999999)   -- result: "1e99999999"
```

Installation
------------

The package is available on [Hackage][doc]:

```sh
cabal install sci-ratio
```

[ca]:  https://travis-ci.org/Rufflewind/sci-ratio
[ci]:  https://travis-ci.org/Rufflewind/sci-ratio.svg?branch=master
[doc]: https://hackage.haskell.org/package/sci-ratio
[pvp]: https://gist.github.com/Rufflewind/03f4e03f7cfa52b8f07d
