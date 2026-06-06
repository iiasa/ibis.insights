# Symmetric relative difference function

Computes the symmetric relative difference of a numeric vector with
respect to its first element. Unlike the standard relative change
([`relChange`](https://iiasa.github.io/ibis.insights/reference/relChange.md)),
this metric is bounded in \\\[-1, 1\]\\ and remains well-defined when
the baseline value is small.

## Usage

``` r
relChangeSym(v)
```

## Arguments

- v:

  A [`numeric`](https://rdrr.io/r/base/numeric.html) vector with length
  greater than 1 and first element \> 0.

## Value

A [`numeric`](https://rdrr.io/r/base/numeric.html) vector of symmetric
relative differences, bounded in \\\[-1, 1\]\\. Returns `NA` where the
denominator \\x_t + x_0 = 0\\.

## Details

The symmetric relative difference is defined as:

\$\$D\_{sym}(t) = \frac{x_t - x_0}{x_t + x_0}\$\$

This formulation is preferred over the standard relative change when:

- The baseline value \\x_0\\ is small, causing the standard formula to
  produce arbitrarily large values (common for rare species or freshly
  colonised habitat).

- Symmetric treatment of gains and losses is required: a change from
  \\a\\ to \\b\\ has the same magnitude (opposite sign) as from \\b\\ to
  \\a\\.

- A bounded, directly comparable index across species or regions with
  very different baseline areas is needed.

## Author

Martin Jung

## Examples

``` r
x <- c(20, 6, 2, 1, 15, 25)
relChangeSym(x)
#> [1]  0.0000000 -0.5384615 -0.8181818 -0.9047619 -0.1428571  0.1111111
```
