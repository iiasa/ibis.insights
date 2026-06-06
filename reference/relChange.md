# Relative change function

This function calculates the relative change of a vector, taking the
first value as a reference value.

## Usage

``` r
relChange(v, fac = 100)
```

## Arguments

- v:

  A [`numeric`](https://rdrr.io/r/base/numeric.html) vector with length
  greater than 1.

- fac:

  A [`numeric`](https://rdrr.io/r/base/numeric.html) constant multiplier
  on the resulting metric.

## Value

A [`numeric`](https://rdrr.io/r/base/numeric.html) vector.

## Author

Martin Jung

## Examples

``` r
# Example vector
x <- c(20,6,2,1,15,25)
relChange(x)
#> [1]   0 -70 -90 -95 -25  25
```
