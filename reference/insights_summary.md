# Summarize inSiGHTS into an index

This function handily summarizes the suitable habitat for a given
species or biodiversity feature into an index. If a single timestep (or
object with a single layer) is provided, this function simply summarizes
the suitable area.

## Usage

``` r
insights_summary(
  obj,
  toArea = TRUE,
  fun = "sum",
  relative = TRUE,
  symmetric = FALSE
)

# S4 method for class 'SpatRaster'
insights_summary(obj,toArea,fun,relative,symmetric)

# S4 method for class 'stars'
insights_summary(obj,toArea,fun,relative,symmetric)
```

## Arguments

- obj:

  A
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  or temporal
  [`stars`](https://r-spatial.github.io/sf/reference/stars.html) object
  with the applied InSiGHTS outputs from `insights_fraction` or
  `insights_area`. If the number of layers is greater than 1, the
  parameter `"relative"` might be applied.

- toArea:

  A [`logical`](https://rdrr.io/r/base/logical.html) flag whether
  fractional suitable habitat should be multiplied by cell area before
  summarizing (Default: `TRUE`). Use `FALSE` for outputs from
  [`insights_area()`](https://iiasa.github.io/ibis.insights/reference/insights_area.md),
  which are already in area units.

- fun:

  A [`character`](https://rdrr.io/r/base/character.html) indicating the
  summary function to be applied (Default: `'sum'`). Currently supported
  are `'sum'`, `'min'`, `'max'`, `'median'` and `'mean'`.

- relative:

  A [`logical`](https://rdrr.io/r/base/logical.html) flag whether a
  relative index is to be constructed (Default: `TRUE`).

- symmetric:

  A [`logical`](https://rdrr.io/r/base/logical.html) flag whether to
  additionally compute the symmetric relative difference (Default:
  `FALSE`). Requires `relative = TRUE`.

## Value

A [`data.frame`](https://rdrr.io/r/base/data.frame.html) with area
estimates or the respective indicator.

## Details

When `relative = TRUE`, the standard relative change (in percent) is
computed as \\D(t) = (x_t - x_0) / x_0 \times 100\\.

When `symmetric = TRUE`, the symmetric relative difference is also
reported as an additional column `relative_change_sym`:

\$\$D\_{sym}(t) = \frac{x_t - x_0}{x_t + x_0}\$\$

This metric is bounded in \\\[-1, 1\]\\ and is preferred over the
standard relative change when the baseline habitat area \\x_0\\ is small
(causing the standard metric to become arbitrarily large), or when a
bounded, symmetric index is needed for cross-species comparisons.
Requires the baseline suitability (\\x_0\\) to be positive.

## References

- Baisero, Daniele, Piero Visconti, Michela Pacifici, Marta Cimatti, and
  Carlo Rondinini. "Projected global loss of mammal habitat due to
  land-use and climate change." One Earth 2, no. 6 (2020): 578-585.

- Powers, Ryan P., and Walter Jetz. "Global habitat loss and extinction
  risk of terrestrial vertebrates under future land-use-change
  scenarios." Nature Climate Change 9, no. 4 (2019): 323-329.

## Author

Martin Jung

## Examples

``` r
require(terra)
range <- terra::rast(system.file(
  "extdata/example_range.tif", package = "ibis.insights", mustWork = TRUE
))
lu <- terra::rast(system.file(
  "extdata/Grassland.tif", package = "ibis.insights", mustWork = TRUE
)) / 10000

out <- insights_fraction(range = range, lu = lu)
insights_summary(out, relative = FALSE)
#>   time suitability unit
#> 1   NA    37341.62  km2

ts <- c(out, out * 0.8, out * 0.6)
terra::time(ts, tstep = "years") <- c(2020, 2040, 2060)
insights_summary(ts, relative = TRUE, symmetric = TRUE)
#>   time suitability unit relative_change_perc relative_change_sym
#> 1 2020       0.000  km2                    0           0.0000000
#> 2 2040   -7468.323  km2                  -20          -0.1111111
#> 3 2060  -14936.646  km2                  -40          -0.2500000
```
