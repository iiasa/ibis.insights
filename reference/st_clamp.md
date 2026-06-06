# Clamp raster values to specific bounds

Clamp all cell values in a
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
or [`stars`](https://rdrr.io/r/graphics/stars.html) object to a provided
numeric interval. Values below the lower bound are set to the lower
bound, and values above the upper bound are set to the upper bound.
Missing values are preserved.

This is useful for keeping suitability, fractional land-use, or habitat
condition layers inside valid ranges before they are passed to functions
such as
[`insights_fraction()`](https://iiasa.github.io/ibis.insights/reference/insights_fraction.md).

## Usage

``` r
st_clamp(env, lb = -Inf, ub = Inf, lower = lb, upper = ub)
```

## Arguments

- env:

  A
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  or [`stars`](https://r-spatial.github.io/sf/reference/stars.html)
  object.

- lb:

  A single [`numeric`](https://rdrr.io/r/base/numeric.html) lower bound
  for clamping. Defaults to `-Inf`, which leaves the lower tail
  unchanged.

- ub:

  A single [`numeric`](https://rdrr.io/r/base/numeric.html) upper bound
  for clamping. Defaults to `Inf`, which leaves the upper tail
  unchanged.

- lower:

  Alias for `lb`.

- upper:

  Alias for `ub`.

## Value

An object of the same class as `env`, with values clamped to `[lb, ub]`.

## Details

For
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
inputs, `st_clamp()` delegates to
[`terra::clamp()`](https://rspatial.github.io/terra/reference/clamp.html).
For [`stars`](https://r-spatial.github.io/sf/reference/stars.html)
inputs, each attribute array is clamped directly with
[`pmax()`](https://rdrr.io/r/base/Extremes.html) and
[`pmin()`](https://rdrr.io/r/base/Extremes.html), preserving the
original dimensions and attribute names.

## Author

Martin Jung

## Examples

``` r
require(terra)
r <- terra::rast(nrow = 2, ncol = 2, vals = c(-0.2, 0.4, 1.2, NA))
st_clamp(r, lower = 0, upper = 1)
#> class       : SpatRaster
#> size        : 2, 2, 1  (nrow, ncol, nlyr)
#> resolution  : 180, 90  (x, y)
#> extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (CRS84) (OGC:CRS84)
#> source(s)   : memory
#> name        : lyr.1
#> min value   :     0
#> max value   :     1

require(stars)
#> Loading required package: stars
#> Loading required package: abind
#> Loading required package: sf
#> Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE
s <- stars::st_as_stars(r)
st_clamp(s, lower = 0, upper = 1)
#> stars object with 2 dimensions and 1 attribute
#> attribute(s):
#>        Min. 1st Qu. Median      Mean 3rd Qu. Max. NAs
#> lyr.1     0     0.2    0.4 0.4666667     0.7    1   1
#> dimension(s):
#>   from to offset delta         refsys x/y
#> x    1  2   -180   180 WGS 84 (CRS84) [x]
#> y    1  2     90   -90 WGS 84 (CRS84) [y]
```
