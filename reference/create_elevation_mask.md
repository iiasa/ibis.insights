# Create an elevation suitability mask

Convert a digital elevation model (DEM) to a suitability mask from a
species-specific preferred elevation interval. Values inside the
preferred interval receive a value of 1. Values outside the interval can
either be set to 0 directly or down-weighted with a linear or negative
exponential cutoff.

## Usage

``` r
create_elevation_mask(
  dem,
  elevation_range,
  cutoff = c("binary", "linear", "negative_exponential", "exponential"),
  tolerance = NULL
)

# S4 method for class 'SpatRaster'
create_elevation_mask(dem,elevation_range,cutoff,tolerance)

# S4 method for class 'stars'
create_elevation_mask(dem,elevation_range,cutoff,tolerance)

# S4 method for class 'ANY'
create_elevation_mask(dem,elevation_range,cutoff,tolerance)
```

## Arguments

- dem:

  A
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  or [`stars`](https://r-spatial.github.io/sf/reference/stars.html)
  object containing elevation values.

- elevation_range:

  A two-element [`numeric`](https://rdrr.io/r/base/numeric.html) vector
  giving the minimum and maximum suitable elevation, in the same units
  as `dem`.

- cutoff:

  A [`character`](https://rdrr.io/r/base/character.html) cutoff type.
  One of `"binary"`, `"linear"`, `"negative_exponential"`, or
  `"exponential"`. `"exponential"` is accepted as an alias for
  `"negative_exponential"`.

- tolerance:

  A single positive [`numeric`](https://rdrr.io/r/base/numeric.html)
  value, in the same units as `dem`, describing the uncertainty distance
  outside `elevation_range`. Required for `"linear"` and
  `"negative_exponential"` cutoffs. It is ignored for `"binary"`.

## Value

An object of the same class, dimensions, and attribute names as `dem`,
with values in `[0, 1]`. Missing elevation values are preserved.

## Details

For soft cutoffs, the threshold distance \\d\\ is the elevation distance
to the closest threshold: \\d = 0\\ inside `elevation_range`, \\min -
x\\ below the lower threshold, and \\x - max\\ above the upper
threshold. The binary cutoff uses direct threshold comparisons and does
not construct an intermediate distance raster. Soft cutoffs evaluate
lower and upper threshold distances only for cells outside the preferred
interval.

The supported cutoffs are:

- `"binary"`: \\1\\ inside the preferred range and \\0\\ outside it.

- `"linear"`: \\max(0, 1 - d / tolerance)\\.

- `"negative_exponential"`: \\exp(-d / tolerance)\\.

## Author

Martin Jung

## Examples

``` r
require(terra)

dem <- terra::rast(
  nrow = 1,
  ncol = 6,
  vals = c(100, 150, 200, 250, 300, 350)
)

create_elevation_mask(dem, elevation_range = c(200, 300))
#> class       : SpatRaster
#> size        : 1, 6, 1  (nrow, ncol, nlyr)
#> resolution  : 60, 180  (x, y)
#> extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (CRS84) (OGC:CRS84)
#> source(s)   : memory
#> name        : lyr.1
#> min value   :     0
#> max value   :     1
create_elevation_mask(
  dem,
  elevation_range = c(200, 300),
  cutoff = "linear",
  tolerance = 100
)
#> class       : SpatRaster
#> size        : 1, 6, 1  (nrow, ncol, nlyr)
#> resolution  : 60, 180  (x, y)
#> extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (CRS84) (OGC:CRS84)
#> source(s)   : memory
#> name        : lyr.1
#> min value   :     0
#> max value   :     1
create_elevation_mask(
  dem,
  elevation_range = c(200, 300),
  cutoff = "negative_exponential",
  tolerance = 100
)
#> class       : SpatRaster
#> size        : 1, 6, 1  (nrow, ncol, nlyr)
#> resolution  : 60, 180  (x, y)
#> extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (CRS84) (OGC:CRS84)
#> source(s)   : memory
#> name        :    lyr.1
#> min value   : 0.367879
#> max value   :        1

require(stars)
#> Loading required package: stars
#> Loading required package: abind
#> Loading required package: sf
#> Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE
create_elevation_mask(stars::st_as_stars(dem), c(200, 300))
#> stars object with 2 dimensions and 1 attribute
#> attribute(s):
#>        Min. 1st Qu. Median Mean 3rd Qu. Max.
#> lyr.1     0       0    0.5  0.5       1    1
#> dimension(s):
#>   from to offset delta         refsys x/y
#> x    1  6   -180    60 WGS 84 (CRS84) [x]
#> y    1  1     90  -180 WGS 84 (CRS84) [y]
```
