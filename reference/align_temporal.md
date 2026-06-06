# Harmonize and align temporal raster layers

Align the temporal dimension of a source raster-like object to the time
steps of a target object. For each target time step, `align_temporal()`
selects the most recent source time step that is less than or equal to
it. Target time steps earlier than the first source time step use the
first source layer.

Before alignment, time coordinates are harmonized to integer calendar
years. This supports common `stars` time dimensions stored as `Date`,
`POSIXct`, numeric day offsets from 1970-01-01, or `units` values such
as `"days since 1970-01-01"`. For
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
objects, the output time is written with `tstep = "years"`. For `stars`
objects, the `time` dimension values are returned as `units` in
`"years"`.

## Usage

``` r
align_temporal(source, target, unit = "years")

# S4 method for class 'SpatRaster,SpatRaster'
align_temporal(source,target,unit)

# S4 method for class 'stars,stars'
align_temporal(source,target,unit)

# S4 method for class 'ANY,ANY'
align_temporal(source,target,unit)
```

## Arguments

- source:

  A
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  or [`stars`](https://rdrr.io/r/graphics/stars.html) object providing
  the values to align.

- target:

  A
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  or [`stars`](https://rdrr.io/r/graphics/stars.html) object providing
  the target time steps. It must be the same object type as `source`.

- unit:

  A single `character` value describing the harmonized time unit.
  Currently only `"years"` is supported.

## Value

An object of the same class as `source`, with temporal layers selected
from `source` and time coordinates matching the harmonized target time
steps.

## Details

`align_temporal()` is implemented as an S4 generic with methods for
`SpatRaster,SpatRaster` and `stars,stars` inputs. It is useful when a
range or environmental raster is available at coarser time steps than a
land-use or scenario raster. The function implements a "previous value
carried forward" alignment: a target year of 2035 will use a source
layer from 2030 when the next source layer is 2040.

For `stars` inputs, a time dimension named `Time` is normalized to
`time`. The function does not alter spatial dimensions, attributes,
coordinate reference systems, or cell values beyond selecting/repeating
temporal slices.

## Author

Martin Jung

## Examples

``` r
require(terra)
#> Loading required package: terra
#> terra 1.9.27

source <- terra::rast(nrow = 1, ncol = 1, nlyr = 3, vals = c(10, 20, 30))
terra::time(source) <- as.Date(c("2000-01-01", "2010-01-01", "2020-01-01"))

target <- terra::rast(nrow = 1, ncol = 1, nlyr = 4, vals = 1)
terra::time(target) <- as.Date(c("1995-01-01", "2005-01-01",
                                 "2015-01-01", "2025-01-01"))

align_temporal(source, target)
#> class       : SpatRaster
#> size        : 1, 1, 4  (nrow, ncol, nlyr)
#> resolution  : 360, 180  (x, y)
#> extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (CRS84) (OGC:CRS84)
#> source(s)   : memory
#> names       : lyr.1, lyr.1, lyr.2, lyr.3
#> min values  :    10,    10,    20,    30
#> max values  :    10,    10,    20,    30
#> time (years): 1995-00-00 to 2025-00-00 (4 steps)
```
