# Apply an elevation suitability mask to a species projection

Align an elevation suitability mask to a species projection and multiply
the species values by the mask. The output follows the class, geometry,
layer count, and temporal dimension of `species_projection`.

## Usage

``` r
apply_elevation_mask(elevation_mask, species_projection)
```

## Arguments

- elevation_mask:

  A
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  or [`stars`](https://rdrr.io/r/graphics/stars.html) object containing
  suitability weights in `[0, 1]`, typically from
  [`create_elevation_mask()`](https://iiasa.github.io/ibis.insights/reference/create_elevation_mask.md).

- species_projection:

  A
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  or [`stars`](https://rdrr.io/r/graphics/stars.html) species projection
  to be masked.

## Value

A masked species projection with the same class as `species_projection`.

## Details

The mask is reprojected, cropped, and resampled to the species grid when
needed. Static masks are repeated across temporal species projections.
When both inputs are same-class temporal objects, mask time steps are
aligned to the species projection with
[`align_temporal()`](https://iiasa.github.io/ibis.insights/reference/align_temporal.md).

## Author

Martin Jung

## Examples

``` r
require(terra)
species <- terra::rast(nrow = 1, ncol = 3, vals = c(0.2, 0.6, 1))
mask <- terra::rast(nrow = 1, ncol = 3, vals = c(0, 0.5, 1))
apply_elevation_mask(mask, species)
#> class       : SpatRaster
#> size        : 1, 3, 1  (nrow, ncol, nlyr)
#> resolution  : 120, 180  (x, y)
#> extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (CRS84) (OGC:CRS84)
#> source(s)   : memory
#> name        : lyr.1
#> min value   :     0
#> max value   :     1
```
