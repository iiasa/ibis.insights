# Apply InSiGHTS with continuous areal land-use data

Apply an area-of-habitat (AOH) refinement to a binary or fractional
range estimate using land-use or habitat layers already expressed as
area per cell. Use this method when `lu` values are continuous areal
units such as `km2`. For cell shares or suitability weights in `[0, 1]`,
use
[`insights_fraction()`](https://iiasa.github.io/ibis.insights/reference/insights_fraction.md)
instead.

If `lu` has multiple layers or attributes, they are summed before being
applied to the range. Optional `other` layers can be supplied as
additional suitability or condition masks, but they must already be
scaled to `[0, 1]`; raw environmental layers such as elevation should be
converted to a suitability mask before calling this function.

Raster inputs are reprojected, cropped, and resampled to the range
geometry when needed. Temporal
[`stars`](https://rdrr.io/r/graphics/stars.html) inputs may use a `time`
or `Time` dimension name. If `outfile` is supplied, the extension is
adjusted to `.tif` for raster output or `.nc` for
[`stars`](https://rdrr.io/r/graphics/stars.html) output.

## Usage

``` r
insights_area(range, lu, other, outfile = NULL)

# S4 method for class 'SpatRaster,SpatRaster'
insights_area(range,lu,other,outfile)

# S4 method for class 'SpatRaster,stars'
insights_area(range,lu,other,outfile)

# S4 method for class 'stars,stars'
insights_area(range,lu,other,outfile)

# S4 method for class 'stars,SpatRaster'
insights_area(range,lu,other,outfile)

# S4 method for class 'ANY,ANY'
insights_area(range,lu,other,outfile)
```

## Arguments

- range:

  A
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  or temporal [`stars`](https://rdrr.io/r/graphics/stars.html) object
  describing the estimated distribution of a biodiversity feature (e.g.
  species). Values must be binary or fractional in `[0, 1]`.
  Alternatively a `DistributionModel` fitted with `ibis.iSDM` package
  can be supplied.

- lu:

  A
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  or temporal [`stars`](https://rdrr.io/r/graphics/stars.html) object of
  the future land-use areas to be applied to the range. **Each layer has
  to be in area units and greater than or equal to 0.** Multi-layer
  inputs are summed.

- other:

  Optional
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  or temporal [`stars`](https://rdrr.io/r/graphics/stars.html) object
  describing additional suitable conditions for the species. Values must
  already be suitability weights in `[0, 1]`.

- outfile:

  A writeable [`character`](https://rdrr.io/r/base/character.html) of
  where the output should be written to. If missing, the function will
  return a
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  or [`stars`](https://rdrr.io/r/graphics/stars.html) object
  respectively. Missing `.tif` or `.nc` extensions are added as needed.

## Value

Either a
[`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
or temporal [`stars`](https://rdrr.io/r/graphics/stars.html) object or
nothing if outputs are written directly to drive.

## Note

This function does not infer species-habitat relationships from raw
land-use classes. Select, weight, or transform land-use and condition
layers before calling `insights_area()`. *No checks are conducted on
whether supplied area layers add up to the area of a grid cell.*

## References

- Rondinini, Carlo, and Piero Visconti. "Scenarios of large mammal loss
  in Europe for the 21st century." Conservation Biology 29, no. 4
  (2015): 1028-1036.

- Visconti, Piero, Michel Bakkenes, Daniele Baisero, Thomas Brooks,
  Stuart HM Butchart, Lucas Joppa, Rob Alkemade et al. "Projecting
  global biodiversity indicators under future development scenarios."
  Conservation Letters 9, no. 1 (2016): 5-13.

## Author

Martin Jung

## Examples

``` r
require(terra)
range <- terra::rast(system.file(
  "extdata/example_range.tif", package = "ibis.insights", mustWork = TRUE
))
lu_fraction <- terra::rast(system.file(
  "extdata/Grassland.tif", package = "ibis.insights", mustWork = TRUE
)) / 10000

# Convert a fractional land-use layer to area per cell in km2.
lu_km2 <- lu_fraction * terra::cellSize(lu_fraction, unit = "km")
out <- insights_area(range = range, lu = lu_km2)

# The output is already in area units, so do not multiply by cell area again.
insights_summary(out, toArea = FALSE)
#>   time suitability  unit
#> 1   NA    37341.62 input
```
