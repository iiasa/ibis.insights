# Apply InSiGHTS with fractional land-use data

Apply an area-of-habitat (AOH) refinement to a binary or fractional
range estimate using land-use or habitat layers expressed as fractions.
Use this method when the land-use values represent cell shares or
suitability weights in the interval `[0, 1]`. For land-use inputs
already expressed as area per cell, use
[`insights_area()`](https://iiasa.github.io/ibis.insights/reference/insights_area.md)
instead.

If `lu` has multiple layers or attributes, they are summed before being
applied to the range. This is useful when several land-use classes
jointly represent suitable habitat. Optional `other` layers can be
supplied as additional suitability or condition masks, but they must
already be scaled to `[0, 1]`; raw environmental layers such as
elevation should be converted to a suitability mask before calling this
function.

Raster inputs are reprojected, cropped, and resampled to the range
geometry when needed. Temporal
[`stars`](https://r-spatial.github.io/sf/reference/stars.html) inputs
may use a `time` or `Time` dimension name. If `outfile` is supplied, the
extension is adjusted to `.tif` for raster output or `.nc` for
[`stars`](https://r-spatial.github.io/sf/reference/stars.html) output.

## Usage

``` r
insights_fraction(range, lu, other, outfile = NULL, clamp = FALSE)

# S4 method for class 'SpatRaster,SpatRaster'
insights_fraction(range,lu,other,outfile,clamp)

# S4 method for class 'SpatRaster,stars'
insights_fraction(range,lu,other,outfile,clamp)

# S4 method for class 'stars,stars'
insights_fraction(range,lu,other,outfile,clamp)

# S4 method for class 'stars,SpatRaster'
insights_fraction(range,lu,other,outfile,clamp)

# S4 method for class 'ANY,ANY'
insights_fraction(range,lu,other,outfile,clamp)
```

## Arguments

- range:

  A
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  or temporal
  [`stars`](https://r-spatial.github.io/sf/reference/stars.html) object
  describing the estimated distribution of a biodiversity feature (e.g.
  species). Values must be binary or fractional in `[0, 1]`.
  Alternatively a `DistributionModel` fitted with `ibis.iSDM` package
  can be supplied.

- lu:

  A
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  or temporal
  [`stars`](https://r-spatial.github.io/sf/reference/stars.html) object
  of the future land-use fractions to be applied to the range. **Each
  layer has to be in fractional units between 0 and 1.** Multi-layer
  inputs are summed.

- other:

  Optional
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  or temporal
  [`stars`](https://r-spatial.github.io/sf/reference/stars.html) object
  describing additional suitable conditions for the species. Values must
  already be suitability weights in `[0, 1]`.

- outfile:

  A writeable [`character`](https://rdrr.io/r/base/character.html) of
  where the output should be written to. If missing, the function will
  return a
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  or [`stars`](https://r-spatial.github.io/sf/reference/stars.html)
  object respectively. Missing `.tif` or `.nc` extensions are added as
  needed.

- clamp:

  A [`logical`](https://rdrr.io/r/base/logical.html) on whether `lu` and
  summed fractional suitability should be clamped to 0 and 1 beforehand
  (Default: `FALSE`).

## Value

Either a
[`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
or temporal
[`stars`](https://r-spatial.github.io/sf/reference/stars.html) object or
nothing if outputs are written directly to drive.

## Note

This function does not infer species-habitat relationships from raw
land-use classes. Select, weight, or transform land-use and condition
layers before calling `insights_fraction()`.

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
# Load package example rasters
range <- terra::rast(system.file(
  "extdata/example_range.tif", package = "ibis.insights", mustWork = TRUE
))
lu <- c(
  terra::rast(system.file(
    "extdata/Grassland.tif", package = "ibis.insights", mustWork = TRUE
  )),
  terra::rast(system.file(
    "extdata/Sparsely.vegetated.areas.tif", package = "ibis.insights", mustWork = TRUE
  ))
)

# Convert example land-use layers to fractions between 0 and 1.
lu <- lu / 10000

out <- insights_fraction(range = range, lu = lu, clamp = TRUE)
head(insights_summary(out))
#>   time suitability unit
#> 1   NA    38374.68  km2
```
