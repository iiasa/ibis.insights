# Recreate a derivative variable based on their range

The purpose of this function is to create the range from several
derivative variables. The information to do so is taken from the
variable name and it is assumed that those have been created by
[`ibis.iSDM::predictor_derivate()`](https://iiasa.github.io/ibis.iSDM/reference/predictor_derivate.html)
function.

This function return the range of values from the original data that
fall within the set of coefficients. Currently only positive
coefficients are taken by default.

## Usage

``` r
create_derivate_range(env, varname, co, to_binary = FALSE)
```

## Arguments

- env:

  The original variable stacks as
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  or [`stars`](https://rdrr.io/r/graphics/stars.html).

- varname:

  A [`character`](https://rdrr.io/r/base/character.html) of the variable
  name. Needs to be present in `"env"`.

- co:

  A set of coefficients obtained via
  [`stats::coef()`](https://rdrr.io/r/stats/coef.html) and a
  [`ibis.iSDM::BiodiversityDistribution`](https://iiasa.github.io/ibis.iSDM/reference/BiodiversityDistribution-class.html)
  object.

- to_binary:

  A [`logical`](https://rdrr.io/r/base/logical.html) flag if the output
  should be converted to binary format or left in the original units
  (Default: `FALSE`).

## Value

A
[`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
object containing the predictor range.

## Details

- This function really only makes sense for `'bin'`, `'thresh'` and
  `'hinge'` transformations.

- For `'hinge'` the combined `min` is returned.

## Note

This is rather an internal function created for a specific use and
project.

## Author

Martin Jung

## Examples

``` r
# Toy temperature layer and coefficients for a binned derivative
env <- terra::rast(nrows = 2, ncols = 2, vals = c(5, 15, 25, 10))
names(env) <- "Temperature"
co <- data.frame(Feature = c("(Intercept)", "bin_Temperature_10_20"),
  Beta = c(0, 1))

# Range of values covered by the positive coefficient (10 to 20)
ibis.insights:::create_derivate_range(env, varname = "Temperature", co = co)
#> class       : SpatRaster
#> size        : 2, 2, 1  (nrow, ncol, nlyr)
#> resolution  : 180, 90  (x, y)
#> extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (CRS84) (OGC:CRS84)
#> source(s)   : memory
#> name        : Temperature
#> min value   :           0
#> max value   :          15
# Same range converted to a binary layer
ibis.insights:::create_derivate_range(env, varname = "Temperature", co = co,
  to_binary = TRUE)
#> class       : SpatRaster
#> size        : 2, 2, 1  (nrow, ncol, nlyr)
#> resolution  : 180, 90  (x, y)
#> extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (CRS84) (OGC:CRS84)
#> source(s)   : memory
#> name        : Temperature
#> min value   :           0
#> max value   :           1
```
