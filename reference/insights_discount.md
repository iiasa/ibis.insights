# Apply temporal discount to land-use layers based on an age variable

This function applies a temporal discount to land-use or habitat layers
based on a corresponding age or maturity variable. The age variable is
always connected to the layer being discounted (e.g. forest age linked
to forest fraction) and represents increasing habitat value over time.

Newly established habitat (low age) does not provide full habitat value.
Three maturity parameterizations are supported:

**Target-age parameterization** (default): use this when the available
information is the age at which a land-use class should reach a chosen
fraction of its full habitat value. The `target_age` parameter is the
age at which habitat reaches `target` of its full value:

\$\$H\_{\mathrm{eff}} = H \times \[1 - (1 - p)^{a / a_p}\]\$\$

where \\H\\ is the land-use value, \\a\\ is the cell age, \\a_p\\ is
`target_age`, and \\p\\ is `target`. Internally, this is equivalent to
deriving the per-age discount rate:

\$\$d = 1 - (1 - p)^{1 / a_p}\$\$

and applying:

\$\$H\_{\mathrm{eff}} = H \times \[1 - (1 - d)^a\]\$\$

This requires a land-use layer and an age or maturity layer for the same
land-use class, with `age` and `target_age` in the same time unit.

**Timescale parameterization**: use this when the input age represents
habitat age or time since transition into suitable land use, and a
species- or group-specific establishment/maturation timescale is
available as `tau`:

\$\$Q = 1 - \exp(-a / \tau)\$\$

\$\$H\_{\mathrm{eff}} = H \times Q\$\$

This requires a habitat or suitable land-use layer, a matching
`habitat_age` layer supplied as `age`, and `tau` in the same time unit
as `age`. When `tau` is supplied, `target_age` and `target` are ignored.
The target-age and timescale parameterizations describe the same curve
family, with \\\tau = -a_p / \log(1 - p)\\.

**Smoothed-threshold parameterization**: use this when establishment
quality should transition smoothly around a threshold age. Supply `a50`,
the habitat age at which establishment quality reaches `0.5`, and `k`,
the slope or steepness of the transition:

\$\$Q\_{i,t} = \frac{1}{1 + \exp\[-k_s(a\_{i,t} - a\_{50,s})\]}\$\$

\$\$H\_{\mathrm{eff}} = H \times Q\$\$

This requires a habitat or suitable land-use layer, a matching
habitat-age or time-since-transition layer supplied as `age`, `a50` in
the same time unit as `age`, and `k` in the inverse of that time unit.
When `a50` and `k` are supplied, `target_age` and `target` are ignored.
The `tau` and smoothed-threshold parameterizations are mutually
exclusive.

- In the target-age and timescale forms, at `age = 0`: the factor is `0`
  – no habitat value for brand-new land-use.

- In the target-age form, at `age = target_age`: the factor is `target`,
  e.g. `0.95` by default.

- In the timescale form, at `age = tau`: the factor is `1 - exp(-1)`,
  about `0.63`.

- In the smoothed-threshold form, at `age = a50`: the factor is `0.5`;
  `k` controls how abruptly values move from low to high.

- As `age` increases: the factor approaches `1` – mature habitat reaches
  full value.

## Usage

``` r
insights_discount(
  lu,
  age,
  target_age = 20,
  target = 0.95,
  tau = NULL,
  a50 = NULL,
  k = NULL
)

# S4 method for class 'SpatRaster,SpatRaster'
insights_discount(lu,age,target_age,target,tau,a50,k)

# S4 method for class 'SpatRaster,stars'
insights_discount(lu,age,target_age,target,tau,a50,k)

# S4 method for class 'stars,SpatRaster'
insights_discount(lu,age,target_age,target,tau,a50,k)

# S4 method for class 'stars,stars'
insights_discount(lu,age,target_age,target,tau,a50,k)
```

## Arguments

- lu:

  A
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  or temporal
  [`stars`](https://r-spatial.github.io/sf/reference/stars.html) object
  of the land-use variable (e.g. forest fraction or area). Can be single
  or multi-layer.

- age:

  A
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  or temporal
  [`stars`](https://r-spatial.github.io/sf/reference/stars.html) object
  of the corresponding age or maturity variable (values `>= 0`). Must
  match `lu` in number of layers / time steps.

- target_age:

  A single positive [`numeric`](https://rdrr.io/r/base/numeric.html) age
  at which habitat reaches `target` of full value. Used only when
  `tau = NULL` and `a50 = NULL`. Default: `20`.

- target:

  A single [`numeric`](https://rdrr.io/r/base/numeric.html) target
  maturity value strictly between `0` and `1`. Used only when
  `tau = NULL` and `a50 = NULL`. Default: `0.95`.

- tau:

  Optional single positive
  [`numeric`](https://rdrr.io/r/base/numeric.html)
  establishment/maturation timescale. If supplied, the maturity factor
  is calculated as `1 - exp(-age / tau)` and `target_age` / `target` are
  ignored.

- a50:

  Optional single non-negative
  [`numeric`](https://rdrr.io/r/base/numeric.html) threshold age at
  which establishment quality reaches `0.5`. Must be supplied together
  with `k`. Mutually exclusive with `tau`.

- k:

  Optional single positive
  [`numeric`](https://rdrr.io/r/base/numeric.html) slope or steepness
  parameter for the smoothed-threshold parameterization. Must be
  supplied together with `a50`.

## Value

A discounted version of `lu` in the same format as the input.

## Author

Martin Jung

## Examples

``` r
require(terra)
# Load package example rasters
range <- terra::rast(system.file(
  "extdata/example_range.tif", package = "ibis.insights", mustWork = TRUE
))
lu <- terra::rast(system.file(
  "extdata/Grassland.tif", package = "ibis.insights", mustWork = TRUE
))
lu <- lu / 10000

# Use sparse vegetation as a simple proxy for habitat age/maturity.
# In real applications, use an age or maturity layer for the same land-use class.
age <- terra::rast(system.file(
  "extdata/Grassland.tif", package = "ibis.insights", mustWork = TRUE
))
age <- age / 10000
age <- age * 20

# Specify that habitat reaches 95% of full value at age 20.
lu_discounted <- insights_discount(lu, age, target_age = 20, target = 0.95)
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
# Alternative timescale form if tau is known directly:
lu_discounted_tau <- insights_discount(lu, age, tau = 20)
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
# Smoothed-threshold form:
lu_discounted_smooth <- insights_discount(lu, age, a50 = 10, k = 0.4)
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
out1 <- insights_fraction(range = range, lu = lu)
out2 <- insights_fraction(range = range, lu = lu_discounted)
op <- graphics::par(mfrow = c(1, 2))
terra::plot(out1, main = "Original grassland")
terra::plot(out2, main = "Discounted grassland")

graphics::par(op)
```
