# Changelog

## ibis.insights 0.8

- Function to streamline creation of elevation masks
  ([`create_elevation_mask()`](https://iiasa.github.io/ibis.insights/reference/create_elevation_mask.md)).
- Temporal alignment function added
  ([`align_temporal()`](https://iiasa.github.io/ibis.insights/reference/align_temporal.md)).
- Multiple documentation fixes and pkgdown rendering added.
- [`insights_discount()`](https://iiasa.github.io/ibis.insights/reference/insights_discount.md)
  function now working on age to maturity/full condition
- [`insights_discount()`](https://iiasa.github.io/ibis.insights/reference/insights_discount.md)
  now supports optional `tau` establishment/maturation timescale and
  smoothed-threshold parameterizations.

## ibis.insights 0.7

- New
  [`insights_discount()`](https://iiasa.github.io/ibis.insights/reference/insights_discount.md)
  function for land-use layers with an associated age or maturity
  variable (e.g. forest age). Discounts the effective land-use value
  based on the age variable so that newly established habitat does not
  count at full value immediately.
- [`insights_discount()`](https://iiasa.github.io/ibis.insights/reference/insights_discount.md)
  now lets users specify `target_age`, the age at which habitat reaches
  `target` value (default `0.95`), rather than requiring a direct
  discount rate.
- Progress bars for temporal processing implemented in
  [`insights_fraction()`](https://iiasa.github.io/ibis.insights/reference/insights_fraction.md),
  [`insights_area()`](https://iiasa.github.io/ibis.insights/reference/insights_area.md),
  and
  [`insights_discount()`](https://iiasa.github.io/ibis.insights/reference/insights_discount.md).

## ibis.insights 0.6

- Addition of
  [`insights_area()`](https://iiasa.github.io/ibis.insights/reference/insights_area.md)
  for land use layers that are not fractions.
- Support for flexible summary functions in
  [`insights_summary()`](https://iiasa.github.io/ibis.insights/reference/insights_summary.md).

## ibis.insights 0.5

- Helper functions to allow creating a derivate range from a given
  variable
  [`create_derivate_range()`](https://iiasa.github.io/ibis.insights/reference/create_derivate_range.md)
- Function to ‘clamp’ a given raster to a value range via
  [`st_clamp()`](https://iiasa.github.io/ibis.insights/reference/st_clamp.md).
- Parameter in
  [`insights_fraction()`](https://iiasa.github.io/ibis.insights/reference/insights_fraction.md)
  to automatically clamp.

## ibis.insights 0.4

- Bug fixes and more support for future clipping when `stars` is
  provided.

## ibis.insights 0.3

- Full ibis.iSDM scenario support

## ibis.insights 0.2

- Support of stars objects for suitable habitat and refinements

## ibis.insights 0.1

- Initial upload and basic functions
