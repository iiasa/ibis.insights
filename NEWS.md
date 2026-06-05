# ibis.insights 0.8

* Multiple documentation fixes and pkgdown rendering added.
* `insights_discount()` function now working on age to maturity/full condition

# ibis.insights 0.7

* New `insights_discount()` function for land-use layers with an associated age or maturity variable (e.g. forest age). Discounts the effective land-use value based on the age variable so that newly established habitat does not count at full value immediately.
* `insights_discount()` now lets users specify `target_age`, the age at which habitat reaches `target` value (default `0.95`), rather than requiring a direct discount rate.
* Progress bars for temporal processing implemented in `insights_fraction()`, `insights_area()`, and `insights_discount()`.

# ibis.insights 0.6

* Addition of `insights_area()` for land use layers that are not fractions.
* Support for flexible summary functions in `insights_summary()`.

# ibis.insights 0.5

* Helper functions to allow creating a derivate range from a given variable `create_derivate_range()`
* Function to 'clamp' a given raster to a value range via `st_clamp()`.
* Parameter in `insights_fraction()` to automatically clamp.

# ibis.insights 0.4

* Bug fixes and more support for future clipping when `stars` is provided.

# ibis.insights 0.3

* Full ibis.iSDM scenario support

# ibis.insights 0.2

* Support of stars objects for suitable habitat and refinements

# ibis.insights 0.1

* Initial upload and basic functions
