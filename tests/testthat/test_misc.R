# Tests for utility functions: relChange and st_clamp

# ---- relChange ----
test_that('relChange computes relative change correctly', {

  # Basic decreasing sequence
  x <- c(100, 90, 80, 50)
  result <- relChange(x)
  expect_length(result, 4)
  expect_equal(result[1], 0)        # reference point is always 0
  expect_equal(result[2], -10)      # (90-100)/100 * 100 = -10
  expect_equal(result[3], -20)      # (80-100)/100 * 100 = -20
  expect_equal(result[4], -50)      # (50-100)/100 * 100 = -50

  # Basic increasing sequence
  y <- c(10, 20, 30)
  result2 <- relChange(y)
  expect_equal(result2[1], 0)
  expect_equal(result2[2], 100)     # doubled: +100%
  expect_equal(result2[3], 200)     # tripled: +200%

  # Custom multiplication factor
  result3 <- relChange(y, fac = 1)
  expect_equal(result3[2], 1.0)     # raw fraction rather than percent

  # Negative values are allowed (e.g. temperature change)
  z <- c(-10, -5, 0, 10)
  result4 <- relChange(z)
  expect_length(result4, 4)
  expect_equal(result4[1], 0)

  # Constant series → all zeros
  const <- c(5, 5, 5)
  expect_equal(relChange(const), c(0, 0, 0))
})

test_that('relChange input validation works', {

  # Length-1 input should error
  expect_error(relChange(c(5)))

  # Non-numeric input should error
  expect_error(relChange(c("a", "b")))

  # Non-numeric fac should error
  expect_error(relChange(c(1, 2), fac = "x"))
})

test_that('relChange handles zero reference value', {

  # First element is 0 → warning and all-NA result
  expect_warning(result <- relChange(c(0, 10, 20)))
  expect_true(all(is.na(result)))
})

# ---- st_clamp ----
test_that('st_clamp clamps SpatRaster values correctly', {

  skip_if_not_installed("terra")
  suppressWarnings(requireNamespace("terra", quietly = TRUE))

  # Raster with values -2 .. 2
  r <- terra::rast(nrow = 5, ncol = 5, vals = seq(-2, 2, length.out = 25))

  # Clamp to [0, 1]
  out <- ibis.insights:::st_clamp(r, lb = 0, ub = 1)
  expect_gte(terra::global(out, "min", na.rm = TRUE)[, 1], 0)
  expect_lte(terra::global(out, "max", na.rm = TRUE)[, 1], 1)
  expect_s4_class(out, "SpatRaster")

  # lower/upper aliases are equivalent to lb/ub
  out_alias <- ibis.insights:::st_clamp(r, lower = 0, upper = 1)
  expect_equal(
    terra::values(out_alias, mat = FALSE),
    terra::values(out, mat = FALSE)
  )

  # Only lower bound
  out_lb <- ibis.insights:::st_clamp(r, lb = 0, ub = Inf)
  expect_gte(terra::global(out_lb, "min", na.rm = TRUE)[, 1], 0)
  # Upper end is not constrained, so max remains the original maximum
  expect_gt(terra::global(out_lb, "max", na.rm = TRUE)[, 1], 1)

  # Only upper bound
  out_ub <- ibis.insights:::st_clamp(r, lb = -Inf, ub = 0)
  expect_lte(terra::global(out_ub, "max", na.rm = TRUE)[, 1], 0)

  # Infinite bounds → no change
  out_inf <- ibis.insights:::st_clamp(r, lb = -Inf, ub = Inf)
  expect_equal(
    terra::global(out_inf, "min", na.rm = TRUE)[, 1],
    terra::global(r, "min", na.rm = TRUE)[, 1]
  )
})

test_that('st_clamp clamps stars values correctly', {

  skip_if_not_installed("stars")
  suppressWarnings(requireNamespace("stars", quietly = TRUE))

  vals <- array(seq(-2, 2, length.out = 25), dim = c(x = 5, y = 5))
  s <- stars::st_as_stars(list(a = vals, b = vals * 2))

  out <- ibis.insights:::st_clamp(s, lower = 0, upper = 1)
  expect_s3_class(out, "stars")
  expect_equal(names(out), names(s))
  expect_equal(stars::st_dimensions(out), stars::st_dimensions(s))

  for(attr in names(out)) {
    expect_gte(min(out[[attr]], na.rm = TRUE), 0)
    expect_lte(max(out[[attr]], na.rm = TRUE), 1)
  }

  out_lb <- ibis.insights:::st_clamp(s, lower = 0, upper = Inf)
  for(attr in names(out_lb)) {
    expect_gte(min(out_lb[[attr]], na.rm = TRUE), 0)
  }
  expect_gt(max(out_lb[["b"]], na.rm = TRUE), 1)

  out_ub <- ibis.insights:::st_clamp(s, lower = -Inf, upper = 0)
  for(attr in names(out_ub)) {
    expect_lte(max(out_ub[[attr]], na.rm = TRUE), 0)
  }
})

test_that('st_clamp input validation works', {

  skip_if_not_installed("terra")
  suppressWarnings(requireNamespace("terra", quietly = TRUE))

  r <- terra::rast(nrow = 5, ncol = 5, vals = runif(25))

  # lb == ub should error
  expect_error(ibis.insights:::st_clamp(r, lb = 0.5, ub = 0.5))

  # lb > ub should error
  expect_error(ibis.insights:::st_clamp(r, lb = 1, ub = 0))

  # Non-raster input should error
  expect_error(ibis.insights:::st_clamp(list(a = 1), lb = 0, ub = 1))

  # Non-scalar bounds should error
  expect_error(ibis.insights:::st_clamp(r, lower = c(0, 1), upper = 1))
})
