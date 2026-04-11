# Tests for insights_area (continuous areal land-use variant)

test_that('insights_area works with SpatRaster inputs', {

  skip_if_not_installed("terra")
  suppressWarnings(requireNamespace("terra", quietly = TRUE))

  # Binary range raster (values 0/1)
  set.seed(42)
  range <- terra::rast(nrow = 10, ncol = 10,
                       vals = stats::rbinom(100, 1, 0.5))
  terra::crs(range) <- "EPSG:4326"

  # Land-use in area units (km2 per cell fraction, kept <= 1 so
  # the product range * lu stays in [0, 1] for summary compatibility)
  lu <- terra::rast(nrow = 10, ncol = 10,
                    vals = runif(100, 0, 0.9))
  terra::crs(lu) <- "EPSG:4326"

  # Basic application
  expect_no_error(
    out <- insights_area(range = range, lu = lu)
  )
  expect_s4_class(out, "SpatRaster")
  expect_equal(terra::nlyr(out), 1)
  expect_equal(names(out), "insights_suitability")

  # Output values are non-negative
  expect_gte(terra::global(out, "min", na.rm = TRUE)[, 1], 0)

  # Output max <= range max * lu max (element-wise product)
  expect_lte(
    terra::global(out, "max", na.rm = TRUE)[, 1],
    terra::global(range, "max", na.rm = TRUE)[, 1] *
      terra::global(lu, "max", na.rm = TRUE)[, 1] + 1e-6
  )
})

test_that('insights_area accepts lu values greater than 1', {

  skip_if_not_installed("terra")
  suppressWarnings(requireNamespace("terra", quietly = TRUE))

  set.seed(1)
  range <- terra::rast(nrow = 10, ncol = 10,
                       vals = stats::rbinom(100, 1, 0.5))
  terra::crs(range) <- "EPSG:4326"

  # lu in actual km2 (>> 1)
  lu_km2 <- terra::rast(nrow = 10, ncol = 10,
                         vals = runif(100, 0, 25))
  terra::crs(lu_km2) <- "EPSG:4326"

  # Should NOT error – insights_area only requires >= 0
  expect_no_error(out <- insights_area(range = range, lu = lu_km2))
  expect_s4_class(out, "SpatRaster")
  expect_gte(terra::global(out, "min", na.rm = TRUE)[, 1], 0)
})

test_that('insights_area validates inputs', {

  skip_if_not_installed("terra")
  suppressWarnings(requireNamespace("terra", quietly = TRUE))

  set.seed(2)
  range <- terra::rast(nrow = 10, ncol = 10,
                       vals = stats::rbinom(100, 1, 0.5))
  terra::crs(range) <- "EPSG:4326"
  lu <- terra::rast(nrow = 10, ncol = 10, vals = runif(100, 0, 0.8))
  terra::crs(lu) <- "EPSG:4326"

  # Range values out of [0, 1] should error
  expect_error(insights_area(range = range * 3, lu = lu))

  # Negative lu values should error
  expect_error(insights_area(range = range, lu = lu - 1))

  # outfile pointing to non-existent directory should error
  expect_error(insights_area(range = range, lu = lu,
                             outfile = "/nonexistent_dir/out.tif"))
})

test_that('insights_area works with multiple lu layers', {

  skip_if_not_installed("terra")
  suppressWarnings(requireNamespace("terra", quietly = TRUE))

  set.seed(3)
  range <- terra::rast(nrow = 10, ncol = 10,
                       vals = stats::rbinom(100, 1, 0.5))
  terra::crs(range) <- "EPSG:4326"

  lu1 <- terra::rast(nrow = 10, ncol = 10, vals = runif(100, 0, 0.4))
  lu2 <- terra::rast(nrow = 10, ncol = 10, vals = runif(100, 0, 0.4))
  terra::crs(lu1) <- "EPSG:4326"
  terra::crs(lu2) <- "EPSG:4326"
  lu_stack <- c(lu1, lu2)

  expect_no_error(out <- insights_area(range = range, lu = lu_stack))
  expect_s4_class(out, "SpatRaster")
  # Multi-lu output equals range * (lu1 + lu2) per cell
  expected_max <- terra::global(range, "max", na.rm = TRUE)[, 1] *
    (terra::global(lu1, "max", na.rm = TRUE)[, 1] +
       terra::global(lu2, "max", na.rm = TRUE)[, 1])
  expect_lte(terra::global(out, "max", na.rm = TRUE)[, 1], expected_max + 1e-6)
})

test_that('insights_area works with an other layer', {

  skip_if_not_installed("terra")
  suppressWarnings(requireNamespace("terra", quietly = TRUE))

  set.seed(4)
  range <- terra::rast(nrow = 10, ncol = 10,
                       vals = stats::rbinom(100, 1, 0.5))
  terra::crs(range) <- "EPSG:4326"
  lu <- terra::rast(nrow = 10, ncol = 10, vals = runif(100, 0, 0.8))
  terra::crs(lu) <- "EPSG:4326"

  # other layer restricts output (values in [0,1])
  other <- terra::rast(nrow = 10, ncol = 10, vals = runif(100, 0, 1))
  terra::crs(other) <- "EPSG:4326"

  expect_no_error(
    out_with <- insights_area(range = range, lu = lu, other = other)
  )
  expect_no_error(
    out_without <- insights_area(range = range, lu = lu)
  )
  expect_s4_class(out_with, "SpatRaster")
  # With an other [0,1] layer the output max should be <= without-other max
  expect_lte(
    terra::global(out_with, "max", na.rm = TRUE)[, 1],
    terra::global(out_without, "max", na.rm = TRUE)[, 1] + 1e-6
  )

  # other values > 1 should error
  expect_error(insights_area(range = range, lu = lu, other = other + 1.5))
})

test_that('insights_area works with a multi-layer range (time series)', {

  skip_if_not_installed("terra")
  suppressWarnings(requireNamespace("terra", quietly = TRUE))

  set.seed(5)
  r1 <- terra::rast(nrow = 10, ncol = 10, vals = stats::rbinom(100, 1, 0.7))
  r2 <- terra::rast(nrow = 10, ncol = 10, vals = stats::rbinom(100, 1, 0.5))
  r3 <- terra::rast(nrow = 10, ncol = 10, vals = stats::rbinom(100, 1, 0.3))
  range_ts <- c(r1, r2, r3)
  terra::crs(range_ts) <- "EPSG:4326"
  terra::time(range_ts, tstep = "years") <- c(2000, 2020, 2040)

  lu <- terra::rast(nrow = 10, ncol = 10, vals = runif(100, 0, 0.6))
  terra::crs(lu) <- "EPSG:4326"

  expect_no_error(out <- insights_area(range = range_ts, lu = lu))
  expect_s4_class(out, "SpatRaster")
  expect_equal(terra::nlyr(out), 3)
  expect_true(all(terra::global(out, "min", na.rm = TRUE)[, 1] >= 0))
  # Names include time labels
  expect_true(all(grepl("insights_suitability", names(out))))
})

test_that('insights_area outfile parameter works', {

  skip_if_not_installed("terra")
  suppressWarnings(requireNamespace("terra", quietly = TRUE))

  set.seed(6)
  range <- terra::rast(nrow = 10, ncol = 10,
                       vals = stats::rbinom(100, 1, 0.5))
  terra::crs(range) <- "EPSG:4326"
  lu <- terra::rast(nrow = 10, ncol = 10, vals = runif(100, 0, 0.8))
  terra::crs(lu) <- "EPSG:4326"

  tf <- tempfile(fileext = ".tif")
  on.exit(unlink(tf), add = TRUE)

  # Writing to disk returns invisibly (NULL)
  result <- insights_area(range = range, lu = lu, outfile = tf)
  expect_null(result)
  expect_true(file.exists(tf))

  # Written file can be re-read
  re <- terra::rast(tf)
  expect_s4_class(re, "SpatRaster")
})

test_that('insights_area output is compatible with insights_summary', {

  skip_if_not_installed("terra")
  suppressWarnings(requireNamespace("terra", quietly = TRUE))

  # Use small lu values so product stays in [0, 1]
  set.seed(7)
  range <- terra::rast(nrow = 10, ncol = 10,
                       vals = stats::rbinom(100, 1, 0.5))
  terra::crs(range) <- "EPSG:4326"
  lu <- terra::rast(nrow = 10, ncol = 10, vals = runif(100, 0, 0.9))
  terra::crs(lu) <- "EPSG:4326"

  out <- insights_area(range = range, lu = lu)

  # Single layer: insights_summary should work fine
  df <- insights_summary(out)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 1)
  expect_true("suitability" %in% names(df))
})
