# Manually test ibis.insights using loaded raster and stars objects
test_that('Directly apply InSiGHTS on rasters and stars', {

  skip_if_not_installed("stars")
  skip_if_not_installed("terra")

  suppressWarnings( requireNamespace("terra", quietly = TRUE) )
  suppressWarnings( requireNamespace("stars", quietly = TRUE) )

  # Load range
  range <- ibis.insights:::load_exampledata(timeperiod = "current")
  testthat::expect_s4_class(range, "SpatRaster")

  # Load land-use layers
  lu <- c(
    terra::rast(system.file('extdata/Grassland.tif', package='ibis.insights',mustWork = TRUE)),
    terra::rast(system.file('extdata/Sparsely.vegetated.areas.tif', package='ibis.insights',mustWork = TRUE))
  )
  # Convert to fractions
  lu <- lu / 10000
  testthat::expect_true(
    all(terra::global(lu, "max", na.rm = TRUE)[,1] <= 1)
  )

  # --- #
  # Now apply InSiGHTS
  # range = current | lu = current
  expect_no_error(
    suppressMessages(
      out <- insights_fraction(range = range, lu = lu)
    )
  )
  expect_s4_class(out, "SpatRaster")
  expect_gt(terra::global(out,"max",na.rm=T)[,1], 0)
  # Habitat clips should be smaller/equal as they are a subset
  expect_gte(terra::global(range,"max",na.rm=T)[,1],
             terra::global(out,"max",na.rm=T)[,1],)

  # --------- #
  # Load future layer and repeat
  range <- ibis.insights:::load_exampledata(timeperiod = "future")
  testthat::expect_s3_class(range, "stars")
  testthat::expect_length(range, 1)

  # Now apply InSiGHTS
  # range = future | lu = current
  expect_no_error(
    suppressMessages(
      out <- insights_fraction(range = range, lu = lu)
    )
  )
  testthat::expect_s3_class(out, "stars")
  testthat::expect_s3_class(insights_summary(out), "data.frame")

  # --- #
  # Apply with future lu
  ll <- list.files(system.file('extdata/predictors_presfuture/',package = "ibis.iSDM",mustWork = TRUE),
                   full.names = T)
  # Load the same files future ones
  suppressWarnings(
    lu <- stars::read_stars(ll) |> stars:::slice.stars('Time', seq(1, 86, by = 10))
  )
  sf::st_crs(lu) <- sf::st_crs(4326)

  # Normalize here
  lu <- lu |> stars:::select.stars(crops, secdf)
  suppressMessages(
    lu <- ibis.iSDM::predictor_transform(lu, "norm") |> round(2)
  )

  # range = future | lu = future
  expect_no_error(
    suppressMessages(
      out <- insights_fraction(range = range, lu = lu)
    )
  )
  testthat::expect_s3_class(out, "stars")

})

# ----------------- #
# Exact tests
test_that('Exact INSIGHTS test', {

  skip_if_not_installed("stars")
  skip_if_not_installed("terra")

  suppressWarnings( requireNamespace("terra", quietly = TRUE) )
  suppressWarnings( requireNamespace("stars", quietly = TRUE) )

  # Define raster
  range <- terra::rast(nrow = 10, ncol = 10,
                       vals = stats::rbinom(100,1,.5))
  testthat::expect_s4_class(range, "SpatRaster")

  val <- stats::rlnorm(100,0,.5)
  val <- val/max(val)
  lu <- terra::rast(nrow = 10, ncol = 10, vals = val)

  # --- #
  # Now apply InSiGHTS
  # range = current | lu = current
  expect_no_error(
    suppressMessages(
      out <- insights_fraction(range = range, lu = lu)
    )
  )
  expect_s4_class(out, "SpatRaster")
  expect_gt(terra::global(out,"max",na.rm=T)[,1], 0)
})

test_that('insights_discount reaches target value at target_age', {

  skip_if_not_installed("terra")
  suppressWarnings(requireNamespace("terra", quietly = TRUE))

  lu <- terra::rast(nrow = 2, ncol = 2, vals = 1, crs = "EPSG:4326")
  age <- terra::rast(nrow = 2, ncol = 2, vals = 20, crs = "EPSG:4326")

  out <- insights_discount(lu = lu, age = age, target_age = 20, target = 0.95)

  expect_s4_class(out, "SpatRaster")
  expect_equal(terra::global(out, "mean", na.rm = TRUE)[1, 1], 0.95)
})

# ---- Additional coverage: clamp flag and summary fun variants ----
test_that('insights_fraction with clamping works', {

  skip_if_not_installed("terra")
  suppressWarnings(requireNamespace("terra", quietly = TRUE))

  set.seed(42)
  range <- terra::rast(nrow = 10, ncol = 10,
                       vals = stats::rbinom(100, 1, 0.5))
  terra::crs(range) <- "EPSG:4326"

  # lu values slightly above 1 – would fail without clamp
  lu_over <- terra::rast(nrow = 10, ncol = 10,
                         vals = runif(100, 0.5, 1.2))
  terra::crs(lu_over) <- "EPSG:4326"

  # Without clamp this should error because max > 1
  expect_error(insights_fraction(range = range, lu = lu_over))

  # With clamp=TRUE it succeeds
  expect_no_error(
    suppressMessages(
      out <- insights_fraction(range = range, lu = lu_over, clamp = TRUE)
    )
  )
  expect_s4_class(out, "SpatRaster")
  expect_lte(terra::global(out, "max", na.rm = TRUE)[, 1], 1)

  # Multi-layer lu values can sum above 1 even when each layer is fractional.
  lu_stack <- c(lu_over * 0 + 0.8, lu_over * 0 + 0.7)
  out_sum <- insights_fraction(range = range, lu = lu_stack, clamp = TRUE)
  expect_lte(terra::global(out_sum, "max", na.rm = TRUE)[, 1], 1)
})

test_that('insights_fraction applies other across raster and stars inputs', {

  skip_if_not_installed("terra")
  skip_if_not_installed("stars")
  suppressWarnings(requireNamespace("terra", quietly = TRUE))
  suppressWarnings(requireNamespace("stars", quietly = TRUE))

  range <- terra::rast(nrow = 4, ncol = 4, xmin = 0, xmax = 4,
                       ymin = 0, ymax = 4, vals = 1, crs = "EPSG:3857")
  lu <- terra::rast(nrow = 4, ncol = 4, xmin = 0, xmax = 4,
                    ymin = 0, ymax = 4, vals = 0.8, crs = "EPSG:3857")
  other <- terra::rast(nrow = 4, ncol = 4, xmin = 0, xmax = 4,
                       ymin = 0, ymax = 4, vals = 0.5, crs = "EPSG:3857")
  other_st <- stars::st_as_stars(other)

  out_raster <- insights_fraction(range = range, lu = lu, other = other_st)
  expect_s4_class(out_raster, "SpatRaster")
  expect_equal(terra::global(out_raster, "mean", na.rm = TRUE)[1, 1], 0.4)

  range_ts <- c(range, range)
  lu_ts <- c(lu, lu)
  terra::time(range_ts) <- as.Date(c("2020-01-01", "2040-01-01"))
  terra::time(lu_ts) <- as.Date(c("2020-01-01", "2040-01-01"))
  range_st <- stars::st_as_stars(range_ts)
  lu_st <- stars::st_as_stars(lu_ts)

  range_dims <- stars::st_dimensions(range_st)
  names(range_dims)[3] <- "Time"
  stars::st_dimensions(range_st) <- range_dims

  lu_dims <- stars::st_dimensions(lu_st)
  names(lu_dims)[3] <- "Time"
  stars::st_dimensions(lu_st) <- lu_dims

  out_without <- insights_fraction(range = range_st, lu = lu_st)
  out_with <- insights_fraction(range = range_st, lu = lu_st, other = other_st)
  expect_s3_class(out_with, "stars")
  expect_lt(sum(out_with[[1]], na.rm = TRUE), sum(out_without[[1]], na.rm = TRUE))

  out_st_r <- insights_fraction(range = range_st, lu = lu, other = other_st)
  expect_s3_class(out_st_r, "stars")

  out_r_st <- insights_fraction(range = range, lu = lu_st, other = other)
  expect_s3_class(out_r_st, "stars")
})

test_that('insights_summary works with all supported fun values', {

  skip_if_not_installed("terra")
  suppressWarnings(requireNamespace("terra", quietly = TRUE))

  set.seed(99)
  range <- terra::rast(nrow = 10, ncol = 10,
                       vals = stats::rbinom(100, 1, 0.5))
  terra::crs(range) <- "EPSG:4326"
  lu <- terra::rast(nrow = 10, ncol = 10,
                    vals = runif(100, 0.1, 0.8))
  terra::crs(lu) <- "EPSG:4326"

  out <- suppressMessages(insights_fraction(range = range, lu = lu))

  for (f in c("sum", "min", "max", "mean", "median")) {
    df <- insights_summary(out, fun = f)
    expect_s3_class(df, "data.frame")
    expect_true("suitability" %in% names(df))
  }

  # Invalid fun should error
  expect_error(insights_summary(out, fun = "variance"))
})

test_that('insights_summary returns NA relative change for zero baseline', {

  skip_if_not_installed("terra")
  suppressWarnings(requireNamespace("terra", quietly = TRUE))

  r0 <- terra::rast(nrow = 3, ncol = 3, vals = 0, crs = "EPSG:4326")
  r1 <- terra::rast(nrow = 3, ncol = 3, vals = 0.4, crs = "EPSG:4326")
  obj <- c(r0, r1)
  terra::time(obj, tstep = "years") <- c(2020, 2040)

  expect_warning(
    df <- insights_summary(obj, toArea = FALSE, relative = TRUE),
    regexp = "Reference value"
  )
  expect_true(all(is.na(df$relative_change_perc)))
})

test_that('insights_fraction writes output to disk via outfile', {

  skip_if_not_installed("terra")
  suppressWarnings(requireNamespace("terra", quietly = TRUE))

  set.seed(11)
  range <- terra::rast(nrow = 10, ncol = 10,
                       vals = stats::rbinom(100, 1, 0.5))
  terra::crs(range) <- "EPSG:4326"
  lu <- terra::rast(nrow = 10, ncol = 10,
                    vals = runif(100, 0, 0.8))
  terra::crs(lu) <- "EPSG:4326"

  tf <- tempfile(fileext = ".tif")
  on.exit(unlink(tf), add = TRUE)

  result <- suppressMessages(
    insights_fraction(range = range, lu = lu, outfile = tf)
  )
  # Returns NULL when writing to disk
  expect_null(result)
  expect_true(file.exists(tf))

  re <- terra::rast(tf)
  expect_s4_class(re, "SpatRaster")
  expect_lte(terra::global(re, "max", na.rm = TRUE)[, 1], 1)
})

# ---- Multi-layer SpatRaster: insights_summary + symmetric difference ----
test_that('insights_summary works with multi-layer SpatRaster', {

  skip_if_not_installed("terra")
  suppressWarnings(requireNamespace("terra", quietly = TRUE))

  set.seed(7)
  make_lyr <- function(lo, hi) {
    r <- terra::rast(nrow = 10, ncol = 10, crs = "EPSG:4326",
                     xmin = 0, xmax = 10, ymin = 0, ymax = 10)
    terra::values(r) <- runif(100, lo, hi)
    r
  }
  r_stack <- c(make_lyr(0.3, 0.9), make_lyr(0.2, 0.7), make_lyr(0.05, 0.5))
  terra::time(r_stack) <- as.Date(c("2020-01-01", "2040-01-01", "2060-01-01"))

  # Standard relative change
  df <- insights_summary(r_stack, toArea = FALSE, relative = TRUE, symmetric = FALSE)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 3L)
  expect_true("relative_change_perc" %in% names(df))
  expect_false("relative_change_sym" %in% names(df))
  expect_equal(df$relative_change_perc[1], 0)

  # Symmetric relative difference
  df_sym <- insights_summary(r_stack, toArea = FALSE, relative = TRUE, symmetric = TRUE)
  expect_s3_class(df_sym, "data.frame")
  expect_equal(nrow(df_sym), 3L)
  expect_true("relative_change_perc" %in% names(df_sym))
  expect_true("relative_change_sym" %in% names(df_sym))
  # D_sym(x_0, x_0) = 0 at baseline
  expect_equal(df_sym$relative_change_sym[1], 0)
  # Bounded in [-1, 1]
  expect_true(all(abs(df_sym$relative_change_sym) <= 1, na.rm = TRUE))

  # symmetric = TRUE without relative = TRUE must error
  expect_error(
    insights_summary(r_stack, toArea = FALSE, relative = FALSE, symmetric = TRUE),
    regexp = "relative"
  )
})

# ---- Multi-layer stars: insights_summary + symmetric difference ----
test_that('insights_summary works with multi-layer stars', {

  skip_if_not_installed("terra")
  skip_if_not_installed("stars")
  suppressWarnings(requireNamespace("terra", quietly = TRUE))
  suppressWarnings(requireNamespace("stars", quietly = TRUE))

  # Load the built-in multi-timestep future range (Bombina bombina, SSP1-2.6)
  range_st <- suppressMessages(ibis.insights:::load_exampledata("future"))
  testthat::expect_s3_class(range_st, "stars")
  testthat::expect_length(range_st, 1)

  # Apply insights_fraction with a single current land-use layer
  lu <- terra::rast(system.file('extdata/Grassland.tif',
                                package = 'ibis.insights', mustWork = TRUE))
  lu <- lu / 10000

  out_st <- suppressMessages(insights_fraction(range = range_st, lu = lu))
  testthat::expect_s3_class(out_st, "stars")
  testthat::expect_true(length(stars::st_get_dimension_values(out_st, which = 3)) > 1)

  # Standard summary with relative change
  df <- suppressMessages(
    insights_summary(out_st, relative = TRUE, symmetric = FALSE)
  )
  expect_s3_class(df, "data.frame")
  expect_true(nrow(df) > 1)
  expect_true("relative_change_perc" %in% names(df))
  expect_false("relative_change_sym" %in% names(df))

  # Symmetric relative difference
  df_sym <- suppressMessages(
    insights_summary(out_st, relative = TRUE, symmetric = TRUE)
  )
  expect_s3_class(df_sym, "data.frame")
  expect_true("relative_change_sym" %in% names(df_sym))
  # Bounded in [-1, 1]
  expect_true(all(abs(df_sym$relative_change_sym) <= 1, na.rm = TRUE))

  # symmetric = TRUE without relative = TRUE must error
  expect_error(
    suppressMessages(
      insights_summary(out_st, relative = FALSE, symmetric = TRUE)
    ),
    regexp = "relative"
  )
})
