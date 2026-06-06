# Test that package works and data can be loaded
test_that('Load data and do InSiGHTS on single layer and summarize', {

  suppressWarnings( requireNamespace("terra", quietly = TRUE) )
  suppressWarnings( requireNamespace("ibis.iSDM", quietly = TRUE) )

  # Load files
  range <- terra::rast(system.file('extdata/example_range.tif', package='ibis.insights',mustWork = TRUE))

  # Also load land-use layers
  lu <- c(
    terra::rast(system.file('extdata/Grassland.tif', package='ibis.insights',mustWork = TRUE)),
    terra::rast(system.file('extdata/Sparsely.vegetated.areas.tif', package='ibis.insights',mustWork = TRUE))
  )
  # Convert to fractions
  lu <- lu / 10000

  # Input checks
  expect_true(ibis.iSDM::is.Raster(range))
  expect_true(ibis.iSDM::is.Raster(lu))
  expect_true(terra::nlyr(lu)==2)

  # Do basic clipping
  expect_no_error(
    out <- insights_fraction(range = range,
                           lu = lu)
  )
  expect_true(ibis.iSDM::is.Raster(out))
  expect_gt(terra::global(out, "max", na.rm = TRUE), 0.3)

  # With a single layer
  expect_no_error(
    out <- insights_fraction(range = range,
                             lu = lu$Grassland)
  )
  expect_true(ibis.iSDM::is.Raster(out))

  # See if it fails correctly if non-fractional layers are supplied
  expect_error(
    out <- insights_fraction(range = range,
                             lu = lu*2)
  )
  expect_error(
    out <- insights_fraction(range = range*2,
                             lu = lu)
  )

  # ------ #
  # Load an additional layer to supply for
  dem <- terra::rast(system.file('extdata/DEM.tif', package='ibis.insights',mustWork = TRUE))

  expect_no_error(
    out <- insights_fraction(range = range,
                             lu = lu,
                             dem)
  )
  expect_true(ibis.iSDM::is.Raster(out))
  expect_gt(terra::global(out, "max", na.rm = TRUE), 0.3)

  # Also with possible error in other
  expect_error(
    out <- insights_fraction(range = range,
                             lu = lu,
                             dem+2)
  )

  # --- #
  expect_no_error(
    out <- insights_fraction(range = range,
                             lu = lu,
                             dem)
  )

  # Now summarize
  df <- insights_summary(out)
  expect_s3_class(df, "data.frame")
  expect_gt(df$suitability, 10)

  # Directly on the range should also work
  expect_no_error(insights_summary(range))

})

# Multiple time steps
# This test ensures that the refinement works even when multiple layers are supplied.
test_that('Do InSiGHTS on multiple layers and summarize', {

  suppressWarnings( requireNamespace("terra", quietly = TRUE) )
  suppressWarnings( requireNamespace("ibis.iSDM", quietly = TRUE) )

  # Load files
  range <- terra::rast(system.file('extdata/example_range.tif', package='ibis.insights',mustWork = TRUE))

  # Also load land-use layers
  lu <- c(
    terra::rast(system.file('extdata/Grassland.tif', package='ibis.insights',mustWork = TRUE)),
    terra::rast(system.file('extdata/Sparsely.vegetated.areas.tif', package='ibis.insights',mustWork = TRUE))
  )
  # Convert to fractions
  lu <- lu / 10000

  # Load an additional layer to supply for
  dem <- terra::rast(system.file('extdata/DEM.tif', package='ibis.insights',mustWork = TRUE))

  # ---- #
  # Sample patches and set them to 0
  pp <- terra::patches( terra::which.lyr(range))

  # Make a time series of the range
  range_ts <- c(range,
                terra::as.int(pp>5),
                terra::as.int(pp>10),
                terra::as.int(pp>15))
  range_ts <- terra::na.omit(range_ts)
  terra::time(range_ts, tstep="years") <- seq(2000, 2050, 15)

  # Input checks
  expect_true(ibis.iSDM::is.Raster(range_ts))
  expect_true(ibis.iSDM::is.Raster(lu))
  expect_true(terra::nlyr(lu)==2)

  # Do basic clipping
  expect_no_error(
    out <- insights_fraction(range = range_ts,
                             lu = lu,
                             dem)
  )
  expect_true(ibis.iSDM::is.Raster(out))
  expect_equal(terra::nlyr(out),4)
  expect_true(all(terra::global(out, "max", na.rm = TRUE)[,1]> 0))

  # Now summarize
  df <- insights_summary(out)
  expect_s3_class(df, "data.frame")
  expect_equal(df$suitability[1],0)
  df <- insights_summary(out,toArea = FALSE)
  expect_s3_class(df, "data.frame")
  df <- insights_summary(out,toArea = FALSE, relative = FALSE)

  # Directly on the range should also work
  expect_no_error(insights_summary(range_ts))
})

# Multiple time steps
# This test ensures that the refinement works even when multiple layers are supplied.
test_that('Do InSiGHTS on stars object', {

  suppressWarnings( requireNamespace("terra", quietly = TRUE) )
  suppressWarnings( requireNamespace("ibis.iSDM", quietly = TRUE) )
  suppressWarnings( requireNamespace("stars", quietly = TRUE) )

  # Load files
  range <- terra::rast(system.file('extdata/example_range.tif', package='ibis.insights',mustWork = TRUE))

  # Load present and future predictors
  ll <- list.files(system.file('extdata/predictors_presfuture/',package = 'ibis.iSDM',mustWork = TRUE),full.names = T)
  ll <- ll[grep("primf|primn",ll)]
  # Load the same files future ones
  suppressWarnings(
    pred_future <- stars::read_stars(ll) |> stars:::slice.stars('Time', seq(1, 86, by = 10))
  )
  sf::st_crs(pred_future) <- sf::st_crs(4326)
  names(pred_future) <- tools::file_path_sans_ext(basename(ll))

  # Convert to fractions
  pred_future <- ibis.iSDM::predictor_transform(pred_future, option = "norm") |>
    round(digits = 2)

  # Load an additional layer to supply for
  dem <- terra::rast(system.file('extdata/DEM.tif', package='ibis.insights',mustWork = TRUE))

  # Input checks
  expect_true(inherits(pred_future, "stars"))

  # Do basic clipping
  expect_no_error(
    suppressMessages(
      out <- insights_fraction(range = range,
                               lu = pred_future)
    )
  )
  expect_true(inherits(out, "stars"))
  expect_length(out, 1)
  expect_length(stars::st_get_dimension_values(out, "time"), 9)

  # Now summarize
  df <- insights_summary(out)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 9)
  df <- insights_summary(out,toArea = FALSE)
  expect_s3_class(df, "data.frame")
  df <- insights_summary(out,toArea = FALSE, relative = FALSE)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 9)

  # Directly on the suitable area should also work
  # if those are single layers
  o <- ibis.iSDM:::st_reduce(obj = pred_future, names(pred_future),"suitability",fun = "sum")
  expect_no_error(insights_summary(o))
})
