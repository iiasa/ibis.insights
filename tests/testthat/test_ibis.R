test_that("ibis.iSDM DistributionModel methods delegate to extracted layers", {

  skip_if_not_installed("ibis.iSDM")
  skip_if_not_installed("terra")
  suppressWarnings(requireNamespace("terra", quietly = TRUE))

  threshold <- terra::rast(nrow = 2, ncol = 2, vals = c(0, 1, 1, 0))
  lu <- terra::rast(nrow = 2, ncol = 2, vals = rep(0.5, 4))
  terra::crs(threshold) <- terra::crs(lu) <- "EPSG:4326"

  names(threshold) <- "threshold_mean"
  attr(threshold, "threshold") <- 0.5

  fit <- structure(
    list(
      show_rasters = function() "threshold_percentile",
      get_thresholdvalue = function() 0.5,
      get_data = function(x = "prediction") {
        threshold
      }
    ),
    class = c("DistributionModel", "R6")
  )

  out_direct <- insights_fraction(range = fit, lu = lu)
  out_manual <- insights_fraction(range = threshold, lu = lu)
  expect_equal(
    terra::values(out_direct, mat = FALSE),
    terra::values(out_manual, mat = FALSE)
  )

  area_direct <- insights_area(range = fit, lu = lu)
  area_manual <- insights_area(range = threshold, lu = lu)
  expect_equal(
    terra::values(area_direct, mat = FALSE),
    terra::values(area_manual, mat = FALSE)
  )
})

test_that("ibis.iSDM BiodiversityScenario methods align temporal inputs", {

  skip_if_not_installed("ibis.iSDM")
  skip_if_not_installed("stars")
  skip_if_not_installed("terra")
  suppressWarnings(requireNamespace("stars", quietly = TRUE))
  suppressWarnings(requireNamespace("terra", quietly = TRUE))

  scenario_time <- as.Date(c("2020-01-01", "2030-01-01", "2040-01-01"))
  scenario_dims <- c(x = 2, y = 2, time = length(scenario_time))
  stars_dims <- stars::st_dimensions(
    x = c(0.5, 1.5),
    y = c(1.5, 0.5),
    time = scenario_time,
    .raster = c("x", "y")
  )
  threshold_values <- array(
    c(0, 1, 1, 0,
      1, 1, 0, 0,
      1, 0, 1, 0),
    dim = scenario_dims
  )
  scenario_data <- stars::st_as_stars(list(
    threshold = threshold_values
  ),
  dimensions = stars_dims
  )
  sf::st_crs(scenario_data) <- sf::st_crs(4326)

  sc <- structure(
    list(get_data = function(what = "scenarios") scenario_data),
    class = c("BiodiversityScenario", "R6")
  )

  lu_time <- as.Date(c("2020-01-01", "2040-01-01"))
  lu_dims <- stars::st_dimensions(
    x = c(0.5, 1.5),
    y = c(1.5, 0.5),
    time = lu_time,
    .raster = c("x", "y")
  )
  lu <- stars::st_as_stars(
    list(lu = array(0.5, dim = c(x = 2, y = 2, time = 2))),
    dimensions = lu_dims
  )
  sf::st_crs(lu) <- sf::st_crs(4326)

  out <- insights_fraction(range = sc, lu = lu)
  expect_s3_class(out, "stars")
  expect_equal(
    length(stars::st_get_dimension_values(out, "time")),
    length(scenario_time)
  )

  area_out <- insights_area(range = sc, lu = lu)
  expect_s3_class(area_out, "stars")
  expect_equal(
    length(stars::st_get_dimension_values(area_out, "time")),
    length(scenario_time)
  )
})

# Test that package works and data can be loaded
test_that('Train a ibis.iSDM model and apply inSights on it', {

  skip_if_not_installed("ibis.iSDM")

  suppressWarnings( requireNamespace("terra", quietly = TRUE) )
  suppressWarnings( requireNamespace("ibis.iSDM", quietly = TRUE) )
  suppressPackageStartupMessages(
    require("ibis.iSDM")
  )

  options("ibis.setupmessages" = FALSE)

  # Load ibis test data
  background <- terra::rast(system.file('extdata/europegrid_50km.tif', package='ibis.iSDM',mustWork = TRUE))
  virtual_points <- sf::st_read(system.file('extdata/input_data.gpkg', package='ibis.iSDM',mustWork = TRUE),'points',quiet = TRUE)
  ll <- list.files(system.file('extdata/predictors',package = 'ibis.iSDM',mustWork = TRUE),full.names = T)
  predictors <- terra::rast(ll);names(predictors) <- tools::file_path_sans_ext(basename(ll))

  # Also load land-use layers
  lu <- c(
    terra::rast(system.file('extdata/Grassland.tif', package='ibis.insights',mustWork = TRUE)),
    terra::rast(system.file('extdata/Sparsely.vegetated.areas.tif', package='ibis.insights',mustWork = TRUE))
  )
  # Convert to fractions
  lu <- lu / 10000

  # Now train a small little model
  fit <- ibis.iSDM::distribution(background) |>
    ibis.iSDM::add_biodiversity_poipo(virtual_points,field_occurrence = "Observed") |>
    ibis.iSDM::add_predictors(predictors) |>
    ibis.iSDM::engine_glm() |>
    ibis.iSDM::train() |>
    ibis.iSDM::threshold(method = "perc", value = .33)

  expect_s3_class(fit, "DistributionModel")
  tr <- fit$get_data("threshold_percentile")
  expect_s4_class(tr, "SpatRaster")
  if(terra::nlyr(tr)>1) tr <- tr[[1]]

  # --- #
  # Now apply InSiGHTS
  expect_no_error(
    suppressMessages(
      out <- insights_fraction(range = fit,lu = lu)
    )
  )
  expect_s4_class(out, "SpatRaster")
  expect_gt(terra::global(out,"max",na.rm=T)[,1], 0)

  # Also apply on the threshold directly
  expect_no_error(
    suppressMessages(
      out2 <- insights_fraction(range = tr,lu = lu)
    )
  )
  expect_equal(round(terra::global(out, "max", na.rm = TRUE)[,1],3),
               round(terra::global(out2, "max", na.rm = TRUE)[,1],3))

})

# Test that package works and data can be loaded
test_that('Make a ibis.iSDM scenario projection and apply InSiGHTS on it', {

  skip_if_not_installed("ibis.iSDM")
  skip_if_not_installed("stars")
  skip_on_ci()
  skip_on_cran()

  suppressWarnings( requireNamespace("terra", quietly = TRUE) )
  suppressWarnings( requireNamespace("ibis.iSDM", quietly = TRUE) )
  suppressWarnings( requireNamespace("stars", quietly = TRUE) )
  suppressWarnings(
    suppressPackageStartupMessages( require("ibis.iSDM"))
  )

  options("ibis.setupmessages" = FALSE)

  # Load ibis test data
  background <- terra::rast(system.file('extdata/europegrid_50km.tif', package='ibis.iSDM',mustWork = TRUE))
  virtual_points <- sf::st_read(system.file('extdata/input_data.gpkg', package='ibis.iSDM'), 'points',quiet = TRUE)
  ll <- list.files(system.file('extdata/predictors_presfuture/',package = "ibis.iSDM",mustWork = TRUE),
                   full.names = T)
  # Load the same files future ones
  suppressWarnings(
    pred_future <- stars::read_stars(ll) |> stars:::slice.stars('Time', seq(1, 86, by = 10))
  )
  sf::st_crs(pred_future) <- sf::st_crs(4326)

  pred_current <- ibis.iSDM:::stars_to_raster(pred_future, 1)[[1]]
  names(pred_current) <- names(pred_future)

  ab <- ibis.iSDM::pseudoabs_settings(nrpoints = 1000,min_ratio = 1,method = "mcp")
  suppressMessages(
    virtual_points <- ibis.iSDM::add_pseudoabsence(virtual_points,field_occurrence = 'Observed',
                                      template = background, settings = ab)
  )

  x <- ibis.iSDM::distribution(background) |>
    ibis.iSDM::add_biodiversity_poipa(virtual_points,
                                      field_occurrence = 'Observed', name = 'Virtual points') |>
    ibis.iSDM::add_predictors(pred_current, transform = 'scale',derivates = "none") |>
    ibis.iSDM::engine_glm()

  modf <- ibis.iSDM::train(x, runname = 'Null', verbose = FALSE) |>
    ibis.iSDM::threshold(method = 'percent',value = .3)

  expect_length(modf$show_rasters(),2)
  expect_true(ibis.iSDM::is.Raster(modf$get_data("prediction")))

  # Now build a scenario
  # Guard against ibis.iSDM API changes that cause threshold() to call
  # add_predictors() internally, resulting in "No model object found in scenario?"
  sc <- tryCatch(
    ibis.iSDM::scenario(modf, copy_model = TRUE) |>
      ibis.iSDM::add_predictors(pred_future, transform = 'scale', derivates = "none") |>
      ibis.iSDM::threshold() |>
      ibis.iSDM::project(),
      
    error = function(e) {
      skip(paste("ibis.iSDM scenario incompatibility:", conditionMessage(e)))
    }
  )

  expect_s3_class(sc, "BiodiversityScenario")

  # --- #
  # Now apply InSiGHTS
  lu <- pred_future |> stars:::select.stars(crops, secdf)
  lu <- ibis.iSDM::predictor_transform(lu, "norm") |> round(2)
  out <- insights_fraction(range = sc,
                           lu = lu)

  expect_s3_class(out, "stars")

  # Summarize
  suppressPackageStartupMessages(
    suppressMessages(
      o <- insights_summary(out)
    )
  )
  expect_s3_class(o, "data.frame")
  o <- insights_summary(out,toArea = TRUE,relative = TRUE)
  expect_s3_class(o, "data.frame")
})
