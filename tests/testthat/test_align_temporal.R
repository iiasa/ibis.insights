# Tests for temporal alignment helpers

test_that("align_temporal is an S4 generic with class methods", {

  skip_if_not_installed("terra")
  skip_if_not_installed("stars")
  suppressWarnings(requireNamespace("terra", quietly = TRUE))
  suppressWarnings(requireNamespace("stars", quietly = TRUE))

  expect_true(methods::isGeneric("align_temporal"))
  expect_true(methods::hasMethod("align_temporal", c("SpatRaster", "SpatRaster")))
  expect_true(methods::hasMethod("align_temporal", c("stars", "stars")))
})

test_that("align_temporal aligns SpatRaster layers to target years", {

  skip_if_not_installed("terra")
  suppressWarnings(requireNamespace("terra", quietly = TRUE))

  source <- terra::rast(nrow = 1, ncol = 1, nlyr = 3, vals = c(10, 20, 30))
  terra::time(source) <- as.Date(c("2000-01-01", "2010-01-01", "2020-01-01"))

  target <- terra::rast(nrow = 1, ncol = 1, nlyr = 4, vals = 1)
  terra::time(target) <- as.Date(c("1995-01-01", "2005-01-01",
                                   "2015-01-01", "2025-01-01"))

  out <- align_temporal(source, target)

  expect_s4_class(out, "SpatRaster")
  expect_equal(terra::nlyr(out), 4)
  expect_equal(as.numeric(terra::values(out, mat = TRUE)[1, ]), c(10, 10, 20, 30))
  out_time <- terra::time(out)
  if(inherits(out_time, "POSIXt")) {
    out_years <- as.integer(format(as.POSIXct(out_time), "%Y"))
  } else if(inherits(out_time, "Date")) {
    out_years <- as.integer(format(out_time, "%Y"))
  } else {
    out_years <- as.integer(as.numeric(out_time))
  }
  expect_equal(
    out_years,
    c(1995, 2005, 2015, 2025)
  )
})

test_that("align_temporal aligns stars time dimensions and normalizes Time", {

  skip_if_not_installed("stars")
  skip_if_not_installed("units")
  suppressWarnings(requireNamespace("stars", quietly = TRUE))
  suppressWarnings(requireNamespace("units", quietly = TRUE))

  source_vals <- array(c(10, 20, 30), dim = c(x = 1, y = 1, Time = 3))
  source <- stars::st_as_stars(list(value = source_vals))
  source <- stars::st_set_dimensions(
    source,
    "Time",
    values = as.Date(c("2000-01-01", "2010-01-01", "2020-01-01"))
  )

  target_vals <- array(1:4, dim = c(x = 1, y = 1, time = 4))
  target <- stars::st_as_stars(list(value = target_vals))
  target <- stars::st_set_dimensions(
    target,
    "time",
    values = as.Date(c("1995-01-01", "2005-01-01",
                       "2015-01-01", "2025-01-01"))
  )

  out <- align_temporal(source, target)

  expect_s3_class(out, "stars")
  expect_equal(names(stars::st_dimensions(out))[3], "time")
  expect_equal(as.numeric(out[[1]]), c(10, 10, 20, 30))
  expect_equal(
    as.integer(as.numeric(stars::st_get_dimension_values(out, "time"))),
    c(1995, 2005, 2015, 2025)
  )
  expect_true(
    units::deparse_unit(stars::st_get_dimension_values(out, "time")) %in%
      c("years", "year", "a")
  )
})

test_that("align_temporal harmonizes stars unit-based years", {

  skip_if_not_installed("stars")
  skip_if_not_installed("units")
  suppressWarnings(requireNamespace("stars", quietly = TRUE))
  suppressWarnings(requireNamespace("units", quietly = TRUE))

  source <- stars::st_as_stars(list(
    value = array(c(1, 2), dim = c(x = 1, y = 1, time = 2))
  ))
  source <- stars::st_set_dimensions(
    source,
    "time",
    values = units::set_units(c(2000L, 2020L), "years")
  )

  target <- stars::st_as_stars(list(
    value = array(1:3, dim = c(x = 1, y = 1, time = 3))
  ))
  target <- stars::st_set_dimensions(
    target,
    "time",
    values = as.Date(c("1990-01-01", "2010-01-01", "2030-01-01"))
  )

  out <- align_temporal(source, target)

  expect_equal(as.numeric(out[[1]]), c(1, 1, 2))
  expect_equal(
    as.integer(as.numeric(stars::st_get_dimension_values(out, "time"))),
    c(1990, 2010, 2030)
  )
})

test_that("align_temporal validates matching temporal object types", {

  skip_if_not_installed("terra")
  skip_if_not_installed("stars")
  suppressWarnings(requireNamespace("terra", quietly = TRUE))
  suppressWarnings(requireNamespace("stars", quietly = TRUE))

  raster <- terra::rast(nrow = 1, ncol = 1, nlyr = 1, vals = 1)
  terra::time(raster) <- as.Date("2000-01-01")

  star <- stars::st_as_stars(list(value = array(1, dim = c(x = 1, y = 1, time = 1))))
  star <- stars::st_set_dimensions(star, "time", values = as.Date("2000-01-01"))

  expect_error(align_temporal(raster, star), "both be")
})
