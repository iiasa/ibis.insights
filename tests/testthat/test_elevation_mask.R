# Tests for elevation suitability masks

test_that("create_elevation_mask is an S4 generic with class methods", {

  skip_if_not_installed("terra")
  skip_if_not_installed("stars")
  suppressWarnings(requireNamespace("terra", quietly = TRUE))
  suppressWarnings(requireNamespace("stars", quietly = TRUE))

  expect_true(methods::isGeneric("create_elevation_mask"))
  expect_true(methods::hasMethod("create_elevation_mask", "SpatRaster"))
  expect_true(methods::hasMethod("create_elevation_mask", "stars"))
})

test_that("create_elevation_mask creates a binary SpatRaster mask", {

  skip_if_not_installed("terra")
  suppressWarnings(requireNamespace("terra", quietly = TRUE))

  dem <- terra::rast(
    nrow = 1,
    ncol = 7,
    vals = c(100, 150, 200, 250, 300, 350, NA)
  )
  names(dem) <- "dem"

  out <- create_elevation_mask(dem, elevation_range = c(200, 300))

  expect_s4_class(out, "SpatRaster")
  expect_equal(names(out), "dem")
  expect_equal(
    terra::values(out, mat = FALSE),
    c(0, 0, 1, 1, 1, 0, NA)
  )
})

test_that("create_elevation_mask supports linear SpatRaster cutoffs", {

  skip_if_not_installed("terra")
  suppressWarnings(requireNamespace("terra", quietly = TRUE))

  dem <- terra::rast(
    nrow = 1,
    ncol = 7,
    vals = c(100, 150, 200, 250, 300, 350, NA)
  )

  out <- create_elevation_mask(
    dem,
    elevation_range = c(200, 300),
    cutoff = "linear",
    tolerance = 100
  )

  expect_equal(
    terra::values(out, mat = FALSE),
    c(0, 0.5, 1, 1, 1, 0.5, NA)
  )
})

test_that("create_elevation_mask preserves multi-layer SpatRaster soft masks", {

  skip_if_not_installed("terra")
  suppressWarnings(requireNamespace("terra", quietly = TRUE))

  dem <- terra::rast(
    nrow = 1,
    ncol = 3,
    nlyr = 2,
    vals = c(150, 250, 350, 100, 200, 300)
  )
  names(dem) <- c("dem_1", "dem_2")

  out <- create_elevation_mask(
    dem,
    elevation_range = c(200, 300),
    cutoff = "linear",
    tolerance = 100
  )

  expect_s4_class(out, "SpatRaster")
  expect_equal(names(out), names(dem))
  expect_equal(
    unname(terra::values(out, mat = TRUE)),
    matrix(c(0.5, 1, 0.5, 0, 1, 1), ncol = 2)
  )
})

test_that("create_elevation_mask supports negative exponential SpatRaster cutoffs", {

  skip_if_not_installed("terra")
  suppressWarnings(requireNamespace("terra", quietly = TRUE))

  dem <- terra::rast(
    nrow = 1,
    ncol = 6,
    vals = c(100, 150, 200, 250, 300, 350)
  )

  out <- create_elevation_mask(
    dem,
    elevation_range = c(200, 300),
    cutoff = "negative_exponential",
    tolerance = 100
  )

  expect_equal(
    terra::values(out, mat = FALSE),
    exp(c(-1, -0.5, 0, 0, 0, -0.5))
  )
})

test_that("create_elevation_mask supports stars inputs", {

  skip_if_not_installed("stars")
  suppressWarnings(requireNamespace("stars", quietly = TRUE))

  vals <- array(c(150, 200, 250, 350, NA), dim = c(x = 5, y = 1))
  dem <- stars::st_as_stars(list(elevation = vals))

  out <- create_elevation_mask(
    dem,
    elevation_range = c(200, 300),
    cutoff = "linear",
    tolerance = 100
  )

  expect_s3_class(out, "stars")
  expect_equal(names(out), "elevation")
  expect_equal(stars::st_dimensions(out), stars::st_dimensions(dem))
  expect_equal(
    as.numeric(out[["elevation"]]),
    c(0.5, 1, 1, 0.5, NA)
  )
})

test_that("create_elevation_mask validates inputs", {

  skip_if_not_installed("terra")
  suppressWarnings(requireNamespace("terra", quietly = TRUE))

  dem <- terra::rast(nrow = 1, ncol = 2, vals = c(200, 300))

  expect_error(create_elevation_mask(dem, elevation_range = 200))
  expect_error(create_elevation_mask(dem, elevation_range = c(300, 200)))
  expect_error(
    create_elevation_mask(
      dem,
      elevation_range = c(200, 300),
      cutoff = "linear"
    ),
    "tolerance"
  )
  expect_error(
    create_elevation_mask(
      dem,
      elevation_range = c(200, 300),
      cutoff = "negative_exponential",
      tolerance = 0
    ),
    "tolerance"
  )
  expect_error(create_elevation_mask(data.frame(x = 1), c(200, 300)), "dem must")
})
