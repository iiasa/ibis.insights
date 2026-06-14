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

test_that("apply_elevation_mask repeats static SpatRaster masks over temporal projections", {

  skip_if_not_installed("terra")
  suppressWarnings(requireNamespace("terra", quietly = TRUE))

  mask <- terra::rast(nrow = 1, ncol = 2, vals = c(0.5, 1))
  species_1 <- terra::rast(nrow = 1, ncol = 2, vals = c(2, 4))
  species_2 <- terra::rast(nrow = 1, ncol = 2, vals = c(10, 20))
  species <- c(species_1, species_2)
  names(species) <- c("species_2000", "species_2020")
  terra::time(species, tstep = "years") <- c(2000, 2020)

  out <- apply_elevation_mask(mask, species)

  expect_s4_class(out, "SpatRaster")
  expect_equal(names(out), names(species))
  expect_equal(terra::nlyr(out), 2)
  expect_equal(
    unname(terra::values(out, mat = TRUE)),
    matrix(c(1, 4, 5, 20), ncol = 2)
  )
  expect_equal(as.integer(as.numeric(terra::time(out))), c(2000, 2020))
})

test_that("apply_elevation_mask aligns temporal SpatRaster masks to species time", {

  skip_if_not_installed("terra")
  suppressWarnings(requireNamespace("terra", quietly = TRUE))

  mask_2000 <- terra::rast(nrow = 1, ncol = 1, vals = 0.5)
  mask_2020 <- terra::rast(nrow = 1, ncol = 1, vals = 0.25)
  mask <- c(mask_2000, mask_2020)
  terra::time(mask, tstep = "years") <- c(2000, 2020)

  species <- terra::rast(nrow = 1, ncol = 1, nlyr = 3, vals = 2)
  terra::time(species, tstep = "years") <- c(1990, 2010, 2030)

  out <- apply_elevation_mask(mask, species)

  expect_s4_class(out, "SpatRaster")
  expect_equal(as.numeric(terra::values(out, mat = TRUE)[1, ]), c(1, 1, 0.5))
  expect_equal(as.integer(as.numeric(terra::time(out))), c(1990, 2010, 2030))
})

test_that("apply_elevation_mask resamples masks to the species grid", {

  skip_if_not_installed("terra")
  suppressWarnings(requireNamespace("terra", quietly = TRUE))

  mask <- terra::rast(
    nrow = 1, ncol = 1,
    xmin = 0, xmax = 2, ymin = 0, ymax = 2,
    vals = 0.5
  )
  species <- terra::rast(
    nrow = 2, ncol = 2,
    xmin = 0, xmax = 2, ymin = 0, ymax = 2,
    vals = 2
  )

  out <- apply_elevation_mask(mask, species)

  expect_equal(terra::values(out, mat = FALSE), rep(1, 4))
})

test_that("apply_elevation_mask supports stars masks", {

  skip_if_not_installed("terra")
  skip_if_not_installed("stars")
  suppressWarnings(requireNamespace("terra", quietly = TRUE))
  suppressWarnings(requireNamespace("stars", quietly = TRUE))

  mask <- terra::rast(nrow = 1, ncol = 2, vals = c(0.5, 1))
  mask_stars <- stars::st_as_stars(mask)
  species <- terra::rast(nrow = 1, ncol = 2, vals = c(2, 4))
  species_stars <- stars::st_as_stars(species)

  out_raster <- apply_elevation_mask(mask_stars, species)
  out_stars <- apply_elevation_mask(mask_stars, species_stars)

  expect_s4_class(out_raster, "SpatRaster")
  expect_equal(terra::values(out_raster, mat = FALSE), c(1, 4))
  expect_s3_class(out_stars, "stars")
  expect_equal(as.numeric(out_stars[[1]]), c(1, 4))
})

test_that("apply_elevation_mask applies static SpatRaster masks to temporal stars projections", {

  skip_if_not_installed("terra")
  skip_if_not_installed("stars")
  suppressWarnings(requireNamespace("terra", quietly = TRUE))
  suppressWarnings(requireNamespace("stars", quietly = TRUE))

  mask <- terra::rast(nrow = 1, ncol = 2, vals = c(0.5, 1))
  species_1 <- terra::rast(nrow = 1, ncol = 2, vals = c(2, 4))
  species_2 <- terra::rast(nrow = 1, ncol = 2, vals = c(10, 20))
  species <- c(species_1, species_2)
  terra::time(species) <- as.Date(c("2000-01-01", "2020-01-01"))

  species_stars <- stars::st_as_stars(species)
  species_dims <- stars::st_dimensions(species_stars)
  names(species_dims)[3] <- "Time"
  stars::st_dimensions(species_stars) <- species_dims

  out <- apply_elevation_mask(mask, species_stars)

  expect_s3_class(out, "stars")
  expect_equal(names(stars::st_dimensions(out))[3], "time")
  expect_equal(as.numeric(out[[1]]), c(1, 4, 5, 20))
  expect_equal(
    as.character(stars::st_get_dimension_values(out, "time")),
    c("2000-01-01", "2020-01-01")
  )
})

test_that("apply_elevation_mask validates mask inputs", {

  skip_if_not_installed("terra")
  suppressWarnings(requireNamespace("terra", quietly = TRUE))

  dem <- terra::rast(nrow = 1, ncol = 2, vals = c(200, 300))
  species <- terra::rast(nrow = 1, ncol = 2, vals = 1)

  expect_error(apply_elevation_mask(dem, species), "\\[0, 1\\]")
  expect_error(apply_elevation_mask(data.frame(x = 1), species), "SpatRaster or stars")
})
