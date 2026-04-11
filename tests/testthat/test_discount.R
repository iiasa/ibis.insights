# Tests for insights_discount (temporal discount for land-use based on age variable)

test_that('insights_discount works with SpatRaster inputs', {

  skip_if_not_installed("terra")
  suppressWarnings(requireNamespace("terra", quietly = TRUE))

  # Create land-use and age layers (3 timesteps)
  set.seed(42)
  lu_vals <- runif(100, 0, 0.5)
  lu1 <- terra::rast(nrow = 10, ncol = 10, vals = lu_vals)
  lu2 <- terra::rast(nrow = 10, ncol = 10, vals = lu_vals + 0.1)
  lu3 <- terra::rast(nrow = 10, ncol = 10, vals = lu_vals + 0.2)
  terra::crs(lu1) <- terra::crs(lu2) <- terra::crs(lu3) <- "EPSG:4326"
  terra::time(lu1) <- as.Date("2020-01-01")
  terra::time(lu2) <- as.Date("2030-01-01")
  terra::time(lu3) <- as.Date("2040-01-01")
  lu <- c(lu1, lu2, lu3)

  # Age: 0 = brand new, increasing over time
  age_vals <- runif(100, 0, 10)
  age1 <- terra::rast(nrow = 10, ncol = 10, vals = age_vals)
  age2 <- terra::rast(nrow = 10, ncol = 10, vals = age_vals + 5)
  age3 <- terra::rast(nrow = 10, ncol = 10, vals = age_vals + 10)
  terra::crs(age1) <- terra::crs(age2) <- terra::crs(age3) <- "EPSG:4326"
  terra::time(age1) <- as.Date("2020-01-01")
  terra::time(age2) <- as.Date("2030-01-01")
  terra::time(age3) <- as.Date("2040-01-01")
  age <- c(age1, age2, age3)

  # Default discount = 0.5
  expect_no_error(
    out <- insights_discount(lu, age, discount = 0.5)
  )
  expect_s4_class(out, "SpatRaster")
  expect_equal(terra::nlyr(out), 3)

  # Discounted values should be <= lu values (factor in [0,1])
  expect_true(
    all(terra::values(out) <= terra::values(lu) + 1e-10, na.rm = TRUE)
  )
  # Discounted values should be >= 0
  expect_true(
    all(terra::values(out) >= -1e-10, na.rm = TRUE)
  )

  # Time attributes preserved
  expect_equal(terra::time(out), terra::time(lu))
})

test_that('insights_discount formula is correct', {

  skip_if_not_installed("terra")
  suppressWarnings(requireNamespace("terra", quietly = TRUE))

  # Single pixel test: lu = 0.6, age = 3, discount = 0.5
  lu <- terra::rast(nrow = 1, ncol = 1, vals = 0.6)
  age <- terra::rast(nrow = 1, ncol = 1, vals = 3)
  terra::crs(lu) <- terra::crs(age) <- "EPSG:4326"

  out <- insights_discount(lu, age, discount = 0.5)
  # Expected: 0.6 * (1 - (1-0.5)^3) = 0.6 * (1 - 0.125) = 0.6 * 0.875 = 0.525
  expect_equal(terra::values(out)[1, 1], 0.525, tolerance = 1e-10)
})

test_that('insights_discount at age=0 gives zero', {

  skip_if_not_installed("terra")
  suppressWarnings(requireNamespace("terra", quietly = TRUE))

  lu <- terra::rast(nrow = 1, ncol = 1, vals = 0.8)
  age <- terra::rast(nrow = 1, ncol = 1, vals = 0)
  terra::crs(lu) <- terra::crs(age) <- "EPSG:4326"

  out <- insights_discount(lu, age, discount = 0.5)
  # At age=0: factor = 1 - (1-0.5)^0 = 1 - 1 = 0
  expect_equal(terra::values(out)[1, 1], 0, tolerance = 1e-10)
})

test_that('insights_discount at high age gives near-full value', {

  skip_if_not_installed("terra")
  suppressWarnings(requireNamespace("terra", quietly = TRUE))

  lu <- terra::rast(nrow = 1, ncol = 1, vals = 0.8)
  age <- terra::rast(nrow = 1, ncol = 1, vals = 50)
  terra::crs(lu) <- terra::crs(age) <- "EPSG:4326"

  out <- insights_discount(lu, age, discount = 0.5)
  # At age=50: factor = 1 - (0.5)^50 ≈ 1.0
  expect_equal(terra::values(out)[1, 1], 0.8, tolerance = 1e-6)
})

test_that('insights_discount validates inputs', {

  skip_if_not_installed("terra")
  suppressWarnings(requireNamespace("terra", quietly = TRUE))

  lu <- terra::rast(nrow = 10, ncol = 10, vals = runif(100))
  age <- terra::rast(nrow = 10, ncol = 10, vals = runif(100, 0, 10))
  terra::crs(lu) <- terra::crs(age) <- "EPSG:4326"

  # discount out of range
  expect_error(insights_discount(lu, age, discount = 0))
  expect_error(insights_discount(lu, age, discount = 1))
  expect_error(insights_discount(lu, age, discount = -0.5))
  expect_error(insights_discount(lu, age, discount = 1.5))

  # Mismatched number of layers
  lu2 <- c(lu, lu)
  expect_error(insights_discount(lu2, age, discount = 0.5))

  # Negative age values
  age_neg <- terra::rast(nrow = 10, ncol = 10, vals = runif(100, -5, 5))
  terra::crs(age_neg) <- "EPSG:4326"
  expect_error(insights_discount(lu, age_neg, discount = 0.5))
})

test_that('insights_discount high discount gives faster accumulation', {

  skip_if_not_installed("terra")
  suppressWarnings(requireNamespace("terra", quietly = TRUE))

  lu <- terra::rast(nrow = 1, ncol = 1, vals = 1.0)
  age <- terra::rast(nrow = 1, ncol = 1, vals = 2)
  terra::crs(lu) <- terra::crs(age) <- "EPSG:4326"

  out_high <- insights_discount(lu, age, discount = 0.9)
  out_low  <- insights_discount(lu, age, discount = 0.1)

  # Higher discount => higher effective value (faster accumulation)
  expect_gt(terra::values(out_high)[1, 1], terra::values(out_low)[1, 1])
})

test_that('insights_discount integrates with insights_fraction', {

  skip_if_not_installed("terra")
  suppressWarnings(requireNamespace("terra", quietly = TRUE))

  set.seed(7)
  range <- terra::rast(nrow = 10, ncol = 10,
                       vals = stats::rbinom(100, 1, 0.5))
  terra::crs(range) <- "EPSG:4326"

  lu <- terra::rast(nrow = 10, ncol = 10, vals = runif(100, 0, 0.5))
  age <- terra::rast(nrow = 10, ncol = 10, vals = runif(100, 0, 20))
  terra::crs(lu) <- terra::crs(age) <- "EPSG:4326"

  # Discount and then apply to insights_fraction
  lu_disc <- insights_discount(lu, age, discount = 0.5)
  expect_no_error(
    out <- insights_fraction(range = range, lu = lu_disc)
  )
  expect_s4_class(out, "SpatRaster")
})

test_that('insights_discount multi-layer with progress bar', {

  skip_if_not_installed("terra")
  suppressWarnings(requireNamespace("terra", quietly = TRUE))

  set.seed(99)
  n <- 5
  lu_list <- lapply(1:n, function(i)  {
    r <- terra::rast(nrow = 10, ncol = 10, vals = runif(100, 0, 0.5))
    terra::crs(r) <- "EPSG:4326"
    terra::time(r) <- as.Date("2020-01-01") + (i - 1) * 365 * 10
    r
  })
  age_list <- lapply(1:n, function(i) {
    r <- terra::rast(nrow = 10, ncol = 10, vals = runif(100, 0, 5) + (i - 1) * 2)
    terra::crs(r) <- "EPSG:4326"
    terra::time(r) <- as.Date("2020-01-01") + (i - 1) * 365 * 10
    r
  })
  lu <- do.call(c, lu_list)
  age <- do.call(c, age_list)

  expect_no_error(
    out <- insights_discount(lu, age, discount = 0.4)
  )
  expect_equal(terra::nlyr(out), n)
  expect_equal(terra::time(out), terra::time(lu))
})
