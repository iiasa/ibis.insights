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

  # Default target: 95% maturity at age 20
  expect_no_error(
    out <- insights_discount(lu, age)
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

  # Single pixel test: lu = 0.6, age = 3, target = 0.95 at age 20
  lu <- terra::rast(nrow = 1, ncol = 1, vals = 0.6)
  age <- terra::rast(nrow = 1, ncol = 1, vals = 3)
  terra::crs(lu) <- terra::crs(age) <- "EPSG:4326"

  out <- insights_discount(lu, age, target_age = 20, target = 0.95)
  expected <- 0.6 * (1 - (1 - 0.95)^(3 / 20))
  expect_equal(as.numeric(terra::values(out)[1, 1]), expected, tolerance = 1e-10)
})

test_that('insights_discount tau formula is correct', {

  skip_if_not_installed("terra")
  suppressWarnings(requireNamespace("terra", quietly = TRUE))

  lu <- terra::rast(nrow = 1, ncol = 1, vals = 0.6)
  age <- terra::rast(nrow = 1, ncol = 1, vals = 3)
  terra::crs(lu) <- terra::crs(age) <- "EPSG:4326"

  out <- insights_discount(lu, age, tau = 5)
  expected <- 0.6 * (1 - exp(-3 / 5))
  expect_equal(as.numeric(terra::values(out)[1, 1]), expected, tolerance = 1e-10)
})

test_that('insights_discount target-age and tau parameterizations agree', {

  skip_if_not_installed("terra")
  suppressWarnings(requireNamespace("terra", quietly = TRUE))

  lu <- terra::rast(nrow = 1, ncol = 1, vals = 0.6)
  age <- terra::rast(nrow = 1, ncol = 1, vals = 3)
  terra::crs(lu) <- terra::crs(age) <- "EPSG:4326"

  target_age <- 20
  target <- 0.95
  tau <- -target_age / log(1 - target)

  out_target <- insights_discount(lu, age, target_age = target_age, target = target)
  out_tau <- insights_discount(lu, age, tau = tau)

  expect_equal(
    as.numeric(terra::values(out_tau)[1, 1]),
    as.numeric(terra::values(out_target)[1, 1]),
    tolerance = 1e-10
  )
})

test_that('insights_discount smoothed-threshold formula is correct', {

  skip_if_not_installed("terra")
  suppressWarnings(requireNamespace("terra", quietly = TRUE))

  lu <- terra::rast(nrow = 1, ncol = 1, vals = 0.6)
  age <- terra::rast(nrow = 1, ncol = 1, vals = 8)
  terra::crs(lu) <- terra::crs(age) <- "EPSG:4326"

  out <- insights_discount(lu, age, a50 = 5, k = 0.7)
  expected <- 0.6 * (1 / (1 + exp(-0.7 * (8 - 5))))
  expect_equal(as.numeric(terra::values(out)[1, 1]), expected, tolerance = 1e-10)
})

test_that('insights_discount smoothed threshold is 0.5 at a50', {

  skip_if_not_installed("terra")
  suppressWarnings(requireNamespace("terra", quietly = TRUE))

  lu <- terra::rast(nrow = 1, ncol = 1, vals = 0.6)
  age <- terra::rast(nrow = 1, ncol = 1, vals = 5)
  terra::crs(lu) <- terra::crs(age) <- "EPSG:4326"

  out <- insights_discount(lu, age, a50 = 5, k = 0.7)
  expect_equal(as.numeric(terra::values(out)[1, 1]), 0.3, tolerance = 1e-10)
})

test_that('insights_discount at age=0 gives zero', {

  skip_if_not_installed("terra")
  suppressWarnings(requireNamespace("terra", quietly = TRUE))

  lu <- terra::rast(nrow = 1, ncol = 1, vals = 0.8)
  age <- terra::rast(nrow = 1, ncol = 1, vals = 0)
  terra::crs(lu) <- terra::crs(age) <- "EPSG:4326"

  out <- insights_discount(lu, age, target_age = 20, target = 0.95)
  # At age=0: factor = 1 - (1 - target)^0 = 1 - 1 = 0
  expect_equal(as.numeric(terra::values(out)[1, 1]), 0, tolerance = 1e-10)
})

test_that('insights_discount at high age gives near-full value', {

  skip_if_not_installed("terra")
  suppressWarnings(requireNamespace("terra", quietly = TRUE))

  lu <- terra::rast(nrow = 1, ncol = 1, vals = 0.8)
  age <- terra::rast(nrow = 1, ncol = 1, vals = 100)
  terra::crs(lu) <- terra::crs(age) <- "EPSG:4326"

  out <- insights_discount(lu, age, target_age = 20, target = 0.95)
  # At age=100: factor = 1 - (1 - 0.95)^5 is near 1.0
  expect_equal(as.numeric(terra::values(out)[1, 1]), 0.8, tolerance = 1e-6)
})

test_that('insights_discount validates inputs', {

  skip_if_not_installed("terra")
  suppressWarnings(requireNamespace("terra", quietly = TRUE))

  lu <- terra::rast(nrow = 10, ncol = 10, vals = runif(100))
  age <- terra::rast(nrow = 10, ncol = 10, vals = runif(100, 0, 10))
  terra::crs(lu) <- terra::crs(age) <- "EPSG:4326"

  # target age and target out of range
  expect_error(insights_discount(lu, age, target_age = 0))
  expect_error(insights_discount(lu, age, target_age = -1))
  expect_error(insights_discount(lu, age, target = 0))
  expect_error(insights_discount(lu, age, target = 1))
  expect_error(insights_discount(lu, age, target = 1.5))
  expect_error(insights_discount(lu, age, tau = 0))
  expect_error(insights_discount(lu, age, tau = -1))
  expect_error(insights_discount(lu, age, a50 = 5))
  expect_error(insights_discount(lu, age, k = 0.7))
  expect_error(insights_discount(lu, age, a50 = -1, k = 0.7))
  expect_error(insights_discount(lu, age, a50 = 5, k = 0))
  expect_error(insights_discount(lu, age, tau = 5, a50 = 5, k = 0.7))

  # Mismatched number of layers
  lu2 <- c(lu, lu)
  expect_error(insights_discount(lu2, age, target_age = 20, target = 0.95))

  # Negative age values
  age_neg <- terra::rast(nrow = 10, ncol = 10, vals = runif(100, -5, 5))
  terra::crs(age_neg) <- "EPSG:4326"
  expect_error(insights_discount(lu, age_neg, target_age = 20, target = 0.95))
})

test_that('insights_discount smaller target_age gives faster accumulation', {

  skip_if_not_installed("terra")
  suppressWarnings(requireNamespace("terra", quietly = TRUE))

  lu <- terra::rast(nrow = 1, ncol = 1, vals = 1.0)
  age <- terra::rast(nrow = 1, ncol = 1, vals = 2)
  terra::crs(lu) <- terra::crs(age) <- "EPSG:4326"

  out_fast <- insights_discount(lu, age, target_age = 5, target = 0.95)
  out_slow <- insights_discount(lu, age, target_age = 30, target = 0.95)

  # Smaller target age => higher effective value at the same cell age.
  expect_gt(terra::values(out_fast)[1, 1], terra::values(out_slow)[1, 1])
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
  lu_disc <- insights_discount(lu, age, target_age = 20, target = 0.95)
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
    out <- insights_discount(lu, age, target_age = 20, target = 0.95)
  )
  expect_equal(terra::nlyr(out), n)
  expect_equal(terra::time(out), terra::time(lu))
})
