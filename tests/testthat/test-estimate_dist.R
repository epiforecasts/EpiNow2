test_that("estimate_dist works with lognormal distribution", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  skip_if_not_installed("primarycensored")

  set.seed(123)
  delays <- rlnorm(100, log(5), 0.5)

  result <- estimate_dist(
    delays,
    dist = "lognormal",
    samples = 500,  # Small for testing
    chains = 2,
    backend = "rstan",
    verbose = FALSE
  )

  expect_s3_class(result, "dist_spec")
  expect_equal(result$distribution, "lognormal")
  expect_true("meanlog" %in% names(result$parameters))
  expect_true("sdlog" %in% names(result$parameters))

  # Check parameters are dist_spec with numeric values
  expect_s3_class(result$parameters$meanlog, "dist_spec")
  expect_s3_class(result$parameters$sdlog, "dist_spec")
  expect_true(result$parameters$sdlog$parameters$mean > 0)  # sdlog must be positive
})

test_that("estimate_dist works with gamma distribution", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  skip_if_not_installed("primarycensored")

  set.seed(456)
  delays <- rgamma(100, shape = 5, rate = 1)

  result <- estimate_dist(
    delays,
    dist = "gamma",
    samples = 500,
    chains = 2,
    backend = "rstan",
    verbose = FALSE
  )

  expect_s3_class(result, "dist_spec")
  expect_equal(result$distribution, "gamma")
  expect_true("shape" %in% names(result$parameters))
  expect_true("rate" %in% names(result$parameters))
})

test_that("estimate_dist works with data frame input", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  skip_if_not_installed("primarycensored")

  set.seed(789)

  # Create interval-censored data
  delays <- rlnorm(50, log(3), 0.3)
  delay_df <- data.frame(
    delay = floor(delays),
    delay_upper = floor(delays) + 1,
    n = 1
  )

  result <- estimate_dist(
    delay_df,
    dist = "lognormal",
    samples = 500,
    chains = 2,
    backend = "rstan",
    verbose = FALSE
  )

  expect_s3_class(result, "dist_spec")
  expect_equal(result$distribution, "lognormal")
})

test_that("estimate_dist errors for unsupported distribution", {
  delays <- rlnorm(50, log(5), 0.5)

  expect_error(
    estimate_dist(delays, dist = "normal"),
    "Unsupported distribution"
  )
})

test_that("estimate_dist errors for invalid data frame", {
  bad_df <- data.frame(
    x = 1:10,
    y = 11:20
  )

  expect_error(
    estimate_dist(bad_df, dist = "lognormal"),
    "must have columns: delay, delay_upper"
  )
})

test_that("estimate_dist handles max_value parameter", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  skip_if_not_installed("primarycensored")

  set.seed(321)
  delays <- rlnorm(50, log(5), 0.5)

  result <- estimate_dist(
    delays,
    dist = "lognormal",
    max_value = 30,
    samples = 500,
    chains = 2,
    backend = "rstan",
    verbose = FALSE
  )

  expect_s3_class(result, "dist_spec")
  expect_equal(max(result), 30)
})

test_that(".prepare_delay_intervals converts vector correctly", {
  delays <- c(1, 2, 2, 3, 3, 3, 5)

  result <- .prepare_delay_intervals(delays, verbose = FALSE)

  expect_s3_class(result, "data.frame")
  expect_true(all(c("delay", "delay_upper", "n") %in% names(result)))
  expect_equal(sum(result$n), length(delays))
  expect_equal(result$delay_upper, result$delay + 1)
})

test_that(".prepare_delay_intervals handles NA and negative values", {
  delays <- c(1, 2, NA, -1, 3, 4)

  result <- .prepare_delay_intervals(delays, verbose = FALSE)

  # Should remove NA and negative values
  expect_equal(sum(result$n), 4)  # Only 1, 2, 3, 4
})

test_that(".get_param_bounds_auto returns sensible bounds", {
  delay_data <- data.frame(
    delay = c(1, 2, 3, 4, 5),
    delay_upper = c(2, 3, 4, 5, 6),
    n = c(1, 2, 3, 2, 1)
  )

  # Lognormal
  bounds_ln <- .get_param_bounds_auto(delay_data, "lognormal")
  expect_true(all(c("lower", "upper") %in% names(bounds_ln)))
  expect_length(bounds_ln$lower, 2)
  expect_length(bounds_ln$upper, 2)
  expect_true(bounds_ln$lower[2] > 0)  # sdlog must be positive

  # Gamma
  bounds_gamma <- .get_param_bounds_auto(delay_data, "gamma")
  expect_true(all(bounds_gamma$lower > 0))  # Both must be positive
  expect_true(all(bounds_gamma$upper > bounds_gamma$lower))

  # Weibull
  bounds_weibull <- .get_param_bounds_auto(delay_data, "weibull")
  expect_true(all(bounds_weibull$lower > 0))
  expect_true(all(bounds_weibull$upper > bounds_weibull$lower))
})

test_that("estimate_delay shows deprecation warning", {
  skip_on_cran()
  skip_if_not_installed("rstan")

  set.seed(111)
  delays <- rlnorm(30, log(5), 0.5)

  # Should show deprecation warning
  expect_warning(
    estimate_delay(delays, samples = 100, bootstraps = 1),
    "deprecated"
  )
})
