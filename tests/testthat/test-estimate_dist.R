test_that("estimate_dist recovers lognormal parameters", {
  skip_on_cran()

  set.seed(123)
  true_meanlog <- log(5)
  true_sdlog <- 0.5
  delays <- rlnorm(200, true_meanlog, true_sdlog)

 result <- estimate_dist(
    delays,
    dist = "lognormal",
    samples = 1000,
    chains = 2,
    backend = "rstan",
    verbose = FALSE
  )

  expect_s3_class(result, "dist_spec")
  expect_equal(result$distribution, "lognormal")

  # Check parameter recovery (within reasonable tolerance)
  est_meanlog <- result$parameters$meanlog$parameters$mean
  est_sdlog <- result$parameters$sdlog$parameters$mean

  expect_true(abs(est_meanlog - true_meanlog) < 0.5)
  expect_true(abs(est_sdlog - true_sdlog) < 0.3)
})

test_that("estimate_dist works with gamma distribution", {
  skip_on_cran()

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

  set.seed(789)
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

test_that("estimate_delay shows deprecation warning", {
  skip_on_cran()

  set.seed(111)
  delays <- rlnorm(30, log(5), 0.5)

  expect_warning(
    estimate_delay(delays, samples = 100, bootstraps = 1),
    "deprecated"
  )
})
