test_that("correctly recovers lognormal parameters", {
  skip_on_cran()
  skip_if_not_installed("primarycensored")

  set.seed(123)
  true_meanlog <- log(5)
  true_sdlog <- 0.5
  D <- 30

  # Use primarycensored simulator for proper censored data
  delays <- primarycensored::rprimarycensored(
    n = 500,
    rdist = rlnorm,
    meanlog = true_meanlog,
    sdlog = true_sdlog,
    pwindow = 1,
    swindow = 1,
    D = D
  )

  result <- estimate_dist(
    delays,
    dist = "lognormal",
    stan = stan_opts(samples = 1000, chains = 2),
    truncation_time = D,
    verbose = FALSE
  )

  expect_s3_class(result, "dist_spec")
  expect_equal(result$distribution, "lognormal")

  # Check parameter recovery (within reasonable tolerance)
  est_meanlog <- result$parameters$meanlog$parameters$mean
  est_sdlog <- result$parameters$sdlog$parameters$mean

  expect_true(abs(est_meanlog - true_meanlog) < 0.3)
  expect_true(abs(est_sdlog - true_sdlog) < 0.2)
})

test_that("correctly recovers gamma parameters", {
  skip_on_cran()
  skip_if_not_installed("primarycensored")

  set.seed(456)
  true_shape <- 5
  true_rate <- 1
  D <- 30

  # Use primarycensored simulator for proper censored data
  delays <- primarycensored::rprimarycensored(
    n = 500,
    rdist = rgamma,
    shape = true_shape,
    rate = true_rate,
    pwindow = 1,
    swindow = 1,
    D = D
  )

  result <- estimate_dist(
    delays,
    dist = "gamma",
    stan = stan_opts(samples = 1000, chains = 2),
    truncation_time = D,
    verbose = FALSE
  )

  expect_s3_class(result, "dist_spec")
  expect_equal(result$distribution, "gamma")

  est_shape <- result$parameters$shape$parameters$mean
  est_rate <- result$parameters$rate$parameters$mean

  expect_true(abs(est_shape - true_shape) < 1.5)
  expect_true(abs(est_rate - true_rate) < 0.5)
})

test_that("works as expected with data frame input", {
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
    stan = stan_opts(samples = 500, chains = 2),
    verbose = FALSE
  )

  expect_s3_class(result, "dist_spec")
  expect_equal(result$distribution, "lognormal")
})

test_that("errors for bad distribution specifications", {
  delays <- rlnorm(50, log(5), 0.5)

  expect_error(
    estimate_dist(delays, dist = "normal"),
    "Unsupported distribution"
  )
})

test_that("errors for bad data frame specifications", {
  bad_df <- data.frame(
    x = 1:10,
    y = 11:20
  )

  expect_error(
    estimate_dist(bad_df, dist = "lognormal"),
    "must have columns: delay, delay_upper"
  )
})

test_that("correctly handles max_value parameter", {
  skip_on_cran()

  set.seed(321)
  delays <- rlnorm(50, log(5), 0.5)

  result <- estimate_dist(
    delays,
    dist = "lognormal",
    max_value = 30,
    stan = stan_opts(samples = 500, chains = 2),
    verbose = FALSE
  )

  expect_s3_class(result, "dist_spec")
  expect_equal(max(result), 30)
})

test_that("estimate_delay correctly shows deprecation warning", {
  skip_on_cran()

  set.seed(111)
  delays <- rlnorm(30, log(5), 0.5)

  expect_warning(
    estimate_delay(delays, samples = 100, bootstraps = 1),
    "deprecated"
  )
})
