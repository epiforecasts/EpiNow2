skip_on_cran()

# Integration tests (MCMC-based) ------------------------------------------
# These tests run actual MCMC sampling and are slow.

# Helper to simulate delays and build a date-based linelist
generate_linelist <- function(
    n = 500, rdist = rlnorm, D = 30,
    pwindow = 1, swindow = 1,
    rprimary = stats::runif, rprimary_args = list(),
    weight = 1,
    ...) {
  delays <- primarycensored::rprimarycensored(
    n = n, rdist = rdist,
    pwindow = pwindow, swindow = swindow, D = D,
    rprimary = rprimary, rprimary_args = rprimary_args,
    ...
  )
  origin <- as.Date("2023-01-01")
  pdate_lwr <- origin + sample(0:20, n, replace = TRUE)
  data.frame(
    pdate_lwr = pdate_lwr,
    sdate_lwr = pdate_lwr + delays,
    obs_date = pdate_lwr + D,
    n = weight
  )
}

test_that("correctly recovers normal parameters with date input", {
  skip_if_not_installed("primarycensored")

  set.seed(321)
  true_mean <- 10
  true_sd <- 2

  linelist <- generate_linelist(
    n = 1000, rdist = stats::rnorm,
    mean = true_mean, sd = true_sd
  )

  result <- suppressWarnings(suppressMessages(estimate_dist(
    linelist,
    dist = "normal",
    stan = stan_opts(samples = 1000, chains = 2),
    verbose = FALSE
  )))

  params <- get_parameters(result)$delay
  expect_equal(params$distribution, "normal")

  est_mean <- params$parameters$mean$parameters$mean
  est_sd <- params$parameters$sd$parameters$mean

  expect_true(abs(est_mean - true_mean) < 0.5)
  expect_true(abs(est_sd - true_sd) < 0.5)
})

test_that("correctly recovers exponential parameters with date input", {
  skip_if_not_installed("primarycensored")

  set.seed(654)
  true_rate <- 0.2

  linelist <- generate_linelist(
    n = 1000, rdist = stats::rexp,
    rate = true_rate
  )

  result <- suppressWarnings(suppressMessages(estimate_dist(
    linelist,
    dist = "exp",
    stan = stan_opts(samples = 1000, chains = 2),
    verbose = FALSE
  )))

  params <- get_parameters(result)$delay
  expect_equal(params$distribution, "exp")

  est_rate <- params$parameters$rate$parameters$mean

  expect_true(abs(est_rate - true_rate) < 0.05)
})

test_that("correctly recovers weibull parameters with date input", {
  skip_if_not_installed("primarycensored")

  set.seed(987)
  true_shape <- 2
  true_scale <- 10

  linelist <- generate_linelist(
    n = 1000, rdist = stats::rweibull,
    shape = true_shape, scale = true_scale
  )

  result <- suppressWarnings(suppressMessages(estimate_dist(
    linelist,
    dist = "weibull",
    stan = stan_opts(samples = 1000, chains = 2),
    verbose = FALSE
  )))

  params <- get_parameters(result)$delay
  expect_equal(params$distribution, "weibull")

  est_shape <- params$parameters$shape$parameters$mean
  est_scale <- params$parameters$scale$parameters$mean

  expect_true(abs(est_shape - true_shape) < 0.5)
  expect_true(abs(est_scale - true_scale) < 1.0)
})

test_that("correctly weights observations with the n column", {
  skip_if_not_installed("primarycensored")

  set.seed(246)
  true_meanlog <- log(5)
  true_sdlog <- 0.5

  # Simulate 100 unique observations, each with weight 10 (1000 total)
  linelist <- generate_linelist(
    n = 100, rdist = rlnorm,
    meanlog = true_meanlog, sdlog = true_sdlog,
    weight = 10
  )

  result <- suppressWarnings(suppressMessages(estimate_dist(
    linelist,
    dist = "lognormal",
    stan = stan_opts(samples = 1000, chains = 2),
    verbose = FALSE
  )))

  params <- get_parameters(result)$delay
  est_meanlog <- params$parameters$meanlog$parameters$mean
  est_sdlog <- params$parameters$sdlog$parameters$mean

  expect_true(abs(est_meanlog - true_meanlog) < 0.3)
  expect_true(abs(est_sdlog - true_sdlog) < 0.2)
})

test_that("correctly handles obs_time_threshold", {
  skip_if_not_installed("primarycensored")

  set.seed(135)
  true_meanlog <- log(5)
  true_sdlog <- 0.5

  # Simulate data where some observations are very far from truncation
  # D is 60, but max delay is much smaller
  linelist <- generate_linelist(
    n = 200, rdist = rlnorm, D = 60,
    meanlog = true_meanlog, sdlog = true_sdlog
  )

  # With obs_time_threshold = 1.1, many should be set to Inf
  # Max delay is around 15-20, so threshold is ~22. D=60 is > 22.
  result <- suppressWarnings(suppressMessages(estimate_dist(
    linelist,
    dist = "lognormal",
    obs_time_threshold = 1.1,
    stan = stan_opts(samples = 1000, chains = 2),
    verbose = FALSE
  )))

  expect_s3_class(result, "estimate_dist")
  # Verify that some observations in the Stan data have D = Inf
  # (which is passed as a very large number or handled in the model)
  # Actually in R code we set it to Inf.
  expect_true(any(is.infinite(result$args$D)))

  params <- get_parameters(result)$delay
  expect_true(abs(params$parameters$meanlog$parameters$mean - true_meanlog) < 0.3)
})

test_that("correctly recovers lognormal parameters with date input", {
  skip_if_not_installed("primarycensored")

  set.seed(123)
  true_meanlog <- log(5)
  true_sdlog <- 0.5

  linelist <- generate_linelist(
    n = 500, rdist = rlnorm,
    meanlog = true_meanlog, sdlog = true_sdlog
  )

  result <- suppressWarnings(suppressMessages(estimate_dist(
    linelist,
    dist = "lognormal",
    stan = stan_opts(samples = 1000, chains = 2),
    verbose = FALSE
  )))

  expect_s3_class(result, "estimate_dist")
  expect_s3_class(result, "epinowfit")
  expect_true(!is.null(result$fit))
  expect_true(!is.null(result$args))
  expect_true(!is.null(result$data))

  params <- get_parameters(result)
  expect_true("delay" %in% names(params))
  dist_spec <- params$delay
  expect_s3_class(dist_spec, "dist_spec")
  expect_equal(dist_spec$distribution, "lognormal")

  est_meanlog <- dist_spec$parameters$meanlog$parameters$mean
  est_sdlog <- dist_spec$parameters$sdlog$parameters$mean

  expect_true(abs(est_meanlog - true_meanlog) < 0.3)
  expect_true(abs(est_sdlog - true_sdlog) < 0.2)
})

test_that("correctly recovers gamma parameters with date input", {
  skip_if_not_installed("primarycensored")

  set.seed(456)
  true_shape <- 5
  true_rate <- 1

  linelist <- generate_linelist(
    n = 500, rdist = rgamma,
    shape = true_shape, rate = true_rate
  )

  result <- suppressWarnings(suppressMessages(estimate_dist(
    linelist,
    dist = "gamma",
    stan = stan_opts(samples = 1000, chains = 2),
    verbose = FALSE
  )))

  expect_s3_class(result, "estimate_dist")
  expect_s3_class(result, "epinowfit")

  params <- get_parameters(result)
  dist_spec <- params$delay
  expect_s3_class(dist_spec, "dist_spec")
  expect_equal(dist_spec$distribution, "gamma")

  est_shape <- dist_spec$parameters$shape$parameters$mean
  est_rate <- dist_spec$parameters$rate$parameters$mean

  expect_true(abs(est_shape - true_shape) < 0.75)
  expect_true(abs(est_rate - true_rate) < 0.3)
})

test_that("correctly recovers gamma parameters with expgrowth primary", {
  skip_if_not_installed("primarycensored")

  set.seed(202)
  true_shape <- 5
  true_rate <- 1
  true_r <- -0.1

  linelist <- generate_linelist(
    n = 1000, rdist = stats::rgamma,
    shape = true_shape, rate = true_rate,
    rprimary = primarycensored::rexpgrowth,
    rprimary_args = list(r = true_r)
  )

  result <- suppressWarnings(suppressMessages(estimate_dist(
    linelist,
    dist = "gamma",
    primary = "expgrowth",
    primary_params = true_r,
    stan = stan_opts(samples = 1000, chains = 2),
    verbose = FALSE
  )))

  params <- get_parameters(result)$delay
  expect_equal(params$distribution, "gamma")

  est_shape <- params$parameters$shape$parameters$mean
  est_rate <- params$parameters$rate$parameters$mean

  expect_true(abs(est_shape - true_shape) < 1.0)
  expect_true(abs(est_rate - true_rate) < 0.5)
})

test_that("correctly handles varying censoring windows", {
  skip_if_not_installed("primarycensored")

  set.seed(789)
  n <- 500
  true_meanlog <- 1.5
  true_sdlog <- 0.75

  pwindows <- sample.int(2, n, replace = TRUE)
  swindows <- sample.int(2, n, replace = TRUE)
  obs_times <- sample(20:30, n, replace = TRUE)

  delays <- mapply(
    function(pw, sw, D) {
      primarycensored::rprimarycensored(
        1, rlnorm,
        meanlog = true_meanlog, sdlog = true_sdlog,
        pwindow = pw, swindow = sw, D = D
      )
    },
    pwindows, swindows, obs_times
  )

  # Filter observations not yet fully observed
  keep <- obs_times >= delays + swindows
  delays <- delays[keep]
  pwindows <- pwindows[keep]
  swindows <- swindows[keep]
  obs_times <- obs_times[keep]

  pdate_lwr <- as.Date("2023-01-01") +
    sample(0:13, sum(keep), replace = TRUE)

  linelist <- data.frame(
    pdate_lwr = pdate_lwr,
    pdate_upr = pdate_lwr + pwindows,
    sdate_lwr = pdate_lwr + delays,
    sdate_upr = pdate_lwr + delays + swindows,
    obs_date = pdate_lwr + obs_times
  )

  result <- suppressWarnings(suppressMessages(estimate_dist(
    linelist,
    dist = "lognormal",
    stan = stan_opts(samples = 1000, chains = 2),
    verbose = FALSE
  )))

  expect_s3_class(result, "estimate_dist")
  params <- get_parameters(result)
  dist_spec <- params$delay
  expect_equal(dist_spec$distribution, "lognormal")

  est_meanlog <- dist_spec$parameters$meanlog$parameters$mean
  est_sdlog <- dist_spec$parameters$sdlog$parameters$mean

  expect_true(abs(est_meanlog - true_meanlog) < 0.3)
  expect_true(abs(est_sdlog - true_sdlog) < 0.2)
})

test_that("errors for numeric vector input", {
  delays <- as.integer(rlnorm(50, log(3), 0.3))

  expect_error(
    estimate_dist(delays, dist = "lognormal"),
    "data.frame"
  )
})

test_that("correctly recovers parameters with expgrowth primary", {
  skip_if_not_installed("primarycensored")

  set.seed(101)
  true_meanlog <- log(5)
  true_sdlog <- 0.5
  true_r <- 0.2

  linelist <- generate_linelist(
    n = 500, rdist = rlnorm,
    meanlog = true_meanlog, sdlog = true_sdlog,
    rprimary = primarycensored::rexpgrowth,
    rprimary_args = list(r = true_r)
  )

  result <- suppressWarnings(suppressMessages(estimate_dist(
    linelist,
    dist = "lognormal",
    primary = "expgrowth",
    primary_params = true_r,
    stan = stan_opts(samples = 1000, chains = 2),
    verbose = FALSE
  )))

  expect_s3_class(result, "estimate_dist")

  params <- get_parameters(result)
  dist_spec <- params$delay
  expect_equal(dist_spec$distribution, "lognormal")

  est_meanlog <- dist_spec$parameters$meanlog$parameters$mean
  est_sdlog <- dist_spec$parameters$sdlog$parameters$mean

  expect_true(abs(est_meanlog - true_meanlog) < 0.3)
  expect_true(abs(est_sdlog - true_sdlog) < 0.2)
})

# Input validation tests ---------------------------------------------------

test_that("errors for missing required columns", {
  bad_df <- data.frame(
    x = as.Date("2023-01-01") + 1:10,
    y = as.Date("2023-01-11") + 1:10
  )

  expect_error(
    estimate_dist(bad_df, dist = "lognormal"),
    "must have columns"
  )
})

test_that("errors for negative delays", {
  origin <- as.Date("2023-01-01")
  linelist <- data.frame(
    pdate_lwr = origin + 5:9,
    sdate_lwr = origin + 0:4,
    obs_date = origin + 30
  )

  expect_error(
    estimate_dist(linelist, dist = "lognormal"),
    "negative delay"
  )
})

test_that("errors when obs_date is earlier than sdate_upr", {
  origin <- as.Date("2023-01-01")
  linelist <- data.frame(
    pdate_lwr = origin + 0:4,
    sdate_lwr = origin + 5:9,
    obs_date = origin + 3:7
  )

  expect_error(
    estimate_dist(linelist, dist = "lognormal"),
    "obs_date.*earlier than sdate_upr"
  )
})

test_that("errors for expgrowth without primary_params", {
  origin <- as.Date("2023-01-01")
  linelist <- data.frame(
    pdate_lwr = origin + 0:4,
    sdate_lwr = origin + 5:9
  )

  expect_error(
    estimate_dist(
      linelist,
      dist = "lognormal",
      primary = "expgrowth"
    ),
    "primary_params must be a single numeric value"
  )
})

test_that("correctly handles constant delays without non-finite init", {
  skip_if_not_installed("primarycensored")

  origin <- as.Date("2023-01-01")
  linelist <- data.frame(
    pdate_lwr = origin + 0:9,
    sdate_lwr = origin + 0:9 + 3L,
    obs_date = origin + 30
  )

  expect_no_error(
    suppressWarnings(suppressMessages(estimate_dist(
      linelist,
      dist = "lognormal",
      stan = stan_opts(samples = 100, chains = 1, warmup = 100),
      verbose = FALSE
    )))
  )
})

test_that("errors for unsupported distribution", {
  origin <- as.Date("2023-01-01")
  linelist <- data.frame(
    pdate_lwr = origin + 0:4,
    sdate_lwr = origin + 5:9
  )

  expect_error(
    estimate_dist(linelist, dist = "cauchy"),
    "Unsupported distribution"
  )
})

test_that("errors for unsupported primary distribution", {
  origin <- as.Date("2023-01-01")
  linelist <- data.frame(
    pdate_lwr = origin + 0:4,
    sdate_lwr = origin + 5:9
  )

  expect_error(
    estimate_dist(linelist, primary = "weibull"),
    "Unsupported primary distribution"
  )
})

test_that("errors for invalid prior names", {
  origin <- as.Date("2023-01-01")
  linelist <- data.frame(
    pdate_lwr = origin + 0:4,
    sdate_lwr = origin + 5:9
  )

  expect_error(
    estimate_dist(
      linelist,
      dist = "lognormal",
      priors = list(mu = Normal(1, 1), sigma = Normal(0.5, 0.5))
    ),
    "Invalid prior names"
  )
})

test_that("negative delays error via prepare_linelist_data", {
  origin <- as.Date("2023-01-01")
  linelist <- data.frame(
    pdate_lwr = origin + c(5, 6, 0, 1, 2),
    sdate_lwr = origin + c(2, 3, 5, 6, 7),
    obs_date = origin + 30
  )

  expect_error(
    EpiNow2:::.prepare_linelist_data(linelist),
    "negative delay"
  )
})

# Deprecation tests --------------------------------------------------------

test_that("estimate_delay correctly shows deprecation warning", {
  set.seed(111)
  delays <- rlnorm(30, log(5), 0.5)

  expect_warning(
    suppressMessages(
      estimate_delay(delays, samples = 100, bootstraps = 1)
    ),
    "deprecated"
  )
})
