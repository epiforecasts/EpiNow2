skip_on_cran()

# Integration tests (MCMC-based) ------------------------------------------
# These tests run actual MCMC sampling and are slow.

# Helper to generate date-based linelist from primarycensored delays
generate_linelist <- function(delays, D, n = length(delays)) {
  origin <- as.Date("2023-01-01")
  pdate_lwr <- origin + sample(0:20, n, replace = TRUE)
  data.frame(
    pdate_lwr = pdate_lwr,
    sdate_lwr = pdate_lwr + delays,
    obs_date = pdate_lwr + D
  )
}

test_that("correctly recovers lognormal parameters with date input", {
  skip_if_not_installed("primarycensored")

  set.seed(123)
  true_meanlog <- log(5)
  true_sdlog <- 0.5
  D <- 30

  delays <- primarycensored::rprimarycensored(
    n = 500,
    rdist = rlnorm,
    meanlog = true_meanlog,
    sdlog = true_sdlog,
    pwindow = 1,
    swindow = 1,
    D = D
  )

  linelist <- generate_linelist(delays, D)

  result <- estimate_dist(
    linelist,
    dist = "lognormal",
    stan = stan_opts(samples = 1000, chains = 2),
    verbose = FALSE
  )

  expect_s3_class(result, "dist_spec")
  expect_equal(result$distribution, "lognormal")

  est_meanlog <- result$parameters$meanlog$parameters$mean
  est_sdlog <- result$parameters$sdlog$parameters$mean

  expect_true(abs(est_meanlog - true_meanlog) < 0.3)
  expect_true(abs(est_sdlog - true_sdlog) < 0.2)
})

test_that("correctly recovers gamma parameters with date input", {
  skip_if_not_installed("primarycensored")

  set.seed(456)
  true_shape <- 5
  true_rate <- 1
  D <- 30

  delays <- primarycensored::rprimarycensored(
    n = 500,
    rdist = rgamma,
    shape = true_shape,
    rate = true_rate,
    pwindow = 1,
    swindow = 1,
    D = D
  )

  linelist <- generate_linelist(delays, D)

  result <- estimate_dist(
    linelist,
    dist = "gamma",
    stan = stan_opts(samples = 1000, chains = 2),
    verbose = FALSE
  )

  expect_s3_class(result, "dist_spec")
  expect_equal(result$distribution, "gamma")

  est_shape <- result$parameters$shape$parameters$mean
  est_rate <- result$parameters$rate$parameters$mean

  expect_true(abs(est_shape - true_shape) < 1.5)
  expect_true(abs(est_rate - true_rate) < 0.5)
})

test_that("works as expected with numeric vector input", {
  set.seed(789)
  delays <- as.integer(rlnorm(50, log(3), 0.3))

  result <- estimate_dist(
    delays,
    dist = "lognormal",
    stan = stan_opts(samples = 500, chains = 2),
    verbose = FALSE
  )

  expect_s3_class(result, "dist_spec")
  expect_equal(result$distribution, "lognormal")
})

test_that("works as expected with expgrowth primary", {
  set.seed(101)
  origin <- as.Date("2023-01-01")
  pdate_lwr <- origin + sample(0:20, 50, replace = TRUE)
  linelist <- data.frame(
    pdate_lwr = pdate_lwr,
    sdate_lwr = pdate_lwr + rpois(50, 5)
  )

  expect_no_error(
    estimate_dist(
      linelist,
      dist = "lognormal",
      primary = "expgrowth",
      primary_params = 0.1,
      stan = stan_opts(samples = 500, chains = 2),
      verbose = FALSE
    )
  )
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
  delays <- rlnorm(50, log(5), 0.5)

  expect_error(
    estimate_dist(
      delays,
      dist = "lognormal",
      primary = "expgrowth"
    ),
    "primary_params must be a single numeric value"
  )
})

test_that("errors for unsupported distribution", {
  delays <- rlnorm(50, log(5), 0.5)

  expect_error(
    estimate_dist(delays, dist = "normal"),
    "Unsupported distribution"
  )
})

# Deprecation tests --------------------------------------------------------

test_that("estimate_delay correctly shows deprecation warning", {
  set.seed(111)
  delays <- rlnorm(30, log(5), 0.5)

  expect_warning(
    estimate_delay(delays, samples = 100, bootstraps = 1),
    "deprecated"
  )
})
