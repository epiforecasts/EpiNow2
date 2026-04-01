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

  result <- estimate_dist(
    linelist,
    dist = "lognormal",
    stan = stan_opts(samples = 1000, chains = 2),
    verbose = FALSE
  )

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

test_that("errors for negative delays", {
  origin <- as.Date("2023-01-01")
  linelist <- data.frame(
    pdate_lwr = origin + 5:9,
    sdate_lwr = origin + 0:4
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

test_that("errors for unsupported distribution", {
  origin <- as.Date("2023-01-01")
  linelist <- data.frame(
    pdate_lwr = origin + 0:4,
    sdate_lwr = origin + 5:9
  )

  expect_error(
    estimate_dist(linelist, dist = "normal"),
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

test_that("negative delays are filtered without error", {
  origin <- as.Date("2023-01-01")
  # sdate_lwr before pdate_lwr for first two rows => negative delays
  linelist <- data.frame(
    pdate_lwr = origin + c(5, 6, 0, 1, 2),
    sdate_lwr = origin + c(2, 3, 5, 6, 7)
  )

  # Should not error; negative delays are silently removed
  result <- EpiNow2:::.prepare_linelist_data(linelist)
  expect_true(all(result$delay_lwr >= 0))
  expect_true(nrow(result) > 0)
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
