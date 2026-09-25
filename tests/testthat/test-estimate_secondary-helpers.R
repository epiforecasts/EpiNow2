# Tests for estimate_secondary() input handling and helper functions that
# do not need a model fit.

test_that("estimate_secondary errors when burn_in covers all observations", {
  data <- data.table::data.table(
    date = as.Date("2020-01-01") + 0:9,
    primary = 1:10,
    secondary = 1:10
  )
  expect_error(
    estimate_secondary(data, burn_in = 10, verbose = FALSE),
    "burn_in.*greater or equal to the number of"
  )
})

test_that("estimate_secondary errors for bad argument specifications", {
  data <- data.table::data.table(
    date = as.Date("2020-01-01") + 0:29,
    primary = 1:30,
    secondary = 1:30
  )
  expect_error(estimate_secondary(data, secondary = list()), "secondary_opts")
  expect_error(estimate_secondary(data, obs = list()), "obs_opts")
  expect_error(estimate_secondary(data, burn_in = -1), "burn_in")
  expect_error(estimate_secondary(data, CrIs = 1.5), "CrIs")
  expect_error(estimate_secondary(data, priors = "a"), "priors")
})

test_that("update_secondary_args replaces priors given in a data.frame", {
  data <- list(
    obs_scale_mean = 4, obs_scale_sd = 3,
    delay_params_mean = c(1, 1), delay_params_sd = c(1, 1),
    dispersion_mean = 1, dispersion_sd = 1
  )
  priors <- data.frame(
    variable = c(
      "fraction_observed", "delay_params[1]", "delay_params[2]",
      "reporting_overdispersion"
    ),
    mean = c(0.123456, 1.23456, 0.4, 0.5),
    sd = c(0.01, 0.2, 0.05, 0.1)
  )
  expect_message(
    updated <- update_secondary_args(data, priors),
    "Replacing specified priors"
  )
  # values are rounded to 3 significant figures
  expect_equal(updated$obs_scale_mean, array(0.123))
  expect_equal(updated$obs_scale_sd, array(0.01))
  expect_equal(updated$delay_params_mean, array(c(1.23, 0.4)))
  expect_equal(updated$delay_params_sd, array(c(0.2, 0.05)))
  expect_equal(updated$dispersion_mean, 0.5)
  expect_equal(updated$dispersion_sd, 0.1)
})

test_that("update_secondary_args only replaces priors that are given", {
  data <- list(obs_scale_mean = 4, obs_scale_sd = 3, dispersion_mean = 1)
  priors <- data.frame(variable = "fraction_observed", mean = 0.5, sd = 0.1)
  updated <- update_secondary_args(data, priors, verbose = FALSE)
  expect_equal(updated$obs_scale_mean, array(0.5))
  expect_equal(updated$dispersion_mean, 1)
  expect_null(updated$delay_params_mean)
  # no priors leaves the data unchanged
  expect_identical(update_secondary_args(data, NULL), data)
  expect_identical(update_secondary_args(data, priors[0, ]), data)
})

test_that("update_secondary_args warns when there are no delay defaults", {
  priors <- data.frame(variable = "delay_params[1]", mean = 1, sd = 0.5)
  expect_warning(
    updated <- update_secondary_args(list(), priors, verbose = FALSE),
    "Cannot replace delay distribution parameters"
  )
  expect_equal(updated$delay_params_mean, array(1))
})

constant_primary <- function(type) {
  data <- data.table::data.table(
    date = as.Date("2020-01-01") + 0:39,
    primary = 100, scaling = 0.4, meanlog = 1, sdlog = 0.5
  )
  convolve_and_scale(data, type = type)
}

test_that("convolve_and_scale scales constant incidence", {
  out <- constant_primary("incidence")
  expect_equal(nrow(out), 40)
  expect_equal(out$scaled, rep(40, 40))
  # with constant primary the delayed secondary settles at scaling * primary;
  # values are truncated to integers
  expect_type(out$secondary, "integer")
  expect_true(all(out$secondary %in% 39:40))
})

test_that("convolve_and_scale builds prevalence from inflows and outflows", {
  data <- data.table::data.table(
    date = as.Date("2020-01-01") + 0:19,
    primary = c(rep(100, 10), rep(0, 10)),
    scaling = 0.5, meanlog = 1.5, sdlog = 0.5
  )
  out <- convolve_and_scale(data, type = "prevalence")
  # prevalence is the previous level less outflows (floored at zero) plus
  # new scaled inflows
  expected <- numeric(nrow(out))
  expected[1] <- out$scaled[1]
  for (i in 2:nrow(out)) {
    expected[i] <- max(expected[i - 1] - out$conv[i], 0) + out$scaled[i]
  }
  expect_equal(out$secondary, as.integer(expected))
  expect_true(all(out$secondary >= 0))
  expect_equal(out$secondary[1], 50L)
})

test_that("convolve_and_scale adds Poisson observation noise", {
  data <- data.table::data.table(
    date = as.Date("2020-01-01") + 0:99,
    primary = 100, scaling = 0.4, meanlog = 1, sdlog = 0.5
  )
  set.seed(101)
  out <- convolve_and_scale(data, family = "poisson")
  expect_type(out$secondary, "integer")
  expect_gt(length(unique(out$secondary)), 1)
  # expected value is 40; standard error of the mean of 100 draws is ~0.6
  expect_equal(mean(out$secondary), 40, tolerance = 0.1)
})

test_that("convolve_and_scale passes size to the negative binomial", {
  skip("Known bug, see #1586")
  data <- data.table::data.table(
    date = as.Date("2020-01-01") + 0:9,
    primary = 100, scaling = 0.4, meanlog = 1, sdlog = 0.5
  )
  out <- convolve_and_scale(data, family = "negbin", size = 10)
  expect_type(out$secondary, "integer")
})

test_that("convolve_and_scale errors for bad 'type' and 'family'", {
  data <- data.table::data.table(
    date = as.Date("2020-01-01") + 0:9,
    primary = 100, scaling = 0.4, meanlog = 1, sdlog = 0.5
  )
  expect_error(convolve_and_scale(data, type = "cumulative"), "type")
  expect_error(convolve_and_scale(data, family = "normal"), "family")
})

# A hand-built forecast_secondary object so the plot methods can be tested
# without fitting a model
make_forecast_secondary <- function() {
  dates <- as.Date("2020-01-01") + 0:13
  n <- length(dates)
  predictions <- data.table::data.table(
    date = dates,
    primary = 10 * seq_len(n),
    secondary = c(seq_len(7), rep(NA, 7)),
    mean = seq_len(n), sd = 1, median = seq_len(n),
    lower_90 = seq_len(n) - 2, lower_50 = seq_len(n) - 1,
    lower_20 = seq_len(n) - 0.5, upper_20 = seq_len(n) + 0.5,
    upper_50 = seq_len(n) + 1, upper_90 = seq_len(n) + 2
  )
  structure(
    list(
      samples = data.table::data.table(),
      forecast = data.table::data.table(),
      predictions = predictions,
      observations = predictions[, list(date, primary, secondary)]
    ),
    class = c("forecast_secondary", "list")
  )
}

layer_geoms <- function(p) {
  vapply(p$layers, function(layer) class(layer$geom)[1], character(1))
}

test_that("plot.forecast_secondary returns a ggplot of the predictions", {
  fc <- make_forecast_secondary()
  p <- plot(fc)
  expect_s3_class(p, "ggplot")
  expect_equal(p$data$date, fc$predictions$date)
  expect_equal(p$data$secondary, fc$predictions$secondary)
  expect_equal(unname(layer_geoms(p)), c("GeomCol", rep("GeomRibbon", 3)))
})

test_that("plot.forecast_secondary adds primary observations", {
  p <- plot(make_forecast_secondary(), primary = TRUE)
  expect_true(all(c("GeomPoint", "GeomLine") %in% layer_geoms(p)))
})

test_that("plot.forecast_secondary filters with 'from' and 'to'", {
  fc <- make_forecast_secondary()
  p <- plot(fc, from = as.Date("2020-01-03"), to = as.Date("2020-01-10"))
  expect_equal(p$data$date, as.Date("2020-01-01") + 2:9)
})

test_that("plot.forecast_secondary replaces observations with 'new_obs'", {
  fc <- make_forecast_secondary()
  new_obs <- data.frame(
    date = fc$predictions$date,
    secondary = 100 + seq_along(fc$predictions$date),
    other = "ignored"
  )
  p <- plot(fc, new_obs = new_obs)
  expect_equal(p$data$secondary, new_obs$secondary)
  expect_false("other" %in% names(p$data))
})
