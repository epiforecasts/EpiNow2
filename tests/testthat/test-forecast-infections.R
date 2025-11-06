skip_on_cran()
# Setup for testing -------------------------------------------------------
futile.logger::flog.threshold("FATAL")
reported_cases <- EpiNow2::example_confirmed[1:50]

out <- suppressWarnings(estimate_infections(reported_cases,
  generation_time = gt_opts(example_generation_time),
  delays = delay_opts(example_reporting_delay),
  gp = NULL, rt = rt_opts(rw = 14),
  stan = stan_opts(
    chains = 2, warmup = 100, samples = 100,
    control = list(adapt_delta = 0.9)
  ),
  verbose = FALSE
))


test_that("forecast_infections works to simulate a passed in estimate_infections object", {
  sims <- forecast_infections(out)
  expect_equal(names(sims), c("samples", "summarised", "observations"))
})

test_that("forecast_infections returns an object with the correct class structure", {
  sims <- forecast_infections(out)
  expect_s3_class(sims, "forecast_infections")
  expect_false(inherits(sims, "estimate_infections"))
  expect_true(inherits(sims, "forecast_infections"))
})

test_that("forecast_infections methods work correctly", {
  sims <- forecast_infections(out)
  # Test plot method
  expect_error(plot(sims), NA)
  # Test summary method
  expect_error(summary(sims), NA)
  expect_error(summary(sims, type = "parameters"), NA)
  # Test get_samples method
  samples <- get_samples(sims)
  expect_s3_class(samples, "data.table")
  expect_true("variable" %in% names(samples))
})

test_that("forecast_infections works to simulate a passed in estimate_infections
           object when using the cmdstanr backend", {
  skip_on_os("windows")
  output <- capture.output(suppressMessages(suppressWarnings(
    sims <- forecast_infections(out, backend = "cmdstanr")
  )))
  expect_equal(names(sims), c("samples", "summarised", "observations"))
})

test_that("forecast_infections works to simulate a passed in estimate_infections object with an adjusted Rt", {
  R <- c(rep(NA_real_, 40), rep(0.5, 17))
  sims <- forecast_infections(out, R)
  expect_equal(names(sims), c("samples", "summarised", "observations"))
  expect_equal(tail(sims$summarised[variable == "R"]$median, 9), rep(0.5, 9))
})

test_that("forecast_infections works to simulate a passed in estimate_infections object with a short adjusted Rt", {
  R <- c(rep(NA_real_, 40), rep(0.5, 17))
  sims <- forecast_infections(out, R)
  expect_equal(names(sims), c("samples", "summarised", "observations"))
  expect_equal(tail(sims$summarised[variable == "R"]$median, 9), rep(0.5, 9))
})

test_that("forecast_infections works to simulate a passed in estimate_infections object with a long adjusted Rt", {
  R <- c(rep(NA_real_, 40), rep(1.2, 15), rep(0.8, 15))
  sims <- forecast_infections(out, R)
  sims10 <- forecast_infections(out, R, samples = 10)
  expect_equal(names(sims), c("samples", "summarised", "observations"))
  expect_equal(tail(sims$summarised[variable == "R"]$median, 30), R[41:70])
})

test_that("forecast infections can be run with a limited number of samples", {
  R <- c(rep(NA_real_, 40), rep(1.2, 15), rep(0.8, 15))
  sims <- forecast_infections(out, R, samples = 10)
  expect_equal(names(sims), c("samples", "summarised", "observations"))
  expect_equal(tail(sims$summarised[variable == "R"]$median, 30), R[41:70])
  expect_equal(max(sims$samples$sample), 10)
})

test_that("forecast infections can be run with one sample", {
  R <- c(rep(NA_real_, 40), rep(1.2, 15), rep(0.8, 15))
  sims <- forecast_infections(out, R, samples = 1)
  expect_equal(names(sims), c("samples", "summarised", "observations"))
  expect_equal(tail(sims$summarised[variable == "R"]$median, 30), R[41:70])
  expect_equal(max(sims$samples$sample), 1)
})

test_that("forecast infections fails as expected", {
  expect_error(forecast_infections())
  expect_error(forecast_infections(out[-"fit"]))
})

test_that("forecast_infections works to simulate a passed in estimate_infections object with an adjusted Rt in data frame", {
  R <- c(rep(1.4, 40), rep(0.5, 17))
  R_dt <- data.frame(date = summary(out, type = "parameters", param = "R")$date, value = R)
  sims_dt <- forecast_infections(out, R_dt)
  expect_equal(names(sims_dt), c("samples", "summarised", "observations"))
})

test_that("forecast_infections works to simulate a passed in estimate_infections object with samples of Rt in a data frame", {
  R_samples <- get_samples(out)[variable == "R"]
  R_samples <- R_samples[, .(date, sample, value)][sample <= 1000]
  R_samples <- R_samples[date >= "2020-04-01", value := 1.1]
  sims_sample <- forecast_infections(out, R_samples)
  expect_equal(names(sims_sample), c("samples", "summarised", "observations"))
})
