skip_on_cran()

# Integration tests (MCMC-based) ------------------------------------------
# These tests run actual MCMC sampling and are slow. Tests are divided into:
# - Core tests: Essential tests that always run to catch critical failures
# - Variant tests: Configuration variations that only run weekly (gated by EPINOW2_SKIP_INTEGRATION)

# Setup for testing -------------------------------------------------------
futile.logger::flog.threshold("FATAL")

# Load pre-computed estimate_infections output for testing forecast_infections
# This avoids running MCMC just to create test input
example_estimate_infections_output <- readRDS(system.file(
  package = "EpiNow2", "extdata", "example_estimate_infections.rds"
))

# Core test: Core functionality with default settings (always runs)
test_that("forecast_infections works to simulate a passed in estimate_infections object", {
  sims <- forecast_infections(example_estimate_infections_output)
  expect_equal(names(sims), c("samples", "summarised", "observations"))
})

# Variant tests: Only run in full test mode (EPINOW2_SKIP_INTEGRATION=false)
test_that("forecast_infections methods return expected output structure", {
  skip_if_not(integration_test(), "Skipping slow integration test")
  sims <- forecast_infections(example_estimate_infections_output)

  # Test plot method returns expected object types
  p <- plot(sims)
  expect_s3_class(p, "patchwork")

  # Test summary method returns data.table with expected structure
  sum_snapshot <- summary(sims)
  expect_s3_class(sum_snapshot, "data.frame")
  expect_true(all(c("measure", "estimate") %in% names(sum_snapshot)))

  sum_params <- summary(sims, type = "parameters")
  expect_s3_class(sum_params, "data.table")
  expect_true(all(c("date", "variable", "median", "mean") %in% names(sum_params)))

  # Test get_samples method
  samples <- get_samples(sims)
  expect_s3_class(samples, "data.table")
  expect_true(all(c("variable", "date", "sample", "value") %in% names(samples)))
})

test_that("forecast_infections methods respect CrIs argument", {
  skip_if_not(integration_test(), "Skipping slow integration test")
  sims <- forecast_infections(example_estimate_infections_output)

  # Test summary with custom CrIs
  sum_default <- summary(sims, type = "parameters")
  sum_custom <- summary(sims, type = "parameters", CrIs = c(0.5, 0.95))

  # Should have different credible interval columns
  default_cols <- grep("^lower_|^upper_", names(sum_default), value = TRUE)
  custom_cols <- grep("^lower_|^upper_", names(sum_custom), value = TRUE)
  expect_false(identical(default_cols, custom_cols))

  # Custom should have columns for 50% and 95% CrIs
  expect_true("lower_50" %in% names(sum_custom))
  expect_true("upper_50" %in% names(sum_custom))
  expect_true("lower_95" %in% names(sum_custom))
  expect_true("upper_95" %in% names(sum_custom))

  # Test plot with custom CrIs (should not error)
  expect_error(plot(sims, CrIs = c(0.5, 0.95)), NA)
})

test_that("forecast_infections works to simulate a passed in estimate_infections
           object when using the cmdstanr backend", {
  skip_if_not(integration_test(), "Skipping slow integration test")
  skip_on_os("windows")
  output <- capture.output(suppressMessages(suppressWarnings(
    sims <- forecast_infections(example_estimate_infections_output, backend = "cmdstanr")
  )))
  expect_equal(names(sims), c("samples", "summarised", "observations"))
})

test_that("forecast_infections works to simulate a passed in estimate_infections object with an adjusted Rt", {
  skip_if_not(integration_test(), "Skipping slow integration test")
  R <- c(rep(NA_real_, 40), rep(0.5, 17))
  sims <- forecast_infections(example_estimate_infections_output, R)
  expect_equal(names(sims), c("samples", "summarised", "observations"))
  expect_equal(tail(sims$summarised[variable == "R"]$median, 9), rep(0.5, 9))
})

test_that("forecast_infections works to simulate a passed in estimate_infections object with a short adjusted Rt", {
  skip_if_not(integration_test(), "Skipping slow integration test")
  R <- c(rep(NA_real_, 40), rep(0.5, 17))
  sims <- forecast_infections(example_estimate_infections_output, R)
  expect_equal(names(sims), c("samples", "summarised", "observations"))
  expect_equal(tail(sims$summarised[variable == "R"]$median, 9), rep(0.5, 9))
})

test_that("forecast_infections works to simulate a passed in estimate_infections object with a long adjusted Rt", {
  skip_if_not(integration_test(), "Skipping slow integration test")
  R <- c(rep(NA_real_, 40), rep(1.2, 15), rep(0.8, 15))
  sims <- forecast_infections(example_estimate_infections_output, R)
  sims10 <- forecast_infections(example_estimate_infections_output, R, samples = 10)
  expect_equal(names(sims), c("samples", "summarised", "observations"))
  expect_equal(tail(sims$summarised[variable == "R"]$median, 30), R[41:70])
})

test_that("forecast infections can be run with a limited number of samples", {
  skip_if_not(integration_test(), "Skipping slow integration test")
  R <- c(rep(NA_real_, 40), rep(1.2, 15), rep(0.8, 15))
  sims <- forecast_infections(example_estimate_infections_output, R, samples = 10)
  expect_equal(names(sims), c("samples", "summarised", "observations"))
  expect_equal(tail(sims$summarised[variable == "R"]$median, 30), R[41:70])
  expect_equal(max(sims$samples$sample), 10)
})

test_that("forecast infections can be run with one sample", {
  skip_if_not(integration_test(), "Skipping slow integration test")
  R <- c(rep(NA_real_, 40), rep(1.2, 15), rep(0.8, 15))
  sims <- forecast_infections(example_estimate_infections_output, R, samples = 1)
  expect_equal(names(sims), c("samples", "summarised", "observations"))
  expect_equal(tail(sims$summarised[variable == "R"]$median, 30), R[41:70])
  expect_equal(max(sims$samples$sample), 1)
})

test_that("forecast infections fails as expected", {
  skip_if_not(integration_test(), "Skipping slow integration test")
  expect_error(forecast_infections())
  expect_error(forecast_infections(example_estimate_infections_output[-"fit"]))
})

test_that("forecast_infections works to simulate a passed in estimate_infections object with an adjusted Rt in data frame", {
  skip_if_not(integration_test(), "Skipping slow integration test")
  R <- c(rep(1.4, 40), rep(0.5, 17))
  R_dt <- data.frame(date = summary(example_estimate_infections_output, type = "parameters", param = "R")$date, value = R)
  sims_dt <- forecast_infections(example_estimate_infections_output, R_dt)
  expect_equal(names(sims_dt), c("samples", "summarised", "observations"))
})

test_that("forecast_infections works to simulate a passed in estimate_infections object with samples of Rt in a data frame", {
  skip_if_not(integration_test(), "Skipping slow integration test")
  R_samples <- get_samples(example_estimate_infections_output)[variable == "R"]
  R_samples <- R_samples[, .(date, sample, value)][sample <= 1000]
  R_samples <- R_samples[date >= "2020-04-01", value := 1.1]
  sims_sample <- forecast_infections(example_estimate_infections_output, R_samples)
  expect_equal(names(sims_sample), c("samples", "summarised", "observations"))
})
