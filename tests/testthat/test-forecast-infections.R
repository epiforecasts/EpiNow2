skip_on_cran()

# Uses shared fixtures from setup.R (regional_epinow run once)
# Core tests always run; variant tests gated with skip_integration()

futile.logger::flog.threshold("FATAL")

# Helper to build R vector that fits within fixture constraints
make_adjusted_R <- function(estimate_infections, adjusted_values) {
  n_R <- nrow(summary(estimate_infections, type = "parameters", param = "R"))
  n_adjusted <- length(adjusted_values)
  c(rep(NA_real_, n_R - n_adjusted), adjusted_values)
}

# Core test: basic functionality (always runs)
test_that("forecast_infections works with default settings", {
  fixtures <- get_test_fixtures()
  sims <- forecast_infections(fixtures$estimate_infections)
  expect_equal(names(sims), c("samples", "summarised", "observations"))
  expect_true(nrow(sims$samples) > 0)
  expect_true(nrow(sims$summarised) > 0)
})

test_that("forecast_infections methods return expected output structure", {
  fixtures <- get_test_fixtures()
  sims <- forecast_infections(fixtures$estimate_infections)

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
  fixtures <- get_test_fixtures()
  sims <- forecast_infections(fixtures$estimate_infections)

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

# Integration tests: variant configurations ----------------------------------

test_that("forecast_infections works with cmdstanr backend", {
  skip_integration()
  skip_on_os("windows")
  fixtures <- get_test_fixtures()
  output <- capture.output(suppressMessages(suppressWarnings(
    sims <- forecast_infections(fixtures$estimate_infections, backend = "cmdstanr")
  )))
  expect_equal(names(sims), c("samples", "summarised", "observations"))
})

test_that("forecast_infections works with an adjusted Rt", {
  skip_integration()
  fixtures <- get_test_fixtures()
  adjusted <- rep(0.5, 20)
  R <- make_adjusted_R(fixtures$estimate_infections, adjusted)
  sims <- forecast_infections(fixtures$estimate_infections, R)
  expect_equal(names(sims), c("samples", "summarised", "observations"))
  expect_equal(tail(sims$summarised[variable == "R"]$median, 9), rep(0.5, 9))
})

test_that("forecast_infections works with a short adjusted Rt", {
  skip_integration()
  fixtures <- get_test_fixtures()
  adjusted <- rep(0.5, 7)
  R <- make_adjusted_R(fixtures$estimate_infections, adjusted)
  sims <- forecast_infections(fixtures$estimate_infections, R)
  expect_equal(names(sims), c("samples", "summarised", "observations"))
  expect_equal(tail(sims$summarised[variable == "R"]$median, 5), rep(0.5, 5))
})

test_that("forecast_infections works with a long adjusted Rt", {
  skip_integration()
  fixtures <- get_test_fixtures()
  adjusted <- c(rep(1.2, 20), rep(0.8, 20))
  R <- make_adjusted_R(fixtures$estimate_infections, adjusted)
  sims <- forecast_infections(fixtures$estimate_infections, R)
  sims10 <- forecast_infections(fixtures$estimate_infections, R, samples = 10)
  expect_equal(names(sims), c("samples", "summarised", "observations"))
  expect_equal(
    tail(sims$summarised[variable == "R"]$median, length(adjusted)), adjusted
  )
})

test_that("forecast infections can be run with a limited number of samples", {
  skip_integration()
  fixtures <- get_test_fixtures()
  adjusted <- c(rep(1.2, 20), rep(0.8, 20))
  R <- make_adjusted_R(fixtures$estimate_infections, adjusted)
  sims <- forecast_infections(fixtures$estimate_infections, R, samples = 10)
  expect_equal(names(sims), c("samples", "summarised", "observations"))
  expect_equal(
    tail(sims$summarised[variable == "R"]$median, length(adjusted)), adjusted
  )
  expect_equal(max(sims$samples$sample), 10)
})

test_that("forecast infections can be run with one sample", {
  skip_integration()
  fixtures <- get_test_fixtures()
  adjusted <- c(rep(1.2, 20), rep(0.8, 20))
  R <- make_adjusted_R(fixtures$estimate_infections, adjusted)
  sims <- forecast_infections(fixtures$estimate_infections, R, samples = 1)
  expect_equal(names(sims), c("samples", "summarised", "observations"))
  expect_equal(
    tail(sims$summarised[variable == "R"]$median, length(adjusted)), adjusted
  )
  expect_equal(max(sims$samples$sample), 1)
})

test_that("forecast infections fails as expected", {
  skip_integration()
  fixtures <- get_test_fixtures()
  expect_error(forecast_infections())
  expect_error(forecast_infections(fixtures$estimate_infections[-"fit"]))
})

test_that("forecast_infections works with an adjusted Rt in data frame", {
  skip_integration()
  fixtures <- get_test_fixtures()
  R_dates <- summary(
    fixtures$estimate_infections, type = "parameters", param = "R"
  )$date
  R <- c(rep(1.4, length(R_dates) - 10), rep(0.5, 10))
  R_dt <- data.frame(date = R_dates, value = R)
  sims_dt <- forecast_infections(fixtures$estimate_infections, R_dt)
  expect_equal(names(sims_dt), c("samples", "summarised", "observations"))
  expect_equal(tail(sims_dt$summarised[variable == "R"]$median, 10), rep(0.5, 10))
})

test_that("forecast_infections works with samples of Rt in a data frame", {
  skip_integration()
  fixtures <- get_test_fixtures()
  R_samples <- get_samples(fixtures$estimate_infections)[variable == "R"]
  R_samples <- R_samples[, .(date, sample, value)][sample <= 1000]
  R_samples <- R_samples[date >= "2020-04-01", value := 1.1]
  sims_sample <- forecast_infections(fixtures$estimate_infections, R_samples)
  expect_equal(names(sims_sample), c("samples", "summarised", "observations"))
})
