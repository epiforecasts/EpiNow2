# Setup for testing -------------------------------------------------------
skip_on_cran()

# Unit tests (fast, no MCMC) -----------------------------------------------

test_that("prepare_truncation_obs correctly processes observation snapshots", {
  # Create simple test data: 3 snapshots with increasing completeness
  dates <- seq(as.Date("2020-01-01"), as.Date("2020-01-10"), by = "day")

  # Snapshot 1: only first 7 days
  snap1 <- data.frame(date = dates[1:7], confirm = 10:16)
  # Snapshot 2: first 8 days
  snap2 <- data.frame(date = dates[1:8], confirm = 10:17)
  # Snapshot 3: all 10 days (most complete)
  snap3 <- data.frame(date = dates, confirm = 10:19)

  data <- list(snap1, snap2, snap3)

  result <- EpiNow2:::prepare_truncation_obs(data, trunc_max = 5)

  # Check structure
  expect_type(result, "list")
  expect_named(result, c("obs", "obs_dist", "t", "obs_sets", "dirty_obs"))

  # Check that obs is a matrix
  expect_true(is.matrix(result$obs))

  # Check dimensions: should have 3 observation sets
  expect_equal(result$obs_sets, 3)

  # Check that obs_dist reflects the truncation in each dataset
  # obs_dist has one value per dataset (columns 2:ncol after merge)
  expect_type(result$obs_dist, "double")
  expect_equal(length(result$obs_dist), 3)

  # dirty_obs should be ordered by nrow (shortest first)
  expect_equal(length(result$dirty_obs), 3)
})

test_that("prepare_truncation_obs handles datasets with different start dates", {
  # Snapshot 1: days 1-5
  snap1 <- data.frame(
    date = seq(as.Date("2020-01-01"), as.Date("2020-01-05"), by = "day"),
    confirm = 1:5
  )
  # Snapshot 2: days 3-8 (starts later)
  snap2 <- data.frame(
    date = seq(as.Date("2020-01-03"), as.Date("2020-01-08"), by = "day"),
    confirm = 3:8
  )

  data <- list(snap1, snap2)

  result <- EpiNow2:::prepare_truncation_obs(data, trunc_max = 3)

  # Should only use dates from Jan 3 onwards (the latest start date)
  expect_true(result$t > 0)
  expect_equal(result$obs_sets, 2)
})

# Integration tests (MCMC-based) ------------------------------------------
# These tests run actual MCMC sampling and are slow. Tests are divided into:
# - Core tests: Essential tests that always run to catch critical failures
# - Variant tests: Configuration variations that only run weekly (gated by EPINOW2_SKIP_INTEGRATION)

futile.logger::flog.threshold("FATAL")

# set number of cores to use
old_opts <- options()
options(mc.cores = ifelse(interactive(), 4, 1))

# Run MCMC once and reuse across multiple tests to save time
default_est <- estimate_truncation(example_truncated,
  verbose = FALSE, chains = 2, iter = 1000, warmup = 250
)

# Core test: Core functionality with default settings (always runs)
test_that("estimate_truncation can return values from simulated data and plot
           them", {
  est <- default_est
  expect_equal(
    names(est),
    c("observations", "args", "fit")
  )
  expect_s3_class(get_delays(est)$truncation, "dist_spec")
  expect_s3_class(summary(est), "data.table")
  expect_type(est$observations, "list")
  expect_s3_class(get_predictions(est), "data.table")
  expect_error(plot(est), NA)
})

test_that("get_delays returns valid truncation distribution", {
  est <- default_est

  # Extract the estimated truncation distribution
  trunc_dist <- get_delays(est)$truncation

  # Check structure: should be a dist_spec with lognormal distribution
  expect_s3_class(trunc_dist, "dist_spec")
  expect_equal(trunc_dist$distribution, "lognormal")

  # Check that parameters are Normal distributions (uncertainty from posterior)
  expect_s3_class(trunc_dist$parameters$meanlog, "dist_spec")
  expect_s3_class(trunc_dist$parameters$sdlog, "dist_spec")
  expect_equal(trunc_dist$parameters$meanlog$distribution, "normal")
  expect_equal(trunc_dist$parameters$sdlog$distribution, "normal")
})

test_that("deprecated accessors return correct values with warnings", {
  est <- default_est

  # $obs returns merged predictions+observations with deprecation warning
  lifecycle::expect_deprecated(obs_result <- est$obs)
  expect_s3_class(obs_result, "data.table")
  expect_true("confirm" %in% names(obs_result))
  expect_true("last_confirm" %in% names(obs_result))
  expect_true("median" %in% names(obs_result))

  # $data returns args with deprecation warning
  lifecycle::expect_deprecated(data_result <- est$data)
  expect_equal(data_result, est$args)

  # $dist returns dist_spec with deprecation warning
  lifecycle::expect_deprecated(dist_result <- est$dist)
  expect_s3_class(dist_result, "dist_spec")

  # $last_obs returns data.table with deprecation warning
  lifecycle::expect_deprecated(last_obs_result <- est$last_obs)
  expect_s3_class(last_obs_result, "data.table")
  expect_true("date" %in% names(last_obs_result))
  expect_true("confirm" %in% names(last_obs_result))

  # $cmf returns numeric vector with deprecation warning
  lifecycle::expect_deprecated(cmf_result <- est$cmf)
  expect_type(cmf_result, "double")
  # Use tolerance for floating point comparison
  expect_true(all(cmf_result >= -1e-10 & cmf_result <= 1 + 1e-10))

  # [[ accessor works the same way
  lifecycle::expect_deprecated(obs_bracket <- est[["obs"]])
  expect_s3_class(obs_bracket, "data.table")
  expect_true("confirm" %in% names(obs_bracket))
})

test_that("get_delays returns truncation distribution from estimate_truncation", {
  est <- default_est

  # Test getting all delays as named list
  delays <- get_delays(est)
  expect_type(delays, "list")
  expect_named(delays, "truncation")
  expect_s3_class(delays$truncation, "dist_spec")
})

test_that("get_delay extracts single delay by name", {
  est <- default_est

  # get_delay should return same result as get_delays()$truncation
  trunc_dist <- get_delay(est, "truncation")
  expect_s3_class(trunc_dist, "dist_spec")
  expect_equal(trunc_dist$distribution, "lognormal")

  # Non-existent delay should return NULL
  expect_null(get_delay(est, "nonexistent"))
})

# Variant tests: Only run in full test mode (EPINOW2_SKIP_INTEGRATION=false)
test_that("estimate_truncation can return values from simulated data with the
           cmdstanr backend", {
  skip_integration()
  # fit model to example data
  skip_on_os("windows")
  output <- capture.output(suppressMessages(suppressWarnings(
    est <- estimate_truncation(example_truncated,
      verbose = FALSE, chains = 2, iter = 1000, warmup = 250,
      stan = stan_opts(backend = "cmdstanr")
    )
  )))
  expect_equal(
    names(est),
    c("observations", "args", "fit")
  )
  expect_s3_class(get_delays(est)$truncation, "dist_spec")
  expect_error(plot(est), NA)
})

test_that("estimate_truncation works with filter_leading_zeros set", {
  skip_integration()
  skip_on_os("windows")
  # Modify the first three rows of the first dataset to have zero cases
  # and fit the model with filter_leading_zeros = TRUE. This should
  # be the same as fitting the model to the original dataset because the
  # earlier dataset is corrected to be the same as the final dataset.
  modified_data <- data.table::copy(example_truncated)
  modified_data[[1]][1:3, confirm := 0]
  modified_data <- lapply(modified_data, filter_leading_zeros)
  modified_data_fit <- estimate_truncation(
    modified_data,
    verbose = FALSE, chains = 2, iter = 1000, warmup = 250
  )
  # fit model to original dataset
  original_data_fit <- estimate_truncation(
    example_truncated,
    verbose = FALSE, chains = 2, iter = 1000, warmup = 250
  )
  expect_named(
    modified_data_fit,
    c("observations", "args", "fit")
  )
  # Compare the results of the two fits
  expect_equal(
    get_delays(original_data_fit)$truncation$dist,
    get_delays(modified_data_fit)$truncation$dist
  )
  expect_equal(
    original_data_fit$args$obs_dist,
    modified_data_fit$args$obs_dist
  )
})

test_that("estimate_truncation works with zero_threshold set", {
  skip_integration()
  skip_on_os("windows")
  # fit model to a modified version of example_data with zero leading cases
  # but with filter_leading_zeros = TRUE
  modified_data <- example_truncated
  modified_data <- purrr::map(modified_data, function(x) x[sample(1:10, 6), confirm := 0])
  modified_data <- lapply(modified_data, apply_zero_threshold, threshold = 1)
  out <- estimate_truncation(modified_data,
    verbose = FALSE, chains = 2, iter = 1000, warmup = 250
  )
  expect_named(out, c("observations", "args", "fit"))
  expect_s3_class(get_delays(out)$truncation, "dist_spec")
})

options(old_opts)
