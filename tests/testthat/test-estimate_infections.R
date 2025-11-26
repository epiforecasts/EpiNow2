skip_on_cran()
# Setup for testing -------------------------------------------------------

futile.logger::flog.threshold("FATAL")

reported_cases <- EpiNow2::example_confirmed[1:30]

default_estimate_infections <- function(..., add_stan = list(), gt = TRUE,
                                        delay = TRUE) {
  futile.logger::flog.threshold("FATAL")

  def_stan <- list(
    chains = 2, warmup = 25, samples = 25,
    control = list(adapt_delta = 0.8)
  )
  def_stan <- modifyList(def_stan, add_stan)
  stan_args <- do.call(stan_opts, def_stan)

  suppressWarnings(estimate_infections(...,
    generation_time = ifelse(gt, list(gt_opts(example_generation_time)), list(gt_opts()))[[1]],
    delays = ifelse(delay, list(delay_opts(example_incubation_period + example_reporting_delay)), list(delay_opts()))[[1]],
    stan = stan_args, verbose = FALSE
  ))
}

test_estimate_infections <- function(...) {
  out <- default_estimate_infections(...)
  expect_equal(names(out), c("fit", "args", "observations"))
  expect_true(nrow(get_samples(out)) > 0)
  expect_true(nrow(summary(out, type = "parameters")) > 0)
  expect_true(nrow(out$observations) > 0)
  invisible(out)
}

# Integration tests (MCMC-based) ------------------------------------------
# These tests run actual MCMC sampling and are slow. Tests are divided into:
# - Core tests: Essential tests that always run to catch critical failures
# - Variant tests: Configuration variations that only run weekly (gated by EPINOW2_SKIP_INTEGRATION)

# Run MCMC once and reuse across multiple tests to save time
default_fit <- default_estimate_infections(reported_cases)

# Core test: Core functionality with default settings (always runs)
test_that("estimate_infections successfully returns estimates using default settings", {
  # Reuse pre-computed fit
  expect_equal(names(default_fit), c("fit", "args", "observations"))
  expect_true(nrow(get_samples(default_fit)) > 0)
  expect_true(nrow(summary(default_fit, type = "parameters")) > 0)
  expect_true(nrow(default_fit$observations) > 0)
})

# Variant tests: Only run in full test mode (EPINOW2_SKIP_INTEGRATION=false)
test_that("estimate_infections successfully returns estimates using a Matern 5/2 kernel", {
  skip_integration()
  test_estimate_infections(
    reported_cases,
    gp = gp_opts(kernel = "matern", matern_order = 5 / 2)
  )
})

test_that("estimate_infections successfully returns estimates using backcalculation", {
  skip_integration()
  test_estimate_infections(reported_cases, rt = NULL)
})

test_that("estimate_infections successfully returns estimates using no delays", {
  skip_integration()
  test_estimate_infections(reported_cases, delay = FALSE)
})

# Non-integration tests (fast - use one MCMC fit for multiple checks) ----

test_that("summary with type='parameters' returns all dates by default", {
  # Reuse pre-computed fit
  out <- default_fit

  # Get summary without specifying target_date
  summ <- summary(out, type = "parameters")

  # Should have multiple dates (not filtered to one)
  summ_dates <- unique(summ$date)
  expect_gt(length(summ_dates), 1)

  # Should have data for multiple variables
  expect_true("infections" %in% summ$variable)
  expect_true("R" %in% summ$variable)

  # When target_date is explicitly provided, should filter to that date
  # Use a date that exists in the summarised estimates
  target <- summ_dates[length(summ_dates) %/% 2]  # Pick middle date
  summ_filtered <- summary(out, type = "parameters", target_date = target)
  expect_equal(unique(summ_filtered$date), target)
})

# Deprecation tests -------------------------------------------------------

test_that("summary.estimate_infections with type = 'samples' is deprecated", {
  # Reuse pre-computed fit
  out <- default_fit

  expect_deprecated(summary(out, type = "samples"))

  # Verify it returns the same as get_samples()
  withr::local_options(lifecycle_verbosity = "quiet")
  samples_quiet <- summary(out, type = "samples")
  samples_new <- get_samples(out)

  expect_equal(samples_quiet, samples_new)
})

test_that("extract_parameter_samples is deprecated", {
  # Reuse pre-computed fit
  out <- default_fit

  dates <- out$observations$date
  reported_dates <- dates[-(1:out$args$seeding_time)]

  # Lifecycle warnings need special handling
  expect_deprecated(extract_parameter_samples(
    out$fit,
    out$args,
    reported_dates = reported_dates,
    imputed_dates = reported_dates[out$args$imputed_times],
    reported_inf_dates = dates,
    drop_length_1 = FALSE,
    merge = FALSE
  ))

  # Verify it returns the same as format_simulation_output()
  withr::local_options(lifecycle_verbosity = "quiet")
  old_quiet <- extract_parameter_samples(
    out$fit,
    out$args,
    reported_dates = reported_dates,
    imputed_dates = reported_dates[out$args$imputed_times],
    reported_inf_dates = dates,
    drop_length_1 = FALSE,
    merge = FALSE
  )

  new_output <- format_simulation_output(
    out$fit,
    out$args,
    reported_dates = reported_dates,
    imputed_dates = reported_dates[out$args$imputed_times],
    reported_inf_dates = dates,
    drop_length_1 = FALSE,
    merge = FALSE
  )

  expect_equal(old_quiet, new_output)
})
