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

  # Test get_predictions accessor
  predictions <- get_predictions(out)
  expect_true(nrow(predictions) > 0)
  expect_true("date" %in% names(predictions))
  expect_true("confirm" %in% names(predictions))
  expect_true("mean" %in% names(predictions))

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

test_that("estimate_infections successfully returns estimates using the infectiousness growth rate estimator", {
  skip_integration()
  test_estimate_infections(
    reported_cases,
    rt = rt_opts(growth_method = "infectiousness")
  )
})

test_that("estimate_infections successfully returns estimates using a periodic kernel", {
  skip_integration()
  test_estimate_infections(
    reported_cases,
    gp = gp_opts(kernel = "periodic")
  )
})

test_that("estimate_infections successfully returns estimates when passed NA values", {
  skip_integration()
  reported_cases_na <- data.table::copy(reported_cases)
  reported_cases_na[sample(1:30, 5), confirm := NA]
  test_estimate_infections(reported_cases_na)
})

test_that("estimate_infections successfully returns estimates when accumulating to weekly", {
  skip_integration()
  reported_cases_weekly <- data.table::copy(reported_cases)
  reported_cases_weekly[, confirm := frollsum(confirm, 7)]
  reported_cases_weekly <-
    reported_cases_weekly[seq(7, nrow(reported_cases_weekly), 7)]
  reported_cases_weekly <- fill_missing(
    reported_cases_weekly,
    missing_dates = "accumulate", initial_accumulate = 7
  )
  test_estimate_infections(reported_cases_weekly)
})

test_that("estimate_infections successfully returns estimates using the poisson observation model", {
  skip_integration()
  test_estimate_infections(reported_cases, obs = obs_opts(family = "poisson"))
})

test_that("estimate_infections successfully returns estimates using a fixed Rt", {
  skip_integration()
  test_estimate_infections(reported_cases, gp = NULL)
})

test_that("estimate_infections successfully returns estimates using only mean shifted reported cases", {
  skip_integration()
  test_estimate_infections(reported_cases, gp = NULL, rt = NULL)
})

test_that("estimate_infections successfully returns estimates using a single breakpoint", {
  skip_integration()
  test_estimate_infections(
    data.table::copy(reported_cases)[, breakpoint := ifelse(date == "2020-03-10", 1, 0)],
    gp = NULL
  )
})

test_that("estimate_infections successfully returns estimates using a random walk", {
  skip_integration()
  test_estimate_infections(reported_cases, gp = NULL, rt = rt_opts(rw = 7))
})

test_that("estimate_infections works without setting a generation time", {
  skip_integration()
  df <- test_estimate_infections(reported_cases, gt = FALSE, delay = FALSE)
  ## check exp(r) == R
  samples <- get_samples(df)
  growth_rate <- samples[variable == "growth_rate"][
    ,
    list(date, sample, growth_rate = value)
  ]
  R <- samples[variable == "R"][
    ,
    list(date, sample, R = value)
  ]
  combined <- merge(growth_rate, R, by = c("date", "sample"), all = FALSE)
  expect_equal(exp(combined$growth_rate), combined$R)
})

test_that("estimate_infections works with different kernels", {
  skip_integration()
  test_estimate_infections(reported_cases, gp = gp_opts(kernel = "se"))
  test_estimate_infections(reported_cases, gp = gp_opts(kernel = "ou"))
  test_estimate_infections(reported_cases, gp = gp_opts(matern_order = 5 / 2))
})

test_that("estimate_infections fails as expected when given a very short timeout", {
  skip_integration()
  expect_error(output <- capture.output(suppressMessages(
    out <- default_estimate_infections(
      reported_cases,
      add_stan = list(future = TRUE, max_execution_time = 1, samples = 2000)
    )
  )), "all chains failed")
  expect_error(output <- capture.output(suppressMessages(
    out <- default_estimate_infections(
      reported_cases,
      add_stan = list(future = FALSE, max_execution_time = 1, samples = 2000)
    )
  )), "timed out")
})

test_that("estimate_infections works as expected with failing chains", {
  skip_integration()
  test_estimate_infections(reported_cases,
    add_stan = list(
      chains = 4,
      stuck_chains = 2, future = TRUE,
      control = list(adapt_delta = 0.8)
    )
  )

  expect_error(default_estimate_infections(reported_cases,
    add_stan = list(chains = 4, stuck_chains = 1)
  ))
  expect_error(default_estimate_infections(reported_cases,
    add_stan = list(
      chains = 4,
      stuck_chains = 3,
      future = TRUE
    )
  ))
})

test_that("estimate_infections produces no forecasts when forecast = NULL", {
  skip_integration()
  out <- test_estimate_infections(data = reported_cases, forecast = NULL)
  expect_true(!"forecast" %in% unique(out$summarised$type))
  expect_true(out$args$horizon == 0)
})

test_that("estimate_infections produces no forecasts when forecast_opts horizon is 0", {
  skip_integration()
  out <- test_estimate_infections(
    data = reported_cases, forecast = forecast_opts(horizon = 0)
  )
  expect_true(!"forecast" %in% unique(out$summarised$type))
  expect_true(out$args$horizon == 0)
})

test_that("estimate_infections can sample from the prior", {
  skip_integration()
  reported_cases_prior <- data.table::copy(reported_cases)
  reported_cases_prior[, confirm := NA]
  test_estimate_infections(reported_cases_prior)
})

test_that("estimate_infections output contains breakpoints effect when breakpoints are present", {
  skip_integration()
  data <- data.table::copy(reported_cases)
  bp_dates <- as.Date(c("2020-02-25", "2020-03-05", "2020-03-15"))
  data[, breakpoint := ifelse(date %in% bp_dates, 1, 0)]
  out <- default_estimate_infections(data, gp = NULL)
  samples <- get_samples(out)
  expect_true("breakpoints" %in% unique(samples$variable))
  expect_true(length(unique(samples[variable == "breakpoints"]$strat)) == length(bp_dates))
})

test_that("estimate_infections output does not contain breakpoints effect when breakpoints are not present", {
  skip_integration()
  data <- data.table::copy(reported_cases)
  data[, breakpoint := 0]
  out <- default_estimate_infections(data, gp = NULL)
  samples <- get_samples(out)
  expect_false("breakpoints" %in% unique(samples$variable))
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

test_that("get_delays returns correct delays from estimate_infections", {
  # Reuse pre-computed fit
  out <- default_fit

  # Test getting all delays as named list
  delays <- get_delays(out)
  expect_type(delays, "list")
  expect_true(length(delays) >= 1)
  expect_true("generation_time" %in% names(delays))

  # All elements should be dist_spec
  for (nm in names(delays)) {
    expect_s3_class(delays[[nm]], "dist_spec")
  }
})
