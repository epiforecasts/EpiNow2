skip_on_cran()

# Integration tests (MCMC-based) ------------------------------------------
# These tests run actual MCMC sampling and are slow. Tests are divided into:
# - Core tests: Essential tests that always run to catch critical failures
# - Variant tests: Configuration variations that only run weekly (gated by EPINOW2_SKIP_INTEGRATION)

# Helper functions for setting up test data ----------------------------------

setup_incidence_data <- function() {
  # make some example secondary incidence data
  cases <- example_confirmed
  cases <- as.data.table(cases)[, primary := confirm]

  inc_cases <- copy(cases)
  # Assume that only 40 percent of cases are reported
  inc_cases[, scaling := 0.4]
  # Parameters of the assumed log normal delay distribution
  inc_cases[, meanlog := 1.8][, sdlog := 0.5]

  # Simulate secondary cases
  inc_cases <- convolve_and_scale(inc_cases, type = "incidence")
  inc_cases[
    ,
    c("confirm", "scaling", "meanlog", "sdlog", "index", "scaled", "conv") :=
      NULL
  ]
  return(inc_cases)
}

setup_prevalence_data <- function() {
  # make some example prevalence data
  cases <- example_confirmed
  cases <- as.data.table(cases)[, primary := confirm]

  prev_cases <- copy(cases)
  # Assume that only 30 percent of cases are reported
  prev_cases[, scaling := 0.3]
  # Parameters of the assumed log normal delay distribution
  prev_cases[, meanlog := 1.6][, sdlog := 0.8]

  # Simulate secondary cases
  prev_cases <- convolve_and_scale(prev_cases, type = "prevalence")

  return(prev_cases)
}

# Core tests: Core functionality (always runs) ------------------------------

# Run MCMC once and reuse across multiple tests to save time
inc_cases <- setup_incidence_data()
default_inc <- estimate_secondary(inc_cases[1:60],
  obs = obs_opts(
    scale = Normal(mean = 0.2, sd = 0.2, max = 1), week_effect = FALSE
  ),
  verbose = FALSE
)

# Test output
test_that("estimate_secondary can return values from simulated data and plot
           them", {
  # Reuse pre-computed fit
  inc <- default_inc

  expect_equal(names(inc), c("fit", "args", "observations"))
  expect_s3_class(inc, "estimate_secondary")
  expect_s3_class(inc, "epinowfit")

  # Test accessor methods
  predictions <- get_predictions(inc)
  expect_equal(
    names(predictions),
    c(
      "date", "primary", "secondary", "accumulate", "median", "mean", "sd",
      "lower_90", "lower_50", "lower_20", "upper_20", "upper_50", "upper_90"
    )
  )

  posterior <- get_samples(inc)
  expect_true(is.data.frame(posterior))

  expect_true(is.list(inc$args))
  # validation plot of observations vs estimates
  expect_error(plot(inc, primary = TRUE), NA)
})

test_that("forecast_secondary can return values from simulated data and plot
           them", {
  # Reuse pre-computed fit
  inc <- default_inc

  # then forecast
  inc_preds <- forecast_secondary(
    inc, inc_cases[seq(61, .N)][, value := primary]
  )
  expect_equal(names(inc_preds), c("samples", "forecast", "predictions"))
  # validation plot of observations vs estimates
  expect_error(plot(inc_preds, new_obs = inc_cases, from = "2020-05-01"), NA)
})

test_that("estimate_secondary produces valid posterior samples", {
  # Check that get_samples returns valid data structure with expected parameters
  # Parameter recovery is tested separately in integration tests with longer chains
  samples <- get_samples(default_inc)

  # Check samples structure
  expect_s3_class(samples, "data.table")
  expect_true(nrow(samples) > 0)
  expect_true(all(c("variable", "parameter", "value", "sample") %in% names(samples)))

  # Check that delay parameters are present
  delay_samples <- samples[variable == "delay_params"]
  expect_true(nrow(delay_samples) > 0)
  expect_true("reporting[1]" %in% delay_samples$parameter)  # meanlog
  expect_true("reporting[2]" %in% delay_samples$parameter)  # sdlog

  # Check that scaling parameter is present
  param_samples <- samples[variable == "params"]
  expect_true(nrow(param_samples) > 0)
  expect_true("fraction_observed" %in% param_samples$parameter)

  # Check that parameter values are in valid ranges
  meanlog_samples <- delay_samples[parameter == "reporting[1]", value]
  expect_true(all(is.finite(meanlog_samples)))
  expect_true(all(meanlog_samples > 0))  # meanlog should be positive for delay

  sdlog_samples <- delay_samples[parameter == "reporting[2]", value]
  expect_true(all(is.finite(sdlog_samples)))
  expect_true(all(sdlog_samples > 0))  # sdlog must be positive

  scaling_samples <- param_samples[parameter == "fraction_observed", value]
  expect_true(all(is.finite(scaling_samples)))
  expect_true(all(scaling_samples >= 0 & scaling_samples <= 1))  # scaling in [0,1]
})

# Variant tests: Only run in full test mode (EPINOW2_SKIP_INTEGRATION=false) -

test_that("estimate_secondary successfully returns estimates when passed NA values", {
  skip_integration()
  inc_cases <- setup_incidence_data()
  prev_cases <- setup_prevalence_data()

  cases_na <- data.table::copy(inc_cases)
  cases_na[sample(1:60, 5), secondary := NA]
  inc_na <- estimate_secondary(cases_na[1:60],
    delays = delay_opts(
      LogNormal(meanlog = 1.8, sdlog = 0.5)
    ),
    obs = obs_opts(scale = Normal(mean = 0.2, sd = 0.2), week_effect = FALSE),
    verbose = FALSE
  )
  prev_cases_na <- data.table::copy(prev_cases)
  prev_cases_na[sample(1:60, 5), secondary := NA]
  prev_na <- estimate_secondary(prev_cases_na[1:60],
    secondary = secondary_opts(type = "prevalence"),
    delays = delay_opts(
      LogNormal(mean = 1.8, sd = 0.5)
    ),
    obs = obs_opts(scale = Normal(mean = 0.2, sd = 0.2), week_effect = FALSE),
    verbose = FALSE
  )
  expect_true(is.list(inc_na$args))
  expect_true(is.list(prev_na$args))
})

test_that("estimate_secondary successfully returns estimates when accumulating to weekly", {
  skip_integration()
  inc_cases <- setup_incidence_data()

  cases <- example_confirmed
  cases <- as.data.table(cases)[, primary := confirm]

  secondary_weekly <- inc_cases[, list(date, secondary)]
  secondary_weekly[, secondary := frollsum(secondary, 7)]
  secondary_weekly <- secondary_weekly[seq(7, nrow(secondary_weekly), by = 7)]
  cases_weekly <- merge(
    cases[, list(date, primary)], secondary_weekly,
    by = "date", all.x = TRUE
  )
  cases_weekly <- fill_missing(
    cases_weekly,
    missing_obs = "accumulate", obs_column = "secondary"
  )
  inc_weekly <- estimate_secondary(cases_weekly,
    delays = delay_opts(
      LogNormal(
        mean = 1.8, sd = 0.5
      )
    ),
    obs = obs_opts(
      scale = Normal(mean = 0.4, sd = 0.05), week_effect = FALSE
    ), verbose = FALSE
  )
  expect_true(is.list(inc_weekly$args))
})

test_that("estimate_secondary works when only estimating scaling", {
  skip_integration()
  inc_cases <- setup_incidence_data()

  inc <- estimate_secondary(inc_cases[1:60],
    obs = obs_opts(scale = Normal(mean = 0.2, sd = 0.2), week_effect = FALSE),
    delay = delay_opts(),
    verbose = FALSE
  )
  expect_equal(names(inc), c("fit", "args", "observations"))
})

test_that("estimate_secondary can recover simulated parameters", {
  skip_integration()
  inc_cases <- setup_incidence_data()
  prev_cases <- setup_prevalence_data()

  # fit model to example data specifying a weak prior for fraction reported
  inc <- estimate_secondary(inc_cases[1:60],
    obs = obs_opts(
      scale = Normal(mean = 0.2, sd = 0.2, max = 1), week_effect = FALSE
    ),
    verbose = FALSE
  )

  # fit model to example prevalence data
  prev <- estimate_secondary(prev_cases[1:100],
    secondary = secondary_opts(type = "prevalence"),
    obs = obs_opts(
      week_effect = FALSE,
      scale = Normal(mean = 0.4, sd = 0.1)
    ),
    verbose = FALSE
  )

  # Helper to check if true value falls within 95% credible interval
  check_ci_coverage <- function(samples, true_value) {
    ci <- quantile(samples, probs = c(0.025, 0.975))
    true_value >= ci[1] && true_value <= ci[2]
  }

  # extract posterior variables of interest
  params <- c(
    "meanlog" = "delay_params[1]", "sdlog" = "delay_params[2]",
    "scaling" = "params[1]"
  )

  inc_posterior <- get_samples(inc)[variable %in% params]
  prev_posterior <- get_samples(prev)[variable %in% params]

  # Incidence model: true values meanlog=1.8, sdlog=0.5, scaling=0.4
  inc_true <- c(1.8, 0.5, 0.4)
  for (i in seq_along(params)) {
    samples <- inc_posterior[variable == params[i], value]
    expect_true(
      check_ci_coverage(samples, inc_true[i]),
      info = sprintf(
        "inc %s: true=%.1f not in 95%% CI [%.2f, %.2f]",
        names(params)[i], inc_true[i],
        quantile(samples, 0.025), quantile(samples, 0.975)
      )
    )
  }

  # Prevalence model: true values meanlog=1.6, sdlog=0.8, scaling=0.3
  prev_true <- c(1.6, 0.8, 0.3)
  for (i in seq_along(params)) {
    samples <- prev_posterior[variable == params[i], value]
    expect_true(
      check_ci_coverage(samples, prev_true[i]),
      info = sprintf(
        "prev %s: true=%.1f not in 95%% CI [%.2f, %.2f]",
        names(params)[i], prev_true[i],
        quantile(samples, 0.025), quantile(samples, 0.975)
      )
    )
  }
})

test_that("estimate_secondary can recover simulated parameters with the
           cmdstanr backend", {
  skip_integration()
  skip_on_os("windows")
  inc_cases <- setup_incidence_data()

  # Helper to check if true value falls within 95% credible interval
  check_ci_coverage <- function(samples, true_value) {
    ci <- quantile(samples, probs = c(0.025, 0.975))
    true_value >= ci[1] && true_value <= ci[2]
  }

  # extract posterior variables of interest
  params <- c(
    "meanlog" = "delay_params[1]", "sdlog" = "delay_params[2]",
    "scaling" = "params[1]"
  )

  output <- capture.output(suppressMessages(suppressWarnings(
    inc_cmdstanr <- estimate_secondary(inc_cases[1:60],
      obs = obs_opts(scale = Normal(mean = 0.2, sd = 0.2), week_effect = FALSE),
      verbose = FALSE, stan = stan_opts(backend = "cmdstanr")
    )
  )))
  inc_posterior_cmdstanr <- get_samples(inc_cmdstanr)[variable %in% params]

  # Check parameter recovery using credible intervals
  # True values: meanlog=1.8, sdlog=0.5, scaling=0.4
  inc_true <- c(1.8, 0.5, 0.4)
  for (i in seq_along(params)) {
    samples <- inc_posterior_cmdstanr[variable == params[i], value]
    expect_true(
      check_ci_coverage(samples, inc_true[i]),
      info = sprintf(
        "cmdstanr %s: true=%.1f not in 95%% CI [%.2f, %.2f]",
        names(params)[i], inc_true[i],
        quantile(samples, 0.025), quantile(samples, 0.975)
      )
    )
  }
})

test_that("forecast_secondary works with fixed delays", {
  skip_integration()
  inc_cases <- setup_incidence_data()

  # fit model to example data with a fixed delay
  inc_fixed <- estimate_secondary(inc_cases[1:60],
    delays = delay_opts(Gamma(mean = 15, sd = 5, max = 30)),
    verbose = FALSE
  )

  inc_preds <- forecast_secondary(
    inc_fixed, inc_cases[seq(61, .N)][, value := primary]
  )
  expect_equal(names(inc_preds), c("samples", "forecast", "predictions"))
  # validation plot of observations vs estimates
  expect_error(plot(inc_preds, new_obs = inc_cases, from = "2020-05-01"), NA)
})

test_that("forecast_secondary can return values from simulated data when using
           the cmdstanr backend", {
  skip_integration()
  skip_on_os("windows")
  inc_cases <- setup_incidence_data()

  # fit model first
  inc <- estimate_secondary(inc_cases[1:60],
    obs = obs_opts(
      scale = Normal(mean = 0.2, sd = 0.2, max = 1), week_effect = FALSE
    ),
    verbose = FALSE
  )

  capture.output(suppressMessages(suppressWarnings(
    inc_preds <- forecast_secondary(
      inc, inc_cases[seq(61, .N)][, value := primary],
      backend = "cmdstanr"
    )
  )))
  expect_equal(names(inc_preds), c("samples", "forecast", "predictions"))
})

test_that("estimate_secondary works with weigh_delay_priors = TRUE", {
  skip_integration()
  inc_cases <- setup_incidence_data()

  delays <- LogNormal(
    meanlog = Normal(2.5, 0.5),
    sdlog = Normal(0.47, 0.25),
    max = 30
  )
  inc_weigh <- estimate_secondary(
    inc_cases[1:60],
    delays = delay_opts(delays),
    obs = obs_opts(scale = Normal(mean = 0.2, sd = 0.2), week_effect = FALSE),
    weigh_delay_priors = TRUE, verbose = FALSE
  )
  expect_s3_class(inc_weigh, "estimate_secondary")
})

test_that("estimate_secondary works with filter_leading_zeros set", {
  skip_integration()
  inc_cases <- setup_incidence_data()

  ## testing deprecated functionality
  withr::local_options(lifecycle_verbosity = "quiet")
  modified_data <- inc_cases[1:10, secondary := 0]
  modified_data <- filter_leading_zeros(modified_data, obs_column = "secondary")
  out <- suppressWarnings(estimate_secondary(
    modified_data,
    obs = obs_opts(
      scale = Normal(mean = 0.2, sd = 0.2),
      week_effect = FALSE
    ),
    verbose = FALSE
  ))
  expect_s3_class(out, "estimate_secondary")
  expect_named(out, c("fit", "args", "observations"))
  expect_equal(get_predictions(out)$primary, modified_data$primary)
})

test_that("estimate_secondary works with zero_threshold set", {
  skip_integration()
  inc_cases <- setup_incidence_data()

  ## testing deprecated functionality
  withr::local_options(lifecycle_verbosity = "quiet")
  modified_data <- inc_cases[sample(1:30, 10), primary := 0]
  modified_data <- apply_zero_threshold(
    modified_data,
    threshold = 10, obs_column = "secondary"
  )
  out <- estimate_secondary(
    modified_data,
    obs = obs_opts(
      scale = Normal(mean = 0.2, sd = 0.2),
      week_effect = FALSE
    ),
    verbose = FALSE
  )
  expect_s3_class(out, "estimate_secondary")
  expect_named(out, c("fit", "args", "observations"))
})

test_that("get_delays returns correct delays from estimate_secondary", {
  # Reuse pre-computed fit
  out <- default_inc

  # Test getting all delays as named list
  delays <- get_delays(out)
  expect_type(delays, "list")

  # All elements should be dist_spec (if any exist)
  for (nm in names(delays)) {
    expect_s3_class(delays[[nm]], "dist_spec")
  }
})
