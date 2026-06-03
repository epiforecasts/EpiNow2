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
  expect_false("confirm" %in% names(predictions))
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
  samples <- get_samples(out)
  expect_true(!"forecast" %in% unique(samples$type))
  expect_true(out$args$horizon == 0)
})

test_that("estimate_infections produces no forecasts when forecast_opts horizon is 0", {
  skip_integration()
  out <- test_estimate_infections(
    data = reported_cases, forecast = forecast_opts(horizon = 0)
  )
  samples <- get_samples(out)
  expect_true(!"forecast" %in% unique(samples$type))
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

test_that("Dirichlet concentration anchors the GT posterior to its prior", { # nolint
  skip_integration()
  # Simulate cases from a known true GT, then fit with a prior
  # shifted away from the truth at a low and a high concentration.
  # The generation time is only weakly identified from case data,
  # so the concentration controls how far the posterior can move
  # from the prior: a high concentration keeps it close to the
  # (shifted) prior while a low one lets it drift further.
  true_gt <- c(0, 0.1, 0.3, 0.35, 0.15, 0.07, 0.03)
  shifted_prior <- c(0, 0.05, 0.15, 0.3, 0.25, 0.15, 0.1)

  set.seed(123)
  R_traj <- data.frame(
    date = seq.Date(as.Date("2023-01-01"), length.out = 60, by = "day"),
    R = c(rep(1.4, 30), rep(0.8, 30))
  )
  sim <- simulate_infections(
    R = R_traj,
    initial_infections = 100,
    generation_time = gt_opts(NonParametric(true_gt)),
    delays = delay_opts(Fixed(0)),
    obs = obs_opts(family = "poisson")
  )
  sim_cases <- sim[variable == "reported_cases", c("date", "value")]
  data.table::setnames(sim_cases, "value", "confirm")

  fit_at <- function(conc) {
    suppressWarnings(estimate_infections(
      data = sim_cases,
      generation_time = gt_opts(NonParametric(
        pmf = Dirichlet(prior = shifted_prior, concentration = conc)
      )),
      delays = delay_opts(Fixed(0)),
      rt = rt_opts(prior = LogNormal(mean = 1, sd = 0.2), rw = 7),
      gp = NULL,
      stan = stan_opts(
        samples = 500, warmup = 500,
        chains = 2, cores = 1
      ),
      verbose = FALSE
    ))
  }
  out_loose <- fit_at(1)
  out_tight <- fit_at(50)
  expect_null(out_loose$error)
  expect_null(out_tight$error)

  pmf_of <- function(out) {
    as.numeric(get_pmf(get_parameters(out)$generation_time))
  }
  loose_pmf <- pmf_of(out_loose)
  tight_pmf <- pmf_of(out_tight)

  # Both posteriors are valid PMFs over the GT support.
  expect_equal(sum(loose_pmf), 1, tolerance = 1e-6)
  expect_equal(sum(tight_pmf), 1, tolerance = 1e-6)
  expect_true(all(loose_pmf >= 0))
  expect_true(all(tight_pmf >= 0))

  # The concentration anchors the posterior: a high concentration
  # keeps it closer to the (shifted) prior than a low one.
  drift_to <- function(p) sum((p - shifted_prior)^2)
  expect_lt(drift_to(tight_pmf), drift_to(loose_pmf))

  # The anchored (tight) fit is still pulled towards the truth by
  # the data, ending up closer to it than the shifted prior.
  err_to <- function(p) sum((p - true_gt)^2)
  expect_lt(err_to(tight_pmf), err_to(shifted_prior))
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

test_that("summary.estimate_infections with type = 'samples' errors", {
  out <- default_fit
  expect_error(summary(out, type = "samples"), "get_samples")
})

test_that("get_parameters works as expected for estimate_infections", {
  # Reuse pre-computed fit
  out <- default_fit

  # Test getting all parameters as named list
  params <- get_parameters(out)
  expect_type(params, "list")
  expect_true(length(params) >= 1)
  expect_true("generation_time" %in% names(params))

  # All elements should be dist_spec
  for (nm in names(params)) {
    expect_s3_class(params[[nm]], "dist_spec")
  }
})

test_that("get_parameters works as expected with fixed parameters", {
  # Use fixed delays (no uncertainty) so we can verify exact values
  # PMF must start with 0 (generation time at day 0 is not allowed)
  fixed_gt <- NonParametric(pmf = c(0, 0.4, 0.35, 0.25))
  fixed_delay <- LogNormal(meanlog = 1.5, sdlog = 0.5, max = 10)

  out <- suppressWarnings(estimate_infections(
    reported_cases,
    generation_time = gt_opts(fixed_gt),
    delays = delay_opts(fixed_delay),
    stan = stan_opts(chains = 2, warmup = 25, samples = 25),
    verbose = FALSE
  ))

  params <- get_parameters(out)

  # Check generation time is returned correctly
  expect_true("generation_time" %in% names(params))
  gt_returned <- params$generation_time
  expect_s3_class(gt_returned, "dist_spec")
  # For nonparametric, the [[1]] element is the PMF (as vector, drop dim attr)
  expect_equal(as.vector(gt_returned[[1]]), c(0, 0.4, 0.35, 0.25))

  # Check reporting delay is returned correctly
  expect_true("reporting" %in% names(params))
  delay_returned <- params$reporting
  expect_s3_class(delay_returned, "dist_spec")
  # Delays are discretised when passed to Stan, so returned as nonparametric
  # Check that PMF is approximately correct (discretised LogNormal)
  expected_pmf <- as.vector(discretise(fixed_delay)[[1]])
  expect_equal(as.vector(delay_returned[[1]]), expected_pmf)
})

test_that("$samples accessor errors", {
  out <- default_fit
  expect_error(out$samples, "get_samples")
})

test_that("$summarised accessor errors", {
  out <- default_fit
  expect_error(out$summarised, "summary")
})

test_that("[[ accessor handles deprecated elements", {
  out <- default_fit
  expect_error(out[["samples"]], "get_samples")
  expect_error(out[["summarised"]], "summary")

  # Test non-deprecated elements work without error
  expect_no_error(out[["fit"]])
  expect_no_error(out[["args"]])
  expect_no_error(out[["observations"]])
})

test_that("$ accessor works for non-deprecated elements", {
  # Reuse pre-computed fit
  out <- default_fit

  # Test direct access to non-deprecated elements
  expect_no_warning(out$fit)
  expect_no_warning(out$args)
  expect_no_warning(out$observations)

  # Verify the elements are correct
  expect_s4_class(out$fit, "stanfit")
  expect_type(out$args, "list")
  expect_s3_class(out$observations, "data.frame")
})

test_that("summary with type='parameters' has variable with semantic names", {
  # Reuse pre-computed fit
  out <- default_fit

  summ <- summary(out, type = "parameters")

  # Should have a variable column with semantic parameter names
  expect_true("variable" %in% names(summ))
  # Should not have a separate parameter column (merged into variable)
  expect_false("parameter" %in% names(summ))
  # Check that we have semantic names like R, not generic category names
  expect_true("R" %in% summ$variable)
})

test_that("get_predictions works with format='summary'", {
  out <- default_fit

  preds <- get_predictions(out, format = "summary")

  expect_s3_class(preds, "data.table")
  expect_true("date" %in% names(preds))
  expect_true("mean" %in% names(preds))
  expect_true("median" %in% names(preds))
  expect_false("confirm" %in% names(preds))
})

test_that("get_predictions works with format='sample'", {
  out <- default_fit

  preds <- get_predictions(out, format = "sample")

  expect_s3_class(preds, "data.table")
  expect_true(all(c(
    "forecast_date", "date", "horizon", "sample", "predicted"
  ) %in% names(preds)))
  expect_false("observed" %in% names(preds))
  expect_true(nrow(preds) > 0)
  expect_true(is.numeric(preds$predicted))
  expect_true(is.integer(preds$sample))
  expect_true(is.numeric(preds$horizon))
})

test_that("get_predictions works with format='quantile'", {
  out <- default_fit

  preds <- get_predictions(out, format = "quantile")

  expect_s3_class(preds, "data.table")
  expect_true(all(c(
    "forecast_date", "date", "horizon", "quantile_level", "predicted"
  ) %in% names(preds)))
  expect_true(all(c(0.05, 0.25, 0.5, 0.75, 0.95) %in% preds$quantile_level))
})

test_that("get_predictions default format is 'summary'", {
  out <- default_fit

  preds_default <- get_predictions(out)
  preds_explicit <- get_predictions(out, format = "summary")

  expect_equal(names(preds_default), names(preds_explicit))
  expect_equal(preds_default, preds_explicit)
})

test_that("get_predictions forecast_date equals last observation date", {
  out <- default_fit

  preds <- get_predictions(out, format = "sample")

  expected_forecast_date <- max(out$observations$date, na.rm = TRUE)
  expect_equal(unique(preds$forecast_date), expected_forecast_date)
})

test_that("get_predictions horizon is correctly calculated", {
  out <- default_fit

  preds <- get_predictions(out, format = "sample")

  expected_horizon <- as.numeric(preds$date - preds$forecast_date)
  expect_equal(preds$horizon, expected_horizon)
})

test_that("get_predictions format='sample' compatible with scoringutils", {

  skip_integration()
  skip_if_not_installed("scoringutils")

  reported_cases <- example_confirmed[1:30]
  fit <- estimate_infections(
    reported_cases,
    generation_time = gt_opts(example_generation_time),
    delays = delay_opts(
      example_incubation_period + example_reporting_delay
    ),
    rt = rt_opts(prior = LogNormal(mean = 2, sd = 0.2)),
    stan = stan_opts(
      samples = 25, warmup = 25,
      chains = 2, cores = 1,
      control = list(adapt_delta = 0.8)
    ),
    forecast = forecast_opts(horizon = 7),
    verbose = FALSE
  )

  preds <- get_predictions(fit, format = "sample")
  forecasts <- preds[horizon > 0]
  forecasts <- merge(
    forecasts, data.table::as.data.table(example_confirmed), by = "date"
  )

  forecast_obj <- scoringutils::as_forecast_sample(
    forecasts,
    forecast_unit = "horizon",
    observed = "confirm",
    sample_id = "sample"
  )
  expect_s3_class(forecast_obj, "forecast_sample")

  scores <- scoringutils::score(forecast_obj)
  expect_s3_class(scores, "data.table")
  expect_true("crps" %in% names(scores))
})

test_that("get_predictions format='quantile' compatible with scoringutils", {
  skip_integration()
  skip_if_not_installed("scoringutils")

  reported_cases <- example_confirmed[1:30]
  fit <- estimate_infections(
    reported_cases,
    generation_time = gt_opts(example_generation_time),
    delays = delay_opts(
      example_incubation_period + example_reporting_delay
    ),
    rt = rt_opts(prior = LogNormal(mean = 2, sd = 0.2)),
    stan = stan_opts(
      samples = 25, warmup = 25,
      chains = 2, cores = 1,
      control = list(adapt_delta = 0.8)
    ),
    forecast = forecast_opts(horizon = 7),
    verbose = FALSE
  )

  preds <- get_predictions(fit, format = "quantile")
  forecasts <- preds[horizon > 0]
  forecasts <- merge(
    forecasts, data.table::as.data.table(example_confirmed), by = "date"
  )

  forecast_obj <- scoringutils::as_forecast_quantile(
    forecasts,
    forecast_unit = "horizon",
    observed = "confirm",
    quantile_level = "quantile_level"
  )
  expect_s3_class(forecast_obj, "forecast_quantile")

  scores <- scoringutils::score(forecast_obj)
  expect_s3_class(scores, "data.table")
  expect_true("wis" %in% names(scores))
})

test_that("as_forecast_sample.estimate_infections produces a valid forecast_sample", {
  skip_if_not_installed("scoringutils")

  forecast_obj <- scoringutils::as_forecast_sample(
    default_fit,
    observations = example_confirmed
  )
  expect_s3_class(forecast_obj, "forecast_sample")
  expect_no_error(
    scoringutils::assert_forecast(forecast_obj, verbose = FALSE)
  )
})

test_that("as_forecast_sample horizon arg sets the lower bound", {
  skip_if_not_installed("scoringutils")

  default_obj <- scoringutils::as_forecast_sample(
    default_fit,
    observations = example_confirmed
  )
  expect_true(all(default_obj$horizon >= 0))

  later_obj <- scoringutils::as_forecast_sample(
    default_fit,
    observations = example_confirmed,
    horizon = 5
  )
  expect_true(all(later_obj$horizon >= 5))

  all_obj <- scoringutils::as_forecast_sample(
    default_fit,
    observations = example_confirmed,
    horizon = -Inf
  )
  expect_true(any(all_obj$horizon < 0))
})

test_that("as_forecast_sample errors when observations are missing or invalid", {
  skip_if_not_installed("scoringutils")

  expect_error(
    scoringutils::as_forecast_sample(default_fit),
    "observations"
  )
  expect_error(
    scoringutils::as_forecast_sample(
      default_fit,
      observations = data.frame(date = example_confirmed$date)
    ),
    "confirm"
  )

  duped <- rbind(
    data.table::as.data.table(example_confirmed),
    data.table::as.data.table(example_confirmed[1])
  )
  expect_error(
    scoringutils::as_forecast_sample(default_fit, observations = duped),
    "duplicated"
  )

  no_overlap <- data.table::copy(example_confirmed)
  no_overlap$date <- no_overlap$date + as.difftime(1000, units = "days")
  expect_error(
    scoringutils::as_forecast_sample(default_fit, observations = no_overlap),
    "merging"
  )
})

test_that("as_forecast_sample.epinow dispatches and surfaces failed-run errors", {
  skip_if_not_installed("scoringutils")

  # Synthesise an epinow object by prepending the class to a successful fit
  fake_epinow <- default_fit
  class(fake_epinow) <- c("epinow", class(fake_epinow))

  forecast_obj <- scoringutils::as_forecast_sample(
    fake_epinow,
    observations = example_confirmed
  )
  expect_s3_class(forecast_obj, "forecast_sample")

  failed <- list(error = "boom")
  class(failed) <- c("epinow", "list")
  expect_error(
    scoringutils::as_forecast_sample(failed, observations = example_confirmed),
    "failed"
  )
})
