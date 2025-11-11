skip_on_cran()
# Setup for testing -------------------------------------------------------

futile.logger::flog.threshold("FATAL")

reported_cases <- EpiNow2::example_confirmed[1:30]

default_estimate_infections <- function(..., add_stan = list(), gt = TRUE,
                                        delay = TRUE) {
  futile.logger::flog.threshold("FATAL")

  def_stan <- list(
    cores = 1, chains = 2, warmup = 50, samples = 50,
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

# Test functionality ------------------------------------------------------

test_that("estimate_infections successfully returns estimates using default settings", {
  test_estimate_infections(reported_cases)
})

test_that("estimate_infections successfully returns estimates using the infectiousness growth rate estimator", {
  test_estimate_infections(
    reported_cases,
    rt = rt_opts(growth_method = "infectiousness")
  )
})

test_that("estimate_infections successfully returns estimates using a Matern 5/2 kernel", {
  test_estimate_infections(
    reported_cases,
    gp = gp_opts(kernel = "matern", matern_order = 5 / 2)
  )
})

test_that("estimate_infections successfully returns estimates using a periodic kernel", {
  test_estimate_infections(
    reported_cases,
    gp = gp_opts(kernel = "periodic")
  )
})

test_that("estimate_infections successfully returns estimates when passed NA values", {
  reported_cases_na <- data.table::copy(reported_cases)
  reported_cases_na[sample(1:30, 5), confirm := NA]
  test_estimate_infections(reported_cases_na)
})

test_that("estimate_infections successfully returns estimates when accumulating to weekly", {
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

test_that("estimate_infections successfully returns estimates using no delays", {
  test_estimate_infections(reported_cases, delay = FALSE)
})
test_that("estimate_infections successfully returns estimates using the poisson observation model", {
  test_estimate_infections(reported_cases, obs = obs_opts(family = "poisson"))
})

test_that("estimate_infections successfully returns estimates using backcalculation", {
  test_estimate_infections(reported_cases, rt = NULL)
})

test_that("estimate_infections successfully returns estimates using a fixed Rt", {
  test_estimate_infections(reported_cases, gp = NULL)
})

test_that("estimate_infections successfully returns estimates using only mean shifted reported cases", {
  test_estimate_infections(reported_cases, gp = NULL, rt = NULL)
})

test_that("estimate_infections successfully returns estimates using a single breakpoint", {
  test_estimate_infections(data.table::copy(reported_cases)[, breakpoint := ifelse(date == "2020-03-10", 1, 0)],
    gp = NULL
  )
})

test_that("estimate_infections successfully returns estimates using a random walk", {
  test_estimate_infections(reported_cases, gp = NULL, rt = rt_opts(rw = 7))
})

test_that("estimate_infections works without setting a generation time", {
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
  test_estimate_infections(reported_cases, gp = gp_opts(kernel = "se"))
  test_estimate_infections(reported_cases, gp = gp_opts(kernel = "ou"))
  test_estimate_infections(reported_cases, gp = gp_opts(matern_order = 5 / 2))
})

test_that("estimate_infections fails as expected when given a very short timeout", {
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
  out <- test_estimate_infections(data = reported_cases, forecast = NULL)
  expect_true(!"forecast" %in% unique(out$summarised$type))
  expect_true(out$args$horizon == 0)
})

test_that("estimate_infections produces no forecasts when forecast_opts horizon is 0", {
  out <- test_estimate_infections(
    data = reported_cases, forecast = forecast_opts(horizon = 0)
  )
   expect_true(!"forecast" %in% unique(out$summarised$type))
  expect_true(out$args$horizon == 0)
})

test_that("estimate_infections can sample from the prior", {
  reported_cases[, confirm := NA]
  test_estimate_infections(reported_cases)
})

test_that("estimate_infections output contains breakpoints effect when breakpoints are present", {
  data <- data.table::copy(reported_cases)
  # Add breakpoints at two dates
  bp_dates <- as.Date(c("2020-02-25", "2020-03-05", "2020-03-15"))
  data[, breakpoint := ifelse(date %in% bp_dates, 1, 0)]
  out <- default_estimate_infections(data, gp = NULL)
  samples <- get_samples(out)
  # Should have a breakpoints effect in samples
  expect_true("breakpoints" %in% unique(samples$variable))
  # Should have at least as many unique breakpoints as in the data
  expect_true(length(unique(samples[variable == "breakpoints"]$strat)) == length(bp_dates))
})

test_that("estimate_infections output does not contain breakpoints effect when breakpoints are not present", {
  data <- data.table::copy(reported_cases)
  data[, breakpoint := 0]
  out <- default_estimate_infections(data, gp = NULL)
  samples <- get_samples(out)
  expect_false("breakpoints" %in% unique(samples$variable))
})

# Deprecation tests -------------------------------------------------------

test_that("summary.estimate_infections with type = 'samples' is deprecated", {
  out <- default_estimate_infections(reported_cases)

  expect_deprecated(summary(out, type = "samples"))

  # Verify it returns the same as get_samples()
  withr::local_options(lifecycle_verbosity = "quiet")
  samples_quiet <- summary(out, type = "samples")
  samples_new <- get_samples(out)

  expect_equal(samples_quiet, samples_new)
})

test_that("extract_parameter_samples is deprecated", {
  # Create a simple fit to test with
  out <- default_estimate_infections(reported_cases)

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
