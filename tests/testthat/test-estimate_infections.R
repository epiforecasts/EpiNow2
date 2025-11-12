skip_on_cran()
# Setup for testing -------------------------------------------------------

futile.logger::flog.threshold("FATAL")

reported_cases <- EpiNow2::example_confirmed[1:30]

default_estimate_infections <- function(..., add_stan = list(), gt = TRUE,
                                        delay = TRUE) {
  futile.logger::flog.threshold("FATAL")

  def_stan <- list(
    chains = 2, warmup = 50, samples = 50,
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

test_that("estimate_infections warns when individual generation time PMF exceeds data length", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  
  # Create short data (5 rows)
  short_data <- EpiNow2::example_confirmed[1:5]
  
  # Create a long nonparametric PMF (longer than data)
  long_gen_time <- NonParametric(c(0, rep(0.1/9, 9)))  # length 10

  expect_warning(
      estimate_infections(
        data = short_data,
        generation_time = gt_opts(long_gen_time),
        delays = delay_opts(),
        stan = stan_opts(
          chains = 1, 
          warmup = 20, 
          samples = 20,
          control = list(adapt_delta = 0.8)
        ),
        verbose = FALSE
      ),
      classes = c(
        "pmf_combined_longer_than_data",
        "epinow2_maxgt_gt_seeding"
      )
    ),
    "Non-parametric PMFs are longer than the input data"
  )
})

test_that("estimate_infections warns when individual delay PMF exceeds data length", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  
  # Short data
  short_data <- EpiNow2::example_confirmed[1:6]
  
  # Long delay PMF
  long_delay <- NonParametric(rep(0.1/9, 10))  # length 10
  
  expect_warning(
    suppressWarnings(
      estimate_infections(
        data = short_data,
        generation_time = gt_opts(Fixed(1)),
        delays = delay_opts(long_delay),
        stan = stan_opts(
          chains = 1, 
          warmup = 20, 
          samples = 20,
          control = list(adapt_delta = 0.8)
        ),
        verbose = FALSE
      ),
      classes = "pmf_combined_longer_than_data"
    ),
    "Non-parametric PMFs are longer than the input data"
  )
})

test_that("estimate_infections warns when combined delay PMFs exceed data length", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  
  # Short data
  short_data <- EpiNow2::example_confirmed[1:8]
  
  # Create two medium-length PMFs that combine to exceed data length
  gen_time_np <- NonParametric(c(0, rep(0.1/4, 4)))  # length 5
  delay_np <- NonParametric(rep(0.1/5, 5))  # length 5
  
  # Combined length (5 + 5 - 1 = 9) exceeds data (8 rows)
  expect_warning(
    suppressWarnings(
      estimate_infections(
        data = short_data,
        generation_time = gt_opts(gen_time_np),
        delays = delay_opts(delay_np),
        stan = stan_opts(
          chains = 1,
          warmup = 20,
          samples = 20,
          control = list(adapt_delta = 0.8)
        ),
        verbose = FALSE
      ),
      classes = "pmf_individual_longer_than_data"
    ),
    "The combined non-parametric delays PMF is longer"
  )
})

test_that("estimate_infections runs without PMF warnings when lengths are appropriate", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  
  data <- EpiNow2::example_confirmed[1:30]
  
  # Short PMFs that won't trigger warnings
  short_gen_time <- NonParametric(c(0, 0.3, 0.4, 0.2, 0.1))  # length 5
  short_delay <- NonParametric(c(0.2, 0.3, 0.3, 0.2))  # length 4
  
  # Should not produce PMF length warnings
  expect_no_warning(
    suppressWarnings(
      estimate_infections(
        data = data,
        generation_time = gt_opts(short_gen_time),
        delays = delay_opts(short_delay),
        stan = stan_opts(
          chains = 1,
          warmup = 20,
          samples = 20
        ),
        verbose = FALSE
      ),
      classes = c(
        "epinow2_maxgt_gt_seeding",
        "epinow2_uncertain_tail",
        "epinow2_sparse_pmf_tail"
      )
    ),
    class = "pmf_.*_longer_than_data"
  )
})

test_that("estimate_infections warns for multiple long nonparametric PMFs", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  
  short_data <- EpiNow2::example_confirmed[1:7]
  
  # Multiple long PMFs
  long_gen_time <- NonParametric(c(0, rep(0.1/9, 9)))  # length 10
  long_delay <- NonParametric(rep(0.1/11, 12))  # length 12
  
  expect_warning(
    suppressWarnings(
      estimate_infections(
        data = short_data,
        generation_time = gt_opts(long_gen_time),
        delays = delay_opts(long_delay),
        stan = stan_opts(
          chains = 1,
          warmup = 20,
          samples = 20
        ),
        verbose = FALSE
      ),
      classes = "pmf_combined_longer_than_data"
    ),
    "Non-parametric PMFs are longer than the input data"
  )
})

test_that("estimate_infections handles truncation PMF length check", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  
  short_data <- EpiNow2::example_confirmed[1:6]
  
  # Long truncation PMF
  long_trunc <- NonParametric(rep(0.1/9, 10))  # length 10
  
  expect_warning(
    suppressWarnings(
      estimate_infections(
        data = short_data,
        generation_time = gt_opts(Fixed(1)),
        delays = delay_opts(),
        truncation = trunc_opts(long_trunc),
        stan = stan_opts(
          chains = 1,
          warmup = 20,
          samples = 20
        ),
        verbose = FALSE
      ),
      classes = "pmf_combined_longer_than_data"
    ),
    "Non-parametric PMFs are longer than the input data"
  )
})
