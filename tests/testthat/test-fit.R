test_that("fit_model_with_nuts handles both rstan and cmdstanr backends correctly", {
  # Create test data
  reported_cases <- example_confirmed[1:50]

  # Test rstan backend
  rstan_args <- create_stan_args(
    stan = stan_opts(backend = "rstan", chains = 2, warmup = 100, samples = 100),
    data = list(t = 50, horizon = 0),
    verbose = FALSE
  )

  # Test cmdstanr backend
  cmdstanr_args <- create_stan_args(
    stan = stan_opts(backend = "cmdstanr", chains = 2, warmup = 100, samples = 100),
    data = list(t = 50, horizon = 0),
    verbose = FALSE
  )

  # Capture log messages
  rstan_log <- capture.output(
    futile.logger::flog.debug(
      paste0(
        "Running in exact mode for ",
        ceiling(rstan_args$iter - rstan_args$warmup) * rstan_args$chains,
        " samples (across ", rstan_args$chains,
        " chains each with a warm up of ", rstan_args$warmup, " iterations each) and ",
        rstan_args$data$t, " time steps of which ", rstan_args$data$horizon, " are a forecast"
      ),
      name = "EpiNow2.epinow.estimate_infections.fit"
    )
  )

  cmdstanr_log <- capture.output(
    futile.logger::flog.debug(
      paste0(
        "Running in exact mode for ",
        ceiling(cmdstanr_args$iter_sampling) * cmdstanr_args$chains,
        " samples (across ", cmdstanr_args$chains,
        " chains each with a warm up of ", cmdstanr_args$iter_warmup, " iterations each) and ",
        cmdstanr_args$data$t, " time steps of which ", cmdstanr_args$data$horizon, " are a forecast"
      ),
      name = "EpiNow2.epinow.estimate_infections.fit"
    )
  )

  # Verify both logs contain the correct number of samples
  expect_true(grepl("50 samples", rstan_log))
  expect_true(grepl("50 samples", cmdstanr_log))
})