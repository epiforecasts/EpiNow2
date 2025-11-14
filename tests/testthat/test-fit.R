# Setup for testing logging features
futile.logger::flog.threshold("DEBUG")

# Set up arguments for cmdstanr and rstan
# Backend is auto-detected from the object class

# Create mock objects for testing
cmdstanr_object <- structure(list(), class = "CmdStanModel")
rstan_object <- structure(list(), class = "stanmodel")

args_cmdstanr <- list(
  object = cmdstanr_object,
  iter_sampling = 1000,
  iter_warmup = 500,
  chains = 4
)

args_rstan <- list(
  object = rstan_object,
  iter = 2000,
  warmup = 1000,
  chains = 2
)

test_that("create_sampler_logging_vars returns expected samples and iterations for CmdStanModel objects", {
  result <- create_sampler_logging_vars(args_cmdstanr)
  expect_equal(result$total_samples, 4000)
  expect_equal(result$warmup_iterations, 500)
})

test_that("create_sampler_logging_vars returns expected samples and iterations for stanmodel objects", {
  result <- create_sampler_logging_vars(args_rstan)
  expect_equal(result$total_samples, 2000)
  expect_equal(result$warmup_iterations, 1000)
})

test_that("create_sampler_logging_vars handles missing fields gracefully", {
  args <- args_cmdstanr
  args$iter_warmup <- NULL
  expect_no_error(create_sampler_logging_vars(args))

  args <- args_rstan
  args$warmup <- NULL
  expect_no_error(create_sampler_logging_vars(args))
})

test_that("create_sampler_logging_vars output names are the same for both backends", {
  out_cmdstanr <- create_sampler_logging_vars(args_cmdstanr)
  out_rstan <- create_sampler_logging_vars(args_rstan)
  expect_equal(names(out_cmdstanr), names(out_rstan))
})

test_that("logging component logs correct sampling information for cmdstanr", {
  # Mock arguments that would normally be passed
  #cmdstanr args
  cmdstanr_args <- list(
    object = cmdstanr_object,
    iter_sampling = 100,
    iter_warmup = 50,
    chains = 2,
    data = list(t = 30, horizon = 7)
  )
  # Capture cmdstanr log messages without actually fitting
  cmdstanr_log_messages <- capture.output(
    {
      sampler_vars <- create_sampler_logging_vars(cmdstanr_args)
      futile.logger::flog.debug(
        paste0(
          "test: Running in exact mode for ",
          sampler_vars$total_samples,
          " samples (across ", cmdstanr_args$chains,
          " chains each with a warm up of ", sampler_vars$warmup_iterations,
          " iterations each) and ",
          cmdstanr_args$data$t, " time steps of which ",
          cmdstanr_args$data$horizon, " are a forecast"
        )
      )
    },
    type = "output"
  )
  
  # Verify expected values appear in log
  expect_match(paste(cmdstanr_log_messages, collapse = " "), "200 samples")
  expect_match(paste(cmdstanr_log_messages, collapse = " "), "2 chains")
  expect_match(paste(cmdstanr_log_messages, collapse = " "), "50 iterations")
  expect_match(paste(cmdstanr_log_messages, collapse = " "), "30 time steps")
  expect_match(paste(cmdstanr_log_messages, collapse = " "), "7 are a forecast")
})

test_that("logging component logs correct sampling information for rstan", {
  # Mock arguments that would normally be passed
  # rstan args
  rstan_args <- list(
    object = rstan_object,
    iter = 100,
    warmup = 50,
    chains = 2,
    data = list(t = 30, horizon = 7)
  )
  
  # Capture rstan log messages without actually fitting
  rstan_log_messages <- capture.output(
    {
      sampler_vars <- create_sampler_logging_vars(rstan_args)
      futile.logger::flog.debug(
        paste0(
          "test: Running in exact mode for ",
          sampler_vars$total_samples,
          " samples (across ", rstan_args$chains,
          " chains each with a warm up of ", sampler_vars$warmup_iterations,
          " iterations each) and ",
          rstan_args$data$t, " time steps of which ",
          rstan_args$data$horizon, " are a forecast"
        )
      )
    },
    type = "output"
  )
  
  # Verify expected values appear in log
  expect_match(paste(rstan_log_messages, collapse = " "), "100 samples")
  expect_match(paste(rstan_log_messages, collapse = " "), "2 chains")
  expect_match(paste(rstan_log_messages, collapse = " "), "50 iterations")
  expect_match(paste(rstan_log_messages, collapse = " "), "30 time steps")
  expect_match(paste(rstan_log_messages, collapse = " "), "7 are a forecast")
})
