# Setup for testing logging features
futile.logger::flog.threshold("FATAL")

# Set up arguments for cmdstanr and rstan
args_cmdstanr <- list(
  backend = "cmdstanr",
  iter_sampling = 1000,
  iter_warmup = 500,
  chains = 4
)

args_rstan <- list(
  backend = "rstan",
  iter = 2000,
  warmup = 1000,
  chains = 2
)

# Helper function to capture logging output
capture_logging_output <- function(expr, log_level = "DEBUG") {
  # Create a temporary log file
  temp_log_file <- tempfile(fileext = ".log")

  # Set up logging to capture output
  futile.logger::flog.threshold(log_level, name = "EpiNow2.epinow.estimate_infections.fit")
  futile.logger::flog.appender(futile.logger::appender.file(temp_log_file),
                              name = "EpiNow2.epinow.estimate_infections.fit")

  # Run the expression
  result <- tryCatch({
    eval(expr)
  }, error = function(e) {
    list(error = e$message)
  })

  # Read the log file
  log_content <- readLines(temp_log_file, warn = FALSE)

  # Reset logging
  futile.logger::flog.threshold("FATAL", name = "EpiNow2.epinow.estimate_infections.fit")
  futile.logger::flog.appender(futile.logger::appender.console(),
                              name = "EpiNow2.epinow.estimate_infections.fit")

  return(list(result = result, logs = log_content))
}

test_that("create_logging_sampler_values works for cmdstanr backend", {
  result <- create_logging_sampler_values(args_cmdstanr)
  expect_equal(result$total_samples, 4000)
  expect_equal(result$warmup_iterations, 500)
})

test_that("create_logging_sampler_values works for rstan backend", {
  result <- create_logging_sampler_values(args_rstan)
  expect_equal(result$total_samples, 2000)
  expect_equal(result$warmup_iterations, 1000)
})

test_that("create_logging_sampler_values handles missing fields gracefully", {
  args <- args_cmdstanr
  args$iter_warmup <- NULL
  expect_error(create_logging_sampler_values(args), NA)

  args <- args_rstan
  args$warmup <- NULL
  expect_error(create_logging_sampler_values(args), NA)
})

test_that("create_logging_sampler_values output names are the same for both backends", {
  out_cmdstanr <- create_logging_sampler_values(args_cmdstanr)
  out_rstan <- create_logging_sampler_values(args_rstan)
  expect_equal(names(out_cmdstanr), names(out_rstan))
})

test_that("fit_model_with_nuts logging works with rstan backend", {
  # Test with rstan backend
  log_output <- capture_logging_output({
    epinow(
      data = reported_cases,
      generation_time = gt_opts(example_generation_time),
      delays = delay_opts(example_incubation_period + example_reporting_delay),
      rt = NULL,
      stan = stan_opts(backend = "rstan"),
      logs = NULL,
      verbose = FALSE
    )
  })
  # Check that the function ran successfully
  expect_false(is.null(log_output$result))
  expect_false("error" %in% names(log_output$result))

  # Get values
  samples <- as.integer(
    regmatches(log_output$logs,
    regexpr("(?<=for )\\d+(?= samples)", log_output$logs, perl = TRUE))
  )
  chains <- as.integer(
    regmatches(log_output$logs,
    regexpr("(?<=across )\\d+(?= chains)", log_output$logs, perl = TRUE))
  )
  iterations <- as.integer(
    regmatches(log_output$logs,
    regexpr("(?<=of )\\d+(?= iterations)", log_output$logs, perl = TRUE))
  )
  # Expectations
  expect_equal(samples, 2000)
  expect_equal(chains, 4)
  expect_equal(iterations, 250)
})

test_that("fit_model_with_nuts logging works with cmdstanr backend", {

  # Test NUTS sampling with cmdstanr backend
  log_output <- capture_logging_output({
    epinow(
      data = reported_cases,
      generation_time = gt_opts(example_generation_time),
      delays = delay_opts(example_incubation_period + example_reporting_delay),
      rt = NULL,
      stan = stan_opts(backend = "cmdstanr"),
      logs = NULL,
      verbose = FALSE
    )
  })

  # Check that the function ran successfully
  expect_false(is.null(log_output$result))
  expect_false("error" %in% names(log_output$result))

  # Get values
  samples <- as.integer(
    regmatches(log_output$logs,
    regexpr("(?<=for )\\d+(?= samples)", log_output$logs, perl = TRUE))
  )
  chains <- as.integer(
    regmatches(log_output$logs,
    regexpr("(?<=across )\\d+(?= chains)", log_output$logs, perl = TRUE))
  )
  iterations <- as.integer(
    regmatches(log_output$logs,
    regexpr("(?<=of )\\d+(?= iterations)", log_output$logs, perl = TRUE))
  )
  # Expectations
  expect_equal(samples, 2000)
  expect_equal(chains, 4)
  expect_equal(iterations, 250)
})
