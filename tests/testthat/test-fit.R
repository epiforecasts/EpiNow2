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
  chains = 4,
  data = list()
)

args_rstan <- list(
  object = rstan_object,
  iter = 2000,
  warmup = 1000,
  chains = 2,
  data = list()
)

test_that("create_sampling_log_message works for CmdStanModel objects", {
  result <- create_sampling_log_message(args_cmdstanr)
  expect_type(result, "character")
  expect_match(result, "4000 samples")
  expect_match(result, "4 chains")
  expect_match(result, "500 iterations")
})

test_that("create_sampling_log_message works for stanmodel objects", {
  result <- create_sampling_log_message(args_rstan)
  expect_type(result, "character")
  expect_match(result, "2000 samples")
  expect_match(result, "2 chains")
  expect_match(result, "1000 iterations")
})

test_that("create_sampling_log_message includes time steps when t is present", {
  args <- args_cmdstanr
  args$data$t <- 30
  result <- create_sampling_log_message(args)
  expect_match(result, "30 time steps")
})

test_that("create_sampling_log_message excludes time steps when t is NULL", {
  args <- args_cmdstanr
  args$data$t <- NULL
  result <- create_sampling_log_message(args)
  expect_no_match(result, "time steps")
})

test_that("create_sampling_log_message includes forecast when both t and horizon are present", {
  args <- args_cmdstanr
  args$data$t <- 30
  args$data$horizon <- 7
  result <- create_sampling_log_message(args)
  expect_match(result, "30 time steps")
  expect_match(result, "7 are a forecast")
})

test_that("create_sampling_log_message excludes forecast when horizon is NULL", {
  args <- args_cmdstanr
  args$data$t <- 30
  args$data$horizon <- NULL
  result <- create_sampling_log_message(args)
  expect_match(result, "30 time steps")
  expect_no_match(result, "forecast")
})

test_that("create_sampling_log_message excludes forecast when t is NULL", {
  args <- args_cmdstanr
  args$data$t <- NULL
  args$data$horizon <- 7
  result <- create_sampling_log_message(args)
  expect_no_match(result, "time steps")
  expect_no_match(result, "forecast")
})

test_that("create_sampling_log_message works for rstan with t and horizon", {
  args <- args_rstan
  args$data$t <- 30
  args$data$horizon <- 7
  result <- create_sampling_log_message(args)
  expect_match(result, "2000 samples")
  expect_match(result, "2 chains")
  expect_match(result, "1000 iterations")
  expect_match(result, "30 time steps")
  expect_match(result, "7 are a forecast")
})
