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
  result <- create_sampling_log_message(args_cmdstanr, "sampling")
  expect_type(result, "character")
  expect_match(result, "exact mode")
  expect_match(result, "4000 samples")
  expect_match(result, "4 chains")
  expect_match(result, "500 iterations")
})

test_that("create_sampling_log_message works for stanmodel objects", {
  result <- create_sampling_log_message(args_rstan, "sampling")
  expect_type(result, "character")
  expect_match(result, "exact mode")
  expect_match(result, "2000 samples")
  expect_match(result, "2 chains")
  expect_match(result, "1000 iterations")
})

test_that("create_sampling_log_message includes time steps when t is present", {
  args <- args_cmdstanr
  args$data$t <- 30
  result <- create_sampling_log_message(args, "sampling")
  expect_match(result, "30 time steps")
})

test_that("create_sampling_log_message excludes time steps when t is NULL", {
  args <- args_cmdstanr
  args$data$t <- NULL
  result <- create_sampling_log_message(args, "sampling")
  expect_no_match(result, "time steps")
})

test_that("create_sampling_log_message includes forecast when both t and horizon are present", {
  args <- args_cmdstanr
  args$data$t <- 30
  args$data$horizon <- 7
  result <- create_sampling_log_message(args, "sampling")
  expect_match(result, "30 time steps")
  expect_match(result, "7 are a forecast")
})

test_that("create_sampling_log_message excludes forecast when horizon is NULL", {
  args <- args_cmdstanr
  args$data$t <- 30
  args$data$horizon <- NULL
  result <- create_sampling_log_message(args, "sampling")
  expect_match(result, "30 time steps")
  expect_no_match(result, "forecast")
})

test_that("create_sampling_log_message excludes forecast when t is NULL", {
  args <- args_cmdstanr
  args$data$t <- NULL
  args$data$horizon <- 7
  result <- create_sampling_log_message(args, "sampling")
  expect_no_match(result, "time steps")
  expect_no_match(result, "forecast")
})

test_that("create_sampling_log_message works for rstan with t and horizon", {
  args <- args_rstan
  args$data$t <- 30
  args$data$horizon <- 7
  result <- create_sampling_log_message(args, "sampling")
  expect_match(result, "exact mode")
  expect_match(result, "2000 samples")
  expect_match(result, "2 chains")
  expect_match(result, "1000 iterations")
  expect_match(result, "30 time steps")
  expect_match(result, "7 are a forecast")
})

# Tests for VB approximate mode
test_that("create_sampling_log_message works for VB method", {
  args <- list(
    iter = 10000,
    trials = 10,
    output_samples = 2000,
    data = list()
  )
  result <- create_sampling_log_message(args, "vb")
  expect_type(result, "character")
  expect_match(result, "approximate mode")
  expect_match(result, "10000 iterations")
  expect_match(result, "10 attempts")
  expect_match(result, "2000 approximate posterior samples")
})

test_that("create_sampling_log_message works for VB with time steps and forecast", {
  args <- list(
    iter = 10000,
    trials = 10,
    output_samples = 2000,
    data = list(t = 30, horizon = 7)
  )
  result <- create_sampling_log_message(args, "vb")
  expect_match(result, "approximate mode")
  expect_match(result, "10000 iterations")
  expect_match(result, "2000 approximate posterior samples")
  expect_match(result, "30 time steps")
  expect_match(result, "7 are a forecast")
})

# Tests for Laplace approximate mode
test_that("create_sampling_log_message works for Laplace method", {
  args <- list(
    trials = 10,
    data = list()
  )
  result <- create_sampling_log_message(args, "laplace")
  expect_type(result, "character")
  expect_match(result, "approximate mode")
  expect_match(result, "Laplace approximation")
  expect_match(result, "10 attempts")
})

test_that("create_sampling_log_message works for Laplace with time steps and forecast", {
  args <- list(
    trials = 10,
    data = list(t = 30, horizon = 7)
  )
  result <- create_sampling_log_message(args, "laplace")
  expect_match(result, "Laplace approximation")
  expect_match(result, "10 attempts")
  expect_match(result, "30 time steps")
  expect_match(result, "7 are a forecast")
})

# Tests for Pathfinder approximate mode
test_that("create_sampling_log_message works for Pathfinder method", {
  args <- list(
    trials = 10,
    draws = 2000,
    data = list()
  )
  result <- create_sampling_log_message(args, "pathfinder")
  expect_type(result, "character")
  expect_match(result, "approximate mode")
  expect_match(result, "pathfinder")
  expect_match(result, "10 attempts")
  expect_match(result, "2000 approximate posterior samples")
})

test_that("create_sampling_log_message works for Pathfinder with time steps and forecast", {
  args <- list(
    trials = 10,
    draws = 2000,
    data = list(t = 30, horizon = 7)
  )
  result <- create_sampling_log_message(args, "pathfinder")
  expect_match(result, "pathfinder")
  expect_match(result, "10 attempts")
  expect_match(result, "2000 approximate posterior samples")
  expect_match(result, "30 time steps")
  expect_match(result, "7 are a forecast")
})
