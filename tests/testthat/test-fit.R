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

test_that("create_sampler_logging_vars returns expected samples and iterations for cmdstanr backend", {
  result <- create_sampler_logging_vars(args_cmdstanr)
  expect_equal(result$total_samples, 4000)
  expect_equal(result$warmup_iterations, 500)
})

test_that("create_sampler_logging_vars returns expected samples and iterations for rstan backend", {
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
