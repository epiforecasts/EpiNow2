test_that("stan_sampling_opts splits samples across chains for rstan", {
  opts <- stan_sampling_opts(
    cores = 2, warmup = 100, samples = 1000, chains = 3, seed = 1
  )
  expect_equal(opts$chains, 3)
  expect_equal(opts$cores, 2)
  expect_equal(opts$warmup, 100)
  # rstan counts warmup within iter; samples per chain are rounded up
  expect_equal(opts$iter, ceiling(1000 / 3) + 100)
  expect_equal(opts$control, list(adapt_delta = 0.9, max_treedepth = 12))
  expect_equal(opts$seed, 1)
  expect_false(opts$save_warmup)
  expect_false(opts$future)
  expect_equal(opts$max_execution_time, Inf)
})

test_that("stan_sampling_opts uses cmdstanr argument names", {
  opts <- stan_sampling_opts(
    cores = 2, warmup = 100, samples = 1000, chains = 4, seed = 1,
    backend = "cmdstanr"
  )
  expect_equal(opts$parallel_chains, 2)
  expect_equal(opts$iter_warmup, 100)
  expect_equal(opts$iter_sampling, 250)
  # control settings are passed as top level arguments to cmdstanr
  expect_equal(opts$adapt_delta, 0.9)
  expect_equal(opts$max_treedepth, 12)
  expect_null(opts$iter)
  expect_null(opts$control)
  expect_null(opts$cores)
})

test_that("stan_sampling_opts merges user control with the defaults", {
  opts <- stan_sampling_opts(control = list(adapt_delta = 0.99), seed = 1)
  expect_equal(opts$control, list(adapt_delta = 0.99, max_treedepth = 12))
  opts <- stan_sampling_opts(
    control = list(max_treedepth = 15), backend = "cmdstanr", seed = 1
  )
  expect_equal(opts$adapt_delta, 0.9)
  expect_equal(opts$max_treedepth, 15)
})

test_that("stan_sampling_opts warns and drops iter arguments", {
  expect_warning(
    opts <- stan_sampling_opts(
      warmup = 100, samples = 400, chains = 4, iter = 10, seed = 1
    ),
    "samples"
  )
  expect_equal(opts$iter, 200)
  expect_warning(
    opts <- stan_sampling_opts(
      samples = 400, chains = 4, iter_sampling = 10, seed = 1,
      backend = "cmdstanr"
    ),
    "samples"
  )
  expect_equal(opts$iter_sampling, 100)
  expect_false("iter" %in% names(opts))
})

test_that("stan_sampling_opts passes on additional arguments", {
  opts <- stan_sampling_opts(seed = 1, refresh = 0)
  expect_equal(opts$refresh, 0)
})

test_that("stan_sampling_opts errors for an unknown backend", {
  expect_error(stan_sampling_opts(backend = "pymc"), "backend")
})

test_that("stan_vb_opts returns the expected arguments", {
  expect_equal(
    stan_vb_opts(),
    list(trials = 10, iter = 10000, output_samples = 2000)
  )
  expect_equal(
    stan_vb_opts(samples = 100, trials = 2, iter = 50, tol_rel_obj = 0.1),
    list(trials = 2, iter = 50, output_samples = 100, tol_rel_obj = 0.1)
  )
})

test_that("stan_laplace_opts returns the expected arguments", {
  expect_equal(stan_laplace_opts(), list(trials = 10))
  expect_equal(
    stan_laplace_opts(trials = 3, draws = 100),
    list(trials = 3, draws = 100)
  )
})

test_that("stan_laplace_opts errors for the rstan backend", {
  expect_error(stan_laplace_opts(backend = "rstan"), "cmdstanr")
})

test_that("stan_pathfinder_opts maps samples to draws", {
  expect_equal(stan_pathfinder_opts(), list(trials = 10, draws = 2000))
  expect_equal(
    stan_pathfinder_opts(samples = 100, trials = 2, num_paths = 4),
    list(trials = 2, draws = 100, num_paths = 4)
  )
})

test_that("stan_pathfinder_opts errors for the rstan backend", {
  expect_error(stan_pathfinder_opts(backend = "rstan"), "cmdstanr")
})

test_that("stan_opts works as expected with default arguments", {
  opts <- stan_opts(seed = 1)
  expect_s3_class(opts, "stan_opts")
  expect_equal(opts$backend, "rstan")
  expect_equal(opts$method, "sampling")
  expect_null(opts$object)
  expect_true(opts$return_fit)
  expect_equal(opts$iter, 500 + 2000 / 4)
})

test_that("stan_opts passes arguments to the method's option function", {
  opts <- stan_opts(method = "vb", samples = 100, trials = 2)
  expect_equal(opts$method, "vb")
  expect_equal(opts$output_samples, 100)
  expect_equal(opts$trials, 2)

  skip_if_not_installed("cmdstanr")
  opts <- stan_opts(method = "pathfinder", samples = 100, backend = "cmdstanr")
  expect_equal(opts$backend, "cmdstanr")
  expect_equal(opts$draws, 100)
  opts <- stan_opts(method = "laplace", backend = "cmdstanr", trials = 4)
  expect_equal(opts$method, "laplace")
  expect_equal(opts$trials, 4)
})

test_that("stan_opts errors for methods unsupported by rstan", {
  expect_error(stan_opts(method = "laplace"), "cmdstanr")
  expect_error(stan_opts(method = "pathfinder"), "cmdstanr")
})

test_that("stan_opts errors for bad 'method' specifications", {
  expect_error(stan_opts(method = "optimise"), "method")
})

test_that("stan_opts infers the backend from a model object", {
  cmdstan_model <- structure(list(), class = "CmdStanModel")
  opts <- stan_opts(object = cmdstan_model, samples = 400, seed = 1)
  expect_identical(opts$object, cmdstan_model)
  # the backend is not stored when a model object is given
  expect_null(opts$backend)
  expect_equal(opts$iter_sampling, 100)
  expect_null(opts$iter)

  rstan_model <- structure(list(), class = "stanmodel")
  opts <- stan_opts(object = rstan_model, samples = 400, seed = 1)
  expect_identical(opts$object, rstan_model)
  expect_equal(opts$iter, 500 + 100)
})

test_that("stan_opts warns if a backend is given with a model object", {
  rstan_model <- structure(list(), class = "stanmodel")
  expect_warning(
    opts <- stan_opts(object = rstan_model, backend = "rstan", seed = 1),
    "ignored"
  )
  expect_equal(opts$iter, 500 + 2000 / 4)
})

test_that("stan_opts errors for bad 'object' specifications", {
  expect_error(stan_opts(object = list()), "stan model object")
})
