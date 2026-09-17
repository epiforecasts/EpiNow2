skip_on_cran()

skip_if_no_stanr <- function() {
  skip_if_not_installed("stanr")
}

stanr_backends <- c("stanr", "stanli")

test_that("stan_opts works as expected with the stanr backends", {
  skip_if_no_stanr()
  for (backend in stanr_backends) {
    opts <- stan_opts(backend = backend)
    expect_equal(opts$backend, backend)
    expect_equal(opts$engine, "nuts")
    expect_true("iter_warmup" %in% names(opts))
    expect_true("iter_sampling" %in% names(opts))
    expect_true("num_threads" %in% names(opts))
    expect_false("iter" %in% names(opts))
    expect_false("parallel_chains" %in% names(opts))
  }
})

test_that("stan_opts passes the engine through", {
  skip_if_no_stanr()
  expect_equal(stan_opts(backend = "stanli", engine = "walnuts")$engine,
               "walnuts")
  expect_equal(stan_opts(backend = "stanr", engine = "static")$engine,
               "static")
})

test_that("stan_opts errors for bad 'engine' specifications", {
  skip_if_no_stanr()
  expect_error(stan_opts(backend = "stanli", engine = "hmc"))
  expect_error(
    stan_opts(backend = "rstan", engine = "walnuts"),
    "only available"
  )
})

test_that("stan_opts errors for unsupported stanr methods", {
  skip_if_no_stanr()
  for (backend in stanr_backends) {
    for (method in c("vb", "laplace", "pathfinder")) {
      expect_error(
        stan_opts(backend = backend, method = method),
        "sampling"
      )
    }
  }
})

test_that("stan_sampling_opts errors when stanr is combined with future", {
  skip_if_no_stanr()
  for (backend in stanr_backends) {
    expect_error(
      stan_sampling_opts(backend = backend, future = TRUE),
      "future"
    )
  }
})

test_that("stan_opts infers the stanr backend from a model object", {
  skip_if_no_stanr()
  model <- epinow2_stanr_model("simulate_secondary", backend = "stanli")
  opts <- stan_opts(object = model)
  expect_null(opts$backend)
  expect_s3_class(opts$object, "StanModel")
})

test_that("epinow2_stan_model returns a stanr model for each model", {
  skip_if_no_stanr()
  models <- c(
    "estimate_infections", "simulate_infections", "estimate_secondary",
    "simulate_secondary", "estimate_truncation", "estimate_dist", "dist_fit"
  )
  for (model in models) {
    expect_s3_class(
      epinow2_stan_model("stanli", model), "StanModel"
    )
  }
})

test_that("create_stan_args works as expected with the stanr backends", {
  skip_if_no_stanr()
  for (backend in stanr_backends) {
    args <- create_stan_args(stan = stan_opts(backend = backend))
    expect_s3_class(args$object, "StanModel")
    expect_equal(args$init, 2)
    expect_null(args$backend)
  }
})

test_that("create_stan_args sets fixed parameter sampling for stanr", {
  skip_if_no_stanr()
  args <- create_stan_args(
    stan = stan_opts(backend = "stanli"),
    model = "simulate_infections",
    fixed_param = TRUE
  )
  expect_true(args$fixed_param)
  expect_null(args$engine)
})
