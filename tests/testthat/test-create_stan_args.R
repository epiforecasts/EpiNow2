test_that("create_stan_args returns the expected defaults when the exact method is used", {
  expect_equal(names(create_stan_args()), c(
    "data", "init", "refresh", "object", "method", "chains", "save_warmup",
    "seed", "future", "max_execution_time", "cores", "warmup", "control", "iter",
    "pars", "include"
  ))
})

test_that("create_stan_args returns the expected defaults when the approximate method is used", {
  expect_equal(names(create_stan_args(stan = stan_opts(method = "vb"))), c(
    "data", "init", "refresh",
    "object", "method",
    "trials", "iter", "output_samples",
    "pars", "include"
  ))
})

test_that("create_stan_args can modify arguments", {
  expect_equal(create_stan_args(stan = stan_opts(warmup = 1000))$warmup, 1000)
})

fake_pathfinder_model <- function() {
  fake_fit <- structure(list(), class = "fake_pathfinder_fit")
  structure(list(pathfinder = function(...) fake_fit), class = "CmdStanModel")
}

test_that("create_stan_args does not leak init_method into the returned arguments", {
  args <- create_stan_args(
    stan = stan_opts(object = fake_pathfinder_model(), init_method = "pathfinder")
  )
  expect_false("init_method" %in% names(args))
})

test_that("create_stan_args uses pathfinder output as init when init_method is pathfinder", {
  fake_model <- fake_pathfinder_model()
  args <- create_stan_args(
    stan = stan_opts(object = fake_model, init_method = "pathfinder"),
    data = list(a = 1),
    init = "random"
  )
  expect_identical(args$init, fake_model$pathfinder())
})

test_that("create_stan_args passes stan$seed to the pathfinder call", {
  captured_args <- NULL
  fake_model <- structure(
    list(pathfinder = function(...) {
      captured_args <<- list(...)
      structure(list(), class = "fake_pathfinder_fit")
    }),
    class = "CmdStanModel"
  )
  create_stan_args(
    stan = stan_opts(
      object = fake_model, init_method = "pathfinder", seed = 123
    ),
    data = list(a = 1)
  )
  expect_identical(captured_args$seed, 123)
})

test_that("create_stan_args ignores init_method for non-sampling methods", {
  fake_model <- fake_pathfinder_model()
  stan <- list(
    object = fake_model, method = "vb", backend = "cmdstanr",
    init_method = "pathfinder"
  )
  args <- create_stan_args(
    stan = stan, data = list(a = 1), init = 5
  )
  expect_identical(args$init, 5)
})

test_that("create_stan_args ignores init_method for fixed-parameter sampling", {
  fake_model <- fake_pathfinder_model()
  stan <- list(
    object = fake_model, method = "sampling", backend = "cmdstanr",
    init_method = "pathfinder"
  )
  args <- create_stan_args(
    stan = stan, data = list(a = 1), init = 5, fixed_param = TRUE
  )
  expect_identical(args$init, 5)
})

test_that("create_stan_args excludes deterministic quantities from monitoring for estimate_truncation", {
  # fixed additive noise: reconstructed observations are deterministic
  fixed_noise <- list(param_id_sigma = 2L, params_variable_lookup = c(1L, 0L))
  args <- create_stan_args(model = "estimate_truncation", data = fixed_noise)
  expect_true("delay_np_pmf_use" %in% args$pars)
  expect_true("trunc_obs" %in% args$pars)
  expect_false(args$include)

  # estimated additive noise: reconstructed observations vary and are monitored
  est_noise <- list(param_id_sigma = 2L, params_variable_lookup = c(1L, 2L))
  args <- create_stan_args(model = "estimate_truncation", data = est_noise)
  expect_true("delay_np_pmf_use" %in% args$pars)
  expect_false("trunc_obs" %in% args$pars)
  expect_false(args$include)
})
