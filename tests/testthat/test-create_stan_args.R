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
