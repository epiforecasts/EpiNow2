test_that("create_stan_args returns the expected defaults when the exact method is used", {
  expect_equal(names(create_stan_args()), c(
    "data", "init", "refresh", "object", "method", "chains", "save_warmup",
    "seed", "future", "max_execution_time", "cores", "warmup", "control", "iter"
  ))
})

test_that("create_stan_args returns the expected defaults when the approximate method is used", {
  expect_equal(names(create_stan_args(stan = stan_opts(method = "vb"))), c(
    "data", "init", "refresh",
    "object", "method",
    "trials", "iter", "output_samples"
  ))
})

test_that("create_stan_args can modify arguments", {
  expect_equal(create_stan_args(stan = stan_opts(warmup = 1000))$warmup, 1000)
})
