test_that("stan_sampling_opts defaults to random initialisation", {
  expect_equal(stan_sampling_opts()$init_method, "random")
})

test_that("stan_sampling_opts errors when pathfinder initialisation is requested with rstan", {
  expect_error(
    stan_sampling_opts(init_method = "pathfinder", backend = "rstan"),
    "cmdstanr"
  )
})

test_that("stan_sampling_opts accepts pathfinder initialisation with cmdstanr", {
  expect_equal(
    stan_sampling_opts(
      init_method = "pathfinder", backend = "cmdstanr"
    )$init_method,
    "pathfinder"
  )
})

test_that("stan_opts passes init_method through to stan_sampling_opts", {
  expect_equal(
    stan_opts(backend = "cmdstanr", init_method = "pathfinder")$init_method,
    "pathfinder"
  )
})
