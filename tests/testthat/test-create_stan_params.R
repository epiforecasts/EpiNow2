test_that("create_stan_params can be used with a scaling", {
  obs <- obs_opts(scale = Normal(mean = 0.4, sd = 0.01))
  params <- create_stan_params(list(
    make_param("fraction_observed", obs$scale, lower_bound = 0)
  ))
  expect_equal(params$prior_dist, array(2L))
  expect_equal(params$prior_dist_params, array(c(0.4, 0.01)))
  expect_equal(params$params_lower, array(0))
  expect_equal(params$param_id_fraction_observed, 1L)
})

test_that("create_stan_params can be used with fixed scaling", {
  obs <- obs_opts(scale = Fixed(0.4))
  params <- create_stan_params(list(
    make_param("fraction_observed", obs$scale)
  ))
  expect_equal(params$params_value, array(0.4))
  expect_equal(length(params$prior_dist_params), 0L)
})

test_that("create_stan_params can be used with a user set phi", {
  obs <- obs_opts(
    dispersion = Normal(mean = 10, sd = 0.1)
  )
  params <- create_stan_params(list(
    make_param("reporting_overdispersion", obs$dispersion)
  ))
  expect_equal(params$prior_dist, array(2L))
  expect_equal(params$prior_dist_params, array(c(10, 0.1)))
  expect_equal(params$param_id_reporting_overdispersion, 1L)
})

test_that("create_stan_params can be used with fixed dispersion", {
  obs <- obs_opts(dispersion = Fixed(0.5))
  params <- create_stan_params(list(
    make_param("reporting_overdispersion", obs$dispersion)
  ))
  expect_equal(params$params_value, array(0.5))
  expect_equal(length(params$prior_dist_params), 0L)
})

test_that("create_stan_params can be used with NULL parameters", {
  params <- create_stan_params(list(
    make_param("test", NULL)
  ))
  expect_equal(params$param_id_test, 0)
})

test_that("create_stan_params warns about uncertain parameters", {
  expect_error(create_stan_params(list(
    make_param("test", Normal(mean = 0, sd = Normal(1, 1)))
  )), "cannot have uncertain parameters")
})
