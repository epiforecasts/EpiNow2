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

test_that("create_stan_params gives components of the same type unique names", {
  prior <- initial(LogNormal(mean = 1, sd = 1)) + GP() +
    GP(kernel = "periodic", ls = LogNormal(mean = 7, sd = 2, max = 30)) +
    RW(knots = 10L) + RW()
  params <- list(
    make_param("R", prior, lower_bound = 0), make_param("I", NULL)
  )
  sd <- create_stan_params(
    params, states_supported = c("R", "I"), seeding_time = 7, t = 60,
    horizon = 7
  )
  ids <- unlist(sd[grepl("^param_id_", names(sd))])
  expect_false(anyDuplicated(names(ids)) > 0)
  expect_false(anyDuplicated(ids[ids > 0]) > 0)
  expect_true(all(c(
    "param_id_R_gp_alpha", "param_id_R_gp2_alpha", "param_id_R_gp_rho",
    "param_id_R_gp2_rho", "param_id_R_rw_sd", "param_id_R_rw2_sd"
  ) %in% names(ids)))
})
