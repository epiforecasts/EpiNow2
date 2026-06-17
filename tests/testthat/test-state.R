test_that("GP() constructs a mean-reverting state spec", {
  gp <- GP(mean = Normal(mean = 5, sd = 1))
  expect_s3_class(gp, "state_spec")
  expect_s3_class(gp, "gp_state")
  expect_identical(gp$type, "gp")
  expect_identical(gp$anchor, "mean")
  expect_s3_class(gp$prior, "dist_spec")
  expect_s3_class(gp$settings, "gp_opts")
})

test_that("GP() constructs a first-difference state spec", {
  gp <- GP(init = Normal(mean = 5, sd = 1))
  expect_identical(gp$anchor, "init")
})

test_that("GP() forwards settings to gp_opts()", {
  gp <- GP(mean = Normal(mean = 5, sd = 1), kernel = "se")
  expect_identical(gp$settings$kernel, "se")
})

test_that("RW() constructs a state spec with a step sd prior", {
  rw <- RW(init = Normal(mean = 5, sd = 1))
  expect_s3_class(rw, "state_spec")
  expect_s3_class(rw, "rw_state")
  expect_identical(rw$type, "rw")
  expect_identical(rw$anchor, "init")
  expect_s3_class(rw$settings$sd, "dist_spec")
})

test_that("RW() accepts a custom step sd prior", {
  rw <- RW(mean = Normal(mean = 5, sd = 1), sd = Normal(mean = 0, sd = 0.05))
  expect_identical(rw$anchor, "mean")
  expect_equal(mean(rw$settings$sd), 0)
})

test_that("state constructors accept a known trajectory vector", {
  gp <- GP(mean = c(1, 2, 3, 2, 1))
  expect_true(is.numeric(gp$prior))
  expect_identical(gp$anchor, "mean")

  rw <- RW(init = c(5, 5, 5))
  expect_true(is.numeric(rw$prior))
})

test_that("state constructors require exactly one of mean/init", {
  expect_error(GP(), "Exactly one")
  expect_error(
    GP(mean = Normal(5, 1), init = Normal(5, 1)), "Exactly one"
  )
  expect_error(RW(), "Exactly one")
})

test_that("state constructors reject invalid anchors", {
  expect_error(GP(mean = "a"), "dist_spec.*numeric")
  expect_error(RW(init = list(1)), "dist_spec.*numeric")
})

test_that("RW() validates the step sd prior", {
  expect_error(RW(init = Normal(5, 1), sd = 0.1), "dist_spec")
})

test_that("is_state_spec() identifies state specs", {
  expect_true(is_state_spec(GP(mean = Normal(5, 1))))
  expect_true(is_state_spec(RW(init = Normal(5, 1))))
  expect_false(is_state_spec(Normal(5, 1)))
  expect_false(is_state_spec(5))
})

test_that("state specs print without error", {
  expect_output(print(GP(mean = Normal(5, 1))), "Gaussian process")
  expect_output(print(RW(init = Normal(5, 1))), "random walk")
  expect_output(print(GP(mean = c(1, 2, 3))), "known mean trajectory")
})

test_that("create_stan_params errors on unsupported time-varying parameters", {
  params <- list(
    make_param("alpha", RW(mean = Normal(0.5, 0.1)), lower_bound = 0)
  )
  expect_error(create_stan_params(params), "not supported by this model")
  expect_error(
    create_stan_params(params, states_supported = "fraction_observed"),
    "is not yet supported"
  )
})

test_that("create_stan_params emits RW state data for fraction_observed", {
  params <- list(
    make_param("fraction_observed", RW(mean = Normal(0.5, 0.1)),
      lower_bound = 0
    )
  )
  out <- create_stan_params(params, states_supported = "fraction_observed")
  expect_identical(out$n_states, 1L)
  expect_identical(out$state_param_id, array(1L))
  expect_identical(out$state_type, array(0L))
  expect_identical(out$state_link, array(0L))
  expect_identical(out$state_pos, array(1L))
  expect_identical(out$n_rw_states, 1L)
  expect_identical(out$n_gp_states, 0L)
  expect_identical(out$rw_sd_dist, array(2L))
  expect_equal(out$rw_sd_dist_params, array(c(0, 0.1)))
  # the level prior flows through the normal parameter machinery
  expect_identical(out$n_params_variable, 1L)
})

test_that("create_stan_params emits GP state data for fraction_observed", {
  params <- list(
    make_param("fraction_observed", GP(mean = Normal(0.5, 0.1)),
      lower_bound = 0
    )
  )
  out <- create_stan_params(params, states_supported = "fraction_observed")
  expect_identical(out$n_states, 1L)
  expect_identical(out$state_type, array(1L))
  expect_identical(out$state_pos, array(1L))
  expect_identical(out$n_rw_states, 0L)
  expect_identical(out$n_gp_states, 1L)
  expect_identical(out$gp_kernel, array(2L)) # matern default
  expect_length(out$gp_alpha_dist_params, 2L)
  expect_length(out$gp_rho_dist_params, 2L)
})

test_that("create_stan_params rejects periodic kernel states", {
  params <- list(
    make_param("fraction_observed", GP(mean = Normal(0.5, 0.1),
      kernel = "periodic"
    ), lower_bound = 0)
  )
  expect_error(
    create_stan_params(params, states_supported = "fraction_observed"),
    "Periodic"
  )
})

test_that("create_stan_params emits state data for reporting_overdispersion", {
  params <- list(
    make_param("fraction_observed", Normal(0.5, 0.1), lower_bound = 0),
    make_param("reporting_overdispersion", RW(mean = Normal(0.3, 0.1)),
      lower_bound = 0
    )
  )
  out <- create_stan_params(
    params,
    states_supported = c("fraction_observed", "reporting_overdispersion")
  )
  expect_identical(out$n_states, 1L)
  expect_identical(out$state_param_id, array(2L)) # second param
  expect_identical(out$n_rw_states, 1L)
})

test_that("create_stan_params emits init-anchor state data (centred + Jacobian)", {
  params <- list(
    make_param("fraction_observed", RW(init = Normal(0.4, 0.05)),
      lower_bound = 0
    )
  )
  out <- create_stan_params(params, states_supported = "fraction_observed")
  expect_identical(out$state_anchor, array(1L))
  expect_identical(out$state_init_dist, array(2L)) # normal
  expect_equal(out$state_init_dist_params, array(c(0.4, 0.05)))
  # the level's prior is moved off the normal prior path
  expect_identical(out$params_prior_skip, array(1L))
})

test_that("mean-anchor states keep their prior on the level", {
  params <- list(
    make_param("fraction_observed", RW(mean = Normal(0.4, 0.05)),
      lower_bound = 0
    )
  )
  out <- create_stan_params(params, states_supported = "fraction_observed")
  expect_identical(out$state_anchor, array(0L))
  expect_identical(out$params_prior_skip, array(0L))
})

test_that("GP init anchor emits non-stationary state data", {
  params <- list(
    make_param("fraction_observed", GP(init = Normal(0.4, 0.05)),
      lower_bound = 0
    )
  )
  out <- create_stan_params(params, states_supported = "fraction_observed")
  expect_identical(out$state_type, array(1L)) # gp
  expect_identical(out$state_anchor, array(1L)) # init
  expect_identical(out$state_init_dist, array(2L)) # normal prior on init
  expect_identical(out$params_prior_skip, array(1L)) # level is scaffolding
})

test_that("create_stan_params is a no-op without states", {
  params <- list(
    make_param("fraction_observed", Normal(0.5, 0.1), lower_bound = 0)
  )
  out <- create_stan_params(params, states_supported = "fraction_observed")
  expect_identical(out$n_states, 0L)
  expect_identical(out$n_rw_states, 0L)
  expect_identical(out$n_gp_states, 0L)
})
