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

test_that("GP() stores Gaussian process settings", {
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

test_that("RW() accepts integer knots instead of a period", {
  rw <- RW(init = Normal(mean = 1, sd = 1), knots = c(20, 5, 45))
  expect_null(rw$settings$period)
  expect_identical(rw$settings$knots, c(5L, 20L, 45L)) # sorted
})

test_that("RW() accepts Date knots, unresolved", {
  dates <- as.Date(c("2020-03-23", "2020-02-01"))
  rw <- RW(init = Normal(mean = 1, sd = 1), knots = dates)
  expect_s3_class(rw$settings$knots, "Date")
  expect_identical(rw$settings$knots, sort(dates))
})

test_that("RW() rejects supplying both period and knots", {
  expect_error(
    RW(init = Normal(1, 1), period = 7, knots = c(10, 20)),
    "at most one"
  )
})

test_that("RW() validates knots type", {
  expect_error(RW(init = Normal(1, 1), knots = "a"), "Date.*integer")
})

test_that("resolve_state_dates() resolves Date knots against a date frame", {
  dates <- as.Date("2020-01-01") + 0:29
  rw <- RW(init = Normal(1, 1), knots = as.Date(c("2020-01-11", "2020-01-21")))
  resolved <- resolve_state_dates(rw, dates)
  expect_identical(resolved$settings$knots, c(11L, 21L))
})

test_that("resolve_state_dates() leaves integer knots and NULL specs alone", {
  dates <- as.Date("2020-01-01") + 0:29
  rw <- RW(init = Normal(1, 1), knots = c(5L, 10L))
  expect_identical(resolve_state_dates(rw, dates)$settings$knots, c(5L, 10L))
  expect_null(resolve_state_dates(NULL, dates))
})

test_that("resolve_state_dates() errors on a date not in the frame", {
  dates <- as.Date("2020-01-01") + 0:9
  rw <- RW(init = Normal(1, 1), knots = as.Date("2021-01-01"))
  expect_error(resolve_state_dates(rw, dates), "not found")
})

test_that("resolve_state_dates() resolves knots inside a composed spec", {
  dates <- as.Date("2020-01-01") + 0:29
  spec <- initial(LogNormal(1, 1)) + GP() +
    RW(knots = as.Date("2020-01-16"))
  resolved <- resolve_state_dates(spec, dates)
  rw_comp <- resolved$components[[
    which(vapply(resolved$components, `[[`, character(1), "type") == "rw")
  ]]
  expect_identical(rw_comp$settings$knots, 16L)
})

test_that("create_stan_params emits knots-based random walk data", {
  params <- list(
    make_param("R", initial(LogNormal(1, 1)) + RW(knots = c(5L, 15L)),
      lower_bound = 0
    )
  )
  out <- create_stan_params(params, states_supported = "R")
  expect_identical(out$n_rw_knots, 2L)
  expect_identical(out$rw_knots_n, array(2L))
  expect_identical(out$rw_knots_offset, array(0L))
  expect_identical(out$rw_knots, array(c(5L, 15L)))
})

test_that("create_stan_params keeps period-based components out of rw_knots", {
  params <- list(
    make_param("R", initial(LogNormal(1, 1)) + RW(period = 7),
      lower_bound = 0
    )
  )
  out <- create_stan_params(params, states_supported = "R")
  expect_identical(out$n_rw_knots, 0L)
  expect_identical(out$rw_knots_n, array(0L))
  expect_identical(out$state_rw_period, 7L)
})

test_that("state constructors accept a known trajectory vector", {
  gp <- GP(mean = c(1, 2, 3, 2, 1))
  expect_true(is.numeric(gp$prior))
  expect_identical(gp$anchor, "mean")

  rw <- RW(init = c(5, 5, 5))
  expect_true(is.numeric(rw$prior))
})

test_that("state constructors accept at most one of mean/init", {
  expect_error(
    GP(mean = Normal(5, 1), init = Normal(5, 1)), "At most one"
  )
  expect_error(
    RW(mean = Normal(5, 1), init = Normal(5, 1)), "At most one"
  )
})

test_that("GP()/RW() with neither mean nor init give a bare component", {
  gp <- GP()
  expect_s3_class(gp, "state_spec")
  expect_null(gp$anchor)
  expect_null(gp$prior)
  expect_length(gp$components, 1)

  rw <- RW()
  expect_null(rw$anchor)
  expect_length(rw$components, 1)
})

test_that("state constructors reject invalid anchors", {
  expect_error(GP(mean = "a"), "dist_spec.*numeric")
  expect_error(RW(init = list(1)), "dist_spec.*numeric")
})

test_that("RW() validates the step sd prior", {
  expect_error(RW(init = Normal(5, 1), sd = 0.1), "dist_spec")
})

test_that("rt_opts accepts a time-varying (state) prior", {
  expect_s3_class(rt_opts(prior = GP(init = LogNormal(1, 1)))$prior, "state_spec")
  expect_s3_class(rt_opts(prior = RW(init = LogNormal(1, 1)))$prior, "rw_state")
  # a plain distribution is deprecated and auto-converted to a GP state
  lifecycle::expect_deprecated(
    rt <- rt_opts(prior = LogNormal(1, 1))
  )
  expect_s3_class(rt$prior, "state_spec")
})

test_that("is_param_spec() accepts both dist_spec and state_spec", {
  # dist_spec values count as parameter specs via their own class
  expect_true(is_param_spec(Normal(5, 1)))
  expect_true(is_param_spec(LogNormal(1, 1) + Gamma(2, 1))) # multi
  # state_spec values carry the param_spec class directly
  expect_s3_class(GP(mean = Normal(5, 1)), "param_spec")
  expect_s3_class(RW(init = Normal(5, 1)), "param_spec")
  expect_true(is_param_spec(GP(mean = Normal(5, 1))))
  expect_false(is_param_spec(5))
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
  expect_identical(out$comp_type, array(0L))
  expect_identical(out$state_link, array(0L))
  expect_identical(out$comp_pos, array(1L))
  expect_identical(out$n_components, 1L)
  expect_identical(out$state_comp_offset, array(0L))
  expect_identical(out$state_comp_n, array(1L))
  expect_identical(out$n_rw_components, 1L)
  expect_identical(out$n_gp_components, 0L)
  # the step sd is appended to the parameter vector as its own parameter
  expect_identical(out$rw_sd_id, array(2L))
  expect_identical(out$n_params_variable, 2L) # level + step sd
  # level prior (normal(0.5, 0.1)) then the step sd prior (normal(0, 0.1))
  expect_identical(as.integer(out$prior_dist), c(2L, 2L))
  expect_equal(as.numeric(out$prior_dist_params), c(0.5, 0.1, 0, 0.1))
})

test_that("+ composes a baseline with one or more bare components", {
  spec <- constant(LogNormal(2, 0.2)) + GP()
  expect_s3_class(spec, "trajectory_spec")
  expect_identical(spec$anchor, "mean")
  expect_length(spec$components, 1)
  expect_identical(spec$components[[1]]$type, "gp")

  spec2 <- initial(LogNormal(1, 1)) + GP() + RW(period = 7)
  expect_identical(spec2$anchor, "init")
  expect_length(spec2$components, 2)
  expect_identical(
    vapply(spec2$components, `[[`, character(1), "type"), c("gp", "rw")
  )

  # the baseline may also come from a single anchored component
  spec3 <- GP(mean = LogNormal(2, 0.2)) + RW(period = 7)
  expect_identical(spec3$anchor, "mean")
  expect_length(spec3$components, 2)
})

test_that("+ is commutative in which side carries the baseline", {
  a <- constant(LogNormal(2, 0.2)) + GP()
  b <- GP() + constant(LogNormal(2, 0.2))
  expect_identical(a$anchor, b$anchor)
  expect_identical(a$prior, b$prior)
  expect_identical(
    vapply(a$components, `[[`, character(1), "type"),
    vapply(b$components, `[[`, character(1), "type")
  )
})

test_that("+ errors when both sides already carry a baseline", {
  expect_error(
    constant(LogNormal(2, 0.2)) + GP(mean = Normal(1, 1)), "only one baseline"
  )
  expect_error(
    GP(mean = Normal(1, 1)) + RW(init = Normal(1, 1)), "only one baseline"
  )
})

test_that("+ errors when the right-hand side is not a state spec", {
  expect_error(constant(LogNormal(2, 0.2)) + 1, "time-varying state")
})

test_that("composed trajectory_spec prints without error", {
  expect_output(
    print(constant(LogNormal(2, 0.2)) + GP() + RW(period = 7)),
    "Gaussian process"
  )
  expect_output(
    print(initial(LogNormal(1, 1)) + GP() + RW(period = 7)), "random walk"
  )
  expect_output(print(GP()), "bare component")
})

test_that("create_stan_params emits a composed RW+GP state (restores main's
  composition)", {
  params <- list(
    make_param(
      "R", initial(LogNormal(1, 1)) + GP() + RW(period = 7), lower_bound = 0
    )
  )
  out <- create_stan_params(params, states_supported = "R")
  expect_identical(out$n_states, 1L)
  expect_identical(out$n_components, 2L)
  expect_identical(out$state_comp_offset, array(0L))
  expect_identical(out$state_comp_n, array(2L))
  expect_identical(out$comp_type, array(c(1L, 0L))) # gp then rw
  expect_identical(out$n_rw_components, 1L)
  expect_identical(out$n_gp_components, 1L)
  expect_identical(out$state_anchor, array(1L)) # init
})

test_that("create_stan_params errors on a trajectory with no baseline", {
  params <- list(make_param("R", GP() + RW(period = 7), lower_bound = 0))
  expect_error(
    create_stan_params(params, states_supported = "R"), "no baseline"
  )
})

test_that("create_stan_params emits GP state data for fraction_observed", {
  params <- list(
    make_param("fraction_observed", GP(mean = Normal(0.5, 0.1)),
      lower_bound = 0
    )
  )
  out <- create_stan_params(params, states_supported = "fraction_observed")
  expect_identical(out$n_states, 1L)
  expect_identical(out$comp_type, array(1L))
  expect_identical(out$comp_pos, array(1L))
  expect_identical(out$n_rw_components, 0L)
  expect_identical(out$n_gp_components, 1L)
  expect_identical(out$gp_kernel, array(2L)) # matern default
  # magnitude and lengthscale are appended as their own parameters
  expect_identical(out$gp_alpha_id, array(2L))
  expect_identical(out$gp_rho_id, array(3L))
  expect_identical(out$n_params_variable, 3L) # level + magnitude + lengthscale
})

test_that("create_stan_params resolves the future setting into model data", {
  gp <- function(future) {
    make_param(
      "R", GP(init = LogNormal(mean = 1, sd = 0.5), future = future),
      lower_bound = 0
    )
  }
  latest <- create_stan_params(
    list(make_param("R", GP(init = LogNormal(mean = 1, sd = 0.5)),
      lower_bound = 0)),
    states_supported = "R", seeding_time = 7
  )
  expect_equal(as.integer(latest$state_future_fixed), 1L)
  expect_equal(as.integer(latest$state_future_from), 0L)

  project <- create_stan_params(
    list(gp("project")), states_supported = "R", seeding_time = 7
  )
  expect_equal(as.integer(project$state_future_fixed), 0L)

  # "estimate" fixes the state a seeding time before the end of the data
  estimate <- create_stan_params(
    list(gp("estimate")), states_supported = "R", seeding_time = 7
  )
  expect_equal(as.integer(estimate$state_future_fixed), 1L)
  expect_equal(as.integer(estimate$state_future_from), -7L)

  fixed_from <- create_stan_params(
    list(gp(-3L)), states_supported = "R", seeding_time = 7
  )
  expect_equal(as.integer(fixed_from$state_future_from), -3L)
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
  expect_identical(out$n_rw_components, 1L)
})

test_that("create_stan_params emits init-anchor state data (centred + Jacobian)", {
  params <- list(
    make_param("fraction_observed", RW(init = Normal(0.4, 0.05)),
      lower_bound = 0
    )
  )
  out <- create_stan_params(params, states_supported = "fraction_observed")
  expect_identical(out$state_anchor, array(1L))
  # the init prior is the level parameter's own prior (normal(0.4, 0.05))
  expect_identical(as.integer(out$prior_dist[1]), 2L)
  expect_equal(as.numeric(out$prior_dist_params[1:2]), c(0.4, 0.05))
  # the level's prior is applied to the derived init instead of the level
  expect_identical(as.integer(out$params_prior_skip), c(1L, 0L))
})

test_that("mean-anchor states keep their prior on the level", {
  params <- list(
    make_param("fraction_observed", RW(mean = Normal(0.4, 0.05)),
      lower_bound = 0
    )
  )
  out <- create_stan_params(params, states_supported = "fraction_observed")
  expect_identical(out$state_anchor, array(0L))
  # mean-anchored: the level keeps its prior, step sd is applied too
  expect_identical(as.integer(out$params_prior_skip), c(0L, 0L))
})

test_that("GP init anchor emits non-stationary state data", {
  params <- list(
    make_param("fraction_observed", GP(init = Normal(0.4, 0.05)),
      lower_bound = 0
    )
  )
  out <- create_stan_params(params, states_supported = "fraction_observed")
  expect_identical(out$comp_type, array(1L)) # gp
  expect_identical(out$state_anchor, array(1L)) # init
  # the init prior is the level parameter's own prior (normal(0.4, 0.05))
  expect_identical(as.integer(out$prior_dist[1]), 2L)
  # level is scaffolding (skipped); magnitude and lengthscale are applied
  expect_identical(as.integer(out$params_prior_skip), c(1L, 0L, 0L))
})

test_that("create_stan_params errors for fixed state priors", {
  # a fixed init-anchor prior has no variable level to attach it to
  expect_error(
    create_stan_params(
      list(make_param("R", GP(init = Fixed(0.4)), lower_bound = 0)),
      states_supported = "R"
    ),
    "init prior.*cannot be a fixed distribution"
  )
  # a fixed hyperparameter has no slot in the variable parameter vector
  expect_error(
    create_stan_params(
      list(make_param(
        "fraction_observed", RW(mean = Normal(0.5, 0.1), sd = Fixed(0.1)),
        lower_bound = 0
      )),
      states_supported = "fraction_observed"
    ),
    "step sd prior.*cannot be a fixed distribution"
  )
})

test_that("create_stan_params is a no-op without states", {
  params <- list(
    make_param("fraction_observed", Normal(0.5, 0.1), lower_bound = 0)
  )
  out <- create_stan_params(params, states_supported = "fraction_observed")
  expect_identical(out$n_states, 0L)
  expect_identical(out$n_rw_components, 0L)
  expect_identical(out$n_gp_components, 0L)
})

test_that("plot.state_spec returns a ggplot of prior draws", {
  for (spec in list(
    GP(init = LogNormal(mean = 1, sd = 0.5)),
    GP(mean = LogNormal(mean = 1, sd = 0.5)),
    RW(init = LogNormal(mean = 1, sd = 0.5))
  )) {
    p <- plot(spec, n = 20, samples = 10)
    expect_s3_class(p, "ggplot")
    expect_identical(length(unique(p$data$sample)), 10L)
    expect_identical(length(unique(p$data$time)), 20L)
    expect_true(all(p$data$value > 0))
  }
})

test_that("plot.state_spec errors for a known trajectory", {
  expect_error(plot(GP(mean = c(1, 2, 3))), "known")
})
