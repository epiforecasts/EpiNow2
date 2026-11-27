test_that("create_initial_conditions seeds R_mean within the prior support", {
  base <- list(
    delay_n_p = 0, delay_np_est_length = 0, fixed = 1, estimate_r = 1,
    bp_n = 0, week_effect = 0, n_params_variable = 1L,
    params_lower = array(0), params_upper = array(10),
    params_loc = array(0), params_scale = array(1)
  )
  params <- list(
    make_param("alpha", Normal(mean = 0, sd = 0.5, max = 10), lower_bound = 0)
  )
  priors <- list(
    lognormal = LogNormal(mean = 2, sd = 1),
    gamma = Gamma(mean = 2, sd = 1),
    normal = Normal(mean = 2, sd = 1, max = 10)
  )
  for (prior in priors) {
    stan_data <- c(base, make_init_priors(
      list(list(param_id = 1L, dist = prior, lower_bound = 0))
    ))
    init <- create_initial_conditions(stan_data, params)
    draws <- vapply(seq_len(200), function(i) init()$R_mean, numeric(1))
    expect_true(all(draws >= 0 & draws <= max(prior)))
    expect_gt(mean(draws), 0)
  }
})

test_that("create_initial_conditions gives an empty R_mean when Rt is not estimated", {
  base <- list(
    delay_n_p = 0, delay_np_est_length = 0, fixed = 1, estimate_r = 0,
    bp_n = 0, week_effect = 0, n_params_variable = 1L,
    params_lower = array(0), params_upper = array(10),
    params_loc = array(0), params_scale = array(1)
  )
  params <- list(
    make_param("alpha", Normal(mean = 0, sd = 0.5, max = 10), lower_bound = 0)
  )
  stan_data <- c(base, make_init_priors(list()))
  init <- create_initial_conditions(stan_data, params)
  expect_length(init()$R_mean, 0)
})

test_that("create_initial_conditions gives an empty params_raw when none are variable", {
  base <- list(
    delay_n_p = 0, delay_np_est_length = 0, fixed = 1, estimate_r = 0,
    bp_n = 0, week_effect = 0, n_params_variable = 0L,
    params_lower = array(numeric(0)), params_upper = array(numeric(0))
  )
  stan_data <- c(base, make_init_priors(list()))
  init <- create_initial_conditions(stan_data, params = list())
  expect_length(init()$params_raw, 0)
})

test_that("create_initial_conditions handles data without infection model fields", {
  base <- list(
    delay_n_p = 0, delay_np_est_length = 0, n_params_variable = 1L,
    params_lower = array(0), params_upper = array(10),
    params_loc = array(0), params_scale = array(1)
  )
  params <- list(
    make_param("alpha", Normal(mean = 0, sd = 0.5, max = 10), lower_bound = 0)
  )
  dummies <- list(estimate_r = 0, fixed = 1, bp_n = 0, week_effect = 0)
  set.seed(1)
  without <- create_initial_conditions(base, params)()
  set.seed(1)
  with_dummies <- create_initial_conditions(c(base, dummies), params)()
  expect_identical(without, with_dummies)
  expect_length(without$eta, 0)
  expect_length(without$R_mean, 0)
  expect_length(without$bp_effects, 0)
  expect_null(without$day_of_week_simplex)
})

test_that("create_initial_conditions draws bounded parameters on their scale", {
  params <- list(
    make_param("lower", Normal(mean = 1, sd = 1000), lower_bound = 0),
    make_param("upper", Normal(mean = 9, sd = 1000, max = 10),
               lower_bound = -Inf),
    make_param("both", Normal(mean = 5, sd = 10, max = 10),
               lower_bound = 0),
    make_param("free", Normal(mean = 2, sd = 3), lower_bound = -Inf),
    make_param("fixed", Fixed(1)),
    make_param("missing", NULL)
  )
  stan_data <- c(
    list(delay_n_p = 0, delay_np_est_length = 0), create_stan_params(params)
  )
  init <- create_initial_conditions(stan_data, params)
  set.seed(123)
  raw <- replicate(1000, as.vector(init()$params_raw))
  link <- sweep(raw, 1, stan_data$params_scale, `*`)
  link <- sweep(link, 1, stan_data$params_loc, `+`)
  draws <- rbind(exp(link[1, ]), 10 - exp(link[2, ]),
                 10 * plogis(link[3, ]), link[4, ])
  expect_true(all(is.finite(draws)))
  expect_true(all(draws[1, ] > 0))
  expect_true(all(draws[2, ] < 10))
  expect_true(all(draws[3, ] > 0 & draws[3, ] < 10))
  expect_lt(mean(draws[1, ]), 1000)
  expect_gt(mean(draws[2, ]), -990)
  expect_equal(mean(draws[4, ]), 2, tolerance = 0.2)
})
