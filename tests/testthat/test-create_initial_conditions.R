test_that("create_initial_conditions seeds R_mean within the prior support", {
  base <- list(
    delay_n_p = 0, delay_np_est_length = 0, fixed = 1, estimate_r = 1,
    bp_n = 0, week_effect = 0, n_params_variable = 1L,
    params_lower = array(0), params_upper = array(10)
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
    params_lower = array(0), params_upper = array(10)
  )
  params <- list(
    make_param("alpha", Normal(mean = 0, sd = 0.5, max = 10), lower_bound = 0)
  )
  stan_data <- c(base, make_init_priors(list()))
  init <- create_initial_conditions(stan_data, params)
  expect_length(init()$R_mean, 0)
})

test_that("create_initial_conditions seeds eta[1] on the spectral-density scale", {
  base <- list(
    delay_n_p = 0, delay_np_est_length = 0, fixed = 0, estimate_r = 0,
    bp_n = 0, week_effect = 0, n_params_variable = 0L,
    params_lower = array(numeric(0)), params_upper = array(numeric(0)),
    gp_type = 0, M = 4
  )
  stan_data <- c(base, make_init_priors(list()))
  init <- create_initial_conditions(stan_data, params = list())
  eta <- init()$eta
  expect_length(eta, 4)
  # eta[1] is centred (see gaussian_process.stan) so it is seeded at its
  # prior mean rather than with the same spread as the other elements.
  expect_identical(eta[1], 0)
})

test_that("create_initial_conditions seeds both fundamental-frequency eta for the periodic kernel", {
  base <- list(
    delay_n_p = 0, delay_np_est_length = 0, fixed = 0, estimate_r = 0,
    bp_n = 0, week_effect = 0, n_params_variable = 0L,
    params_lower = array(numeric(0)), params_upper = array(numeric(0)),
    gp_type = 1, M = 4
  )
  stan_data <- c(base, make_init_priors(list()))
  init <- create_initial_conditions(stan_data, params = list())
  eta <- init()$eta
  expect_length(eta, 8)
  # eta[1] and eta[M + 1] share the fundamental frequency (see
  # gaussian_process.stan) so both are seeded at their prior mean.
  expect_identical(eta[1], 0)
  expect_identical(eta[5], 0)
})

test_that("create_initial_conditions gives an empty params when none are variable", {
  base <- list(
    delay_n_p = 0, delay_np_est_length = 0, fixed = 1, estimate_r = 0,
    bp_n = 0, week_effect = 0, n_params_variable = 0L,
    params_lower = array(numeric(0)), params_upper = array(numeric(0))
  )
  stan_data <- c(base, make_init_priors(list()))
  init <- create_initial_conditions(stan_data, params = list())
  expect_length(init()$params, 0)
})
