test_that("create_initial_conditions gives an empty params when none are variable", {
  base <- list(
    delay_n_p = 0, delay_np_est_length = 0, fixed = 1, estimate_r = 0,
    bp_n = 0, week_effect = 0, n_params_variable = 0L,
    params_lower = array(numeric(0)), params_upper = array(numeric(0))
  )
  stan_data <- base
  init <- create_initial_conditions(stan_data, params = list())
  expect_length(init()$params, 0)
})

test_that("create_initial_conditions handles data without infection model fields", {
  base <- list(
    delay_n_p = 0, delay_np_est_length = 0, n_params_variable = 1L,
    params_lower = array(0), params_upper = array(10)
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
  expect_length(without$initial_infections, 0)
  expect_length(without$state_gp_eta, 0)
  expect_length(without$state_rw_steps, 0)
  expect_null(without$day_of_week_simplex)
})
