skip_on_cran()

# Setup test data
reported_cases <- EpiNow2::example_confirmed[1:30]

test_that("create_stan_data works with default settings", {
  data <- create_stan_data(
    reported_cases = reported_cases,
    seeding_time = 7L,
    rt = rt_opts(),
    gp = gp_opts(),
    obs = obs_opts(),
    horizon = 0
  )

  expect_type(data, "list")
  expect_true("t" %in% names(data))
  expect_true("obs" %in% names(data))
  expect_equal(data$t, nrow(reported_cases))
})

test_that("create_stan_data handles Poisson observation model correctly", {
  data <- create_stan_data(
    reported_cases = reported_cases,
    seeding_time = 7L,
    rt = rt_opts(),
    gp = gp_opts(),
    obs = obs_opts(family = "poisson"),
    horizon = 0
  )

  expect_equal(data$model_type, 0)  # Poisson model type
  expect_type(data, "list")
})

test_that("create_stan_data handles Negative Binomial observation model correctly", {
  data <- create_stan_data(
    reported_cases = reported_cases,
    seeding_time = 7L,
    rt = rt_opts(),
    gp = gp_opts(),
    obs = obs_opts(family = "negbinomial"),
    horizon = 0
  )

  expect_equal(data$model_type, 1)  # Negative Binomial model type
  expect_type(data, "list")
})

test_that("create_stan_data handles backcalculation (rt = NULL)", {
  data <- create_stan_data(
    reported_cases = reported_cases,
    seeding_time = 7L,
    rt = NULL,
    gp = NULL,
    obs = obs_opts(),
    horizon = 0
  )

  expect_equal(data$estimate_r, 0)  # Should not estimate R
  expect_type(data, "list")
})

test_that("create_stan_data handles fixed Rt (gp = NULL)", {
  data <- create_stan_data(
    reported_cases = reported_cases,
    seeding_time = 7L,
    rt = rt_opts(),
    gp = NULL,
    obs = obs_opts(),
    horizon = 0
  )

  expect_equal(data$fixed, 1)  # Fixed Rt
  expect_type(data, "list")
})

test_that("create_stan_data handles Matern 5/2 kernel", {
  data <- create_stan_data(
    reported_cases = reported_cases,
    seeding_time = 7L,
    rt = rt_opts(),
    gp = gp_opts(kernel = "matern", matern_order = 5 / 2),
    obs = obs_opts(),
    horizon = 0
  )

  expect_type(data, "list")
  expect_true("gp_type" %in% names(data) || "kernel" %in% names(data))
})

test_that("create_stan_data handles periodic kernel", {
  data <- create_stan_data(
    reported_cases = reported_cases,
    seeding_time = 7L,
    rt = rt_opts(),
    gp = gp_opts(kernel = "periodic"),
    obs = obs_opts(),
    horizon = 0
  )

  expect_type(data, "list")
  expect_true("gp_type" %in% names(data) || "kernel" %in% names(data))
})

test_that("create_stan_data handles NA values in reported cases", {
  reported_cases_na <- data.table::copy(reported_cases)
  reported_cases_na[sample(1:30, 5), confirm := NA]

  data <- create_stan_data(
    reported_cases = reported_cases_na,
    seeding_time = 7L,
    rt = rt_opts(),
    gp = gp_opts(),
    obs = obs_opts(),
    horizon = 0
  )

  expect_type(data, "list")
  # Check that NAs are handled (either removed or marked)
  expect_true(all(is.finite(data$obs[data$obs_valid == 1])))
})

test_that("create_stan_data handles delays correctly", {
  data_with_delay <- create_stan_data(
    reported_cases = reported_cases,
    seeding_time = 7L,
    rt = rt_opts(),
    gp = gp_opts(),
    obs = obs_opts(),
    delays = delay_opts(example_incubation_period + example_reporting_delay),
    horizon = 0
  )

  data_no_delay <- create_stan_data(
    reported_cases = reported_cases,
    seeding_time = 7L,
    rt = rt_opts(),
    gp = gp_opts(),
    obs = obs_opts(),
    delays = delay_opts(),
    horizon = 0
  )

  expect_type(data_with_delay, "list")
  expect_type(data_no_delay, "list")
  # With delay should have delay parameters
  expect_true(data_with_delay$delay_n > 0 || data_with_delay$delay_type_max > 0)
  # Without delay should have no delay parameters
  expect_true(data_no_delay$delay_n == 0 || data_no_delay$delay_type_max == 0)
})

test_that("create_stan_data handles week effects", {
  data_week_effect <- create_stan_data(
    reported_cases = reported_cases,
    seeding_time = 7L,
    rt = rt_opts(),
    gp = gp_opts(),
    obs = obs_opts(week_effect = TRUE),
    horizon = 0
  )

  data_no_week_effect <- create_stan_data(
    reported_cases = reported_cases,
    seeding_time = 7L,
    rt = rt_opts(),
    gp = gp_opts(),
    obs = obs_opts(week_effect = FALSE),
    horizon = 0
  )

  expect_type(data_week_effect, "list")
  expect_type(data_no_week_effect, "list")
})

test_that("create_stan_data handles forecast horizon", {
  data_with_horizon <- create_stan_data(
    reported_cases = reported_cases,
    seeding_time = 7L,
    rt = rt_opts(),
    gp = gp_opts(),
    obs = obs_opts(),
    horizon = 7
  )

  expect_type(data_with_horizon, "list")
  expect_equal(data_with_horizon$horizon, 7)
  expect_true(data_with_horizon$t >= nrow(reported_cases))
})

test_that("create_stan_data validates input data", {
  # Test with invalid data
  expect_error(
    create_stan_data(
      reported_cases = data.frame(),  # Empty data
      seeding_time = 7L,
      rt = rt_opts(),
      gp = gp_opts(),
      obs = obs_opts(),
      horizon = 0
    )
  )
})
