# define some dates for use in tests
dates <- seq(as.Date("2020-03-15"), by = "days", length.out = 15)

test_that("create_obs_model works with default settings", {
  obs <- create_obs_model(dates = dates)
  expect_equal(length(obs), 7)
  expect_equal(names(obs), c(
    "model_type", "week_effect", "obs_weight", "obs_scale",
    "likelihood", "return_likelihood", "day_of_week"
  ))
  expect_equal(obs$model_type, 1)
  expect_equal(obs$week_effect, 7)
  expect_equal(obs$obs_scale, 0)
  expect_equal(obs$likelihood, 1)
  expect_equal(obs$return_likelihood, 0)
  expect_equal(obs$day_of_week, c(7, 1, 2, 3, 4, 5, 6, 7, 1, 2, 3, 4, 5, 6, 7))
})

test_that("create_obs_model can be used with a Poisson model", {
  obs <- create_obs_model(dates = dates, obs = obs_opts(family = "poisson"))
  expect_equal(obs$model_type, 0)
})

test_that("create_obs_model can be used with no week effect", {
  obs <- create_obs_model(dates = dates, obs = obs_opts(week_effect = FALSE))
  expect_equal(obs$week_effect, 1)
  expect_equal(obs$day_of_week, rep(1, 15))
})

test_that("create_obs_model can be used with a custom week length", {
  obs <- create_obs_model(dates = dates, obs = obs_opts(week_length = 3))
  expect_equal(obs$day_of_week, c(3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2))
})
