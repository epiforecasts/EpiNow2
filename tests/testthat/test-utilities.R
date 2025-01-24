test_that("setup_forecast works as expected when forecast = NULL but horizon is set", {
  horizon <- 7
  forecast <- setup_forecast(forecast = NULL, horizon = horizon)
  expect_identical(forecast$horizon, horizon)
})

test_that("setup_forecast uses horizon if specified", {
  horizon <- 7
  forecast <- setup_forecast(forecast = forecast_opts(horizon = 14), horizon = horizon)
  expect_identical(forecast$horizon, horizon)
})

test_that("setup_forecast sets horizon to 0 if forecast = NULL and horizon not set", {
  forecast <- setup_forecast(forecast = NULL)
  expect_identical(forecast$horizon, 0)
})
