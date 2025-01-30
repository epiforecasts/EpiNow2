test_that("forecast_opts returns correct default values", {
  forecast <- forecast_opts()
  expect_equal(forecast$horizon, 7)
  expect_null(forecast$accumulate)
})

test_that("forecast_opts sets infer_accumulate to FALSE if accumulate is given", {
  forecast <- forecast_opts(accumulate = 7)
  expect_equal(forecast$accumulate, 7)
})
