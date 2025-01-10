test_that("create_forecast_data returns expected default values", {
  result <- create_forecast_data(data = example_confirmed)

  expect_type(result, "list")
  expect_equal(result$horizon, 7)
  expect_equal(result$future_accumulate, 1)
})

test_that("create_rt_data identifies gaps correctly", {
  reported_weekly <- suppressWarnings(fill_missing(
    example_confirmed[seq(1, 60, by = 7)],
    missing_dates = "accumulate"
  ))
  expect_message(
    result <- create_forecast_data(data = reported_weekly),
    "same as accumulation used in the likelihood"
  )
  expect_equal(result$future_accumulate, 7)
})

test_that("create_rt_data doesn't try to identify non-equally spaced gaps", {
  reported_irregular <- example_confirmed[c(seq(1, 43, by = 7), 45)]
  expect_no_message(
    result <- create_forecast_data(data = reported_irregular)
  )
  expect_equal(result$future_accumulate, 1)
})
