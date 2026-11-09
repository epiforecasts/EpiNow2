cases <- data.table::data.table(
  date = rep(as.Date("2020-01-01") + 0:1, 2),
  confirm = 1:4,
  region = rep(c("testland", "realland"), each = 2)
)

test_that("opts_list repeats the default options for each region", {
  opts <- opts_list(forecast_opts(horizon = 3), cases)
  expect_named(opts, c("testland", "realland"))
  expect_equal(opts$testland, forecast_opts(horizon = 3))
  expect_equal(opts$realland, forecast_opts(horizon = 3))
})

test_that("opts_list overrides the defaults for named regions", {
  opts <- opts_list(
    forecast_opts(horizon = 3), cases,
    realland = forecast_opts(horizon = 14)
  )
  expect_equal(opts$testland$horizon, 3)
  expect_equal(opts$realland$horizon, 14)
})

test_that("filter_opts picks region specific options when present", {
  opts <- opts_list(
    forecast_opts(horizon = 3), cases,
    realland = forecast_opts(horizon = 14)
  )
  expect_equal(EpiNow2:::filter_opts(opts, "realland")$horizon, 14)
  expect_equal(EpiNow2:::filter_opts(opts, "testland")$horizon, 3)
})

test_that("filter_opts returns shared options unchanged", {
  opts <- forecast_opts(horizon = 3)
  expect_identical(EpiNow2:::filter_opts(opts, "testland"), opts)
})
