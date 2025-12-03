# Test backcalc_opts function

test_that("backcalc_opts returns default settings", {
  result <- backcalc_opts()

  expect_s3_class(result, "backcalc_opts")
  expect_equal(result$prior, "reports")
  expect_equal(result$prior_window, 14)
  expect_equal(result$rt_window, 1L)
})

test_that("backcalc_opts accepts different prior types", {
  result_reports <- backcalc_opts(prior = "reports")
  expect_equal(result_reports$prior, "reports")

  result_none <- backcalc_opts(prior = "none")
  expect_equal(result_none$prior, "none")

  result_infections <- backcalc_opts(prior = "infections")
  expect_equal(result_infections$prior, "infections")
})

test_that("backcalc_opts accepts custom window sizes", {
  result <- backcalc_opts(prior_window = 7, rt_window = 3)

  expect_equal(result$prior_window, 7)
  expect_equal(result$rt_window, 3L)
})

test_that("backcalc_opts errors on even rt_window", {
  expect_error(
    backcalc_opts(rt_window = 2),
    "must be odd"
  )

  expect_error(
    backcalc_opts(rt_window = 4),
    "must be odd"
  )
})

test_that("backcalc_opts accepts odd rt_window values", {
  expect_no_error(backcalc_opts(rt_window = 1))
  expect_no_error(backcalc_opts(rt_window = 3))
  expect_no_error(backcalc_opts(rt_window = 5))
})

test_that("backcalc_opts errors on invalid prior", {
  expect_error(backcalc_opts(prior = "invalid"))
})
