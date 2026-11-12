test_that("backcalc_opts works as expected with default arguments", {
  opts <- backcalc_opts()
  expect_s3_class(opts, "backcalc_opts")
  expect_identical(opts$prior, "reports")
  expect_identical(opts$prior_window, 14)
  expect_identical(opts$gp, GP())
})

test_that("backcalc_opts errors for bad 'gp' specifications", {
  expect_error(backcalc_opts(gp = GP(mean = Normal(0, 1))), "baseline")
  expect_error(backcalc_opts(gp = LogNormal(1, 1)))
  expect_error(backcalc_opts(prior = "bad"))
  expect_error(backcalc_opts(rt_window = 2), "odd")
})

test_that("backcalc_opts accepts no deviation", {
  expect_null(backcalc_opts(gp = NULL)$gp)
})
