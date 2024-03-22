test_that("default generation time options produce a warning", {
  expect_warning(generation_time_opts(), "1 day")
})

test_that("deprecated arguments are caught", {
  expect_deprecated(stan_opts(init_fit = "cumulative"))
})
