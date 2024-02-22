test_that("deprecated arguments are caught", {
  expect_deprecated(stan_opts())
})