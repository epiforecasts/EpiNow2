skip_on_cran()
skip_on_os("windows")

test_that("calculate_growth works as expected", {
  skip_on_cran()
  expect_equal(calculate_growth(rep(1, 5), 1), rep(0, 4))
  expect_equal(round(calculate_growth(1:5, 2), 2), c(0.41, 0.29, 0.22))
  expect_equal(round(calculate_growth(exp(0.4*1:5), 2), 2), rep(0.4, 3))
  expect_error(calculate_growth(1:5, 6))
  expect_error(calculate_growth(1:5, 0))
})
