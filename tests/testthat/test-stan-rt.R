skip_on_cran()
skip_on_os("windows")

# Test update_Rt
test_that("update_Rt works to produce multiple Rt estimates with a static gaussian process", {
  expect_equal(
    update_Rt(10, 1.2, rep(0, 9), rep(10, 0), numeric(0), 0),
    rep(1.2, 10)
  )
})
test_that("update_Rt works to produce multiple Rt estimates with a non-static gaussian process", {
  expect_equal(
    round(update_Rt(10, 1.2, rep(0.1, 9), rep(10, 0), numeric(0), 0), 2),
    c(1.20, 1.33, 1.47, 1.62, 1.79, 1.98, 2.19, 2.42, 2.67, 2.95)
  )
})
test_that("update_Rt works to produce multiple Rt estimates with a non-static stationary gaussian process", {
  expect_equal(
    round(update_Rt(10, 1.2, rep(0.1, 10), rep(10, 0), numeric(0), 1), 3),
    c(1.326, 1.326, 1.326, 1.326, 1.326, 1.326, 1.326, 1.326, 1.326, 1.326)
  )
})
test_that("update_Rt works when Rt is fixed", {
  expect_equal(
    round(update_Rt(10, 1.2, numeric(0), rep(10, 0), numeric(0), 0), 2),
    rep(1.2, 10)
  )
  expect_equal(
    round(update_Rt(10, 1.2, numeric(0), rep(10, 0), numeric(0), 1), 2),
    rep(1.2, 10)
  )
})
test_that("update_Rt works when Rt is fixed but a breakpoint is present", {
  expect_equal(
    round(update_Rt(5, 1.2, numeric(0), c(1, 1, 2, 2, 2), 0.1, 0), 2),
    c(1.2, 1.2, rep(1.33, 3))
  )
  expect_equal(
    round(update_Rt(5, 1.2, numeric(0), c(1, 1, 2, 2, 2), 0.1, 1), 2),
    c(1.2, 1.2, rep(1.33, 3))
  )
  expect_equal(
    round(update_Rt(5, 1.2, numeric(0), c(1, 2, 3, 3, 3), rep(0.1, 2), 0), 2),
    c(1.2, 1.33, rep(1.47, 3))
  )
})
test_that("update_Rt works when Rt is variable and a breakpoint is present", {
  expect_equal(
    round(update_Rt(5, 1.2, rep(0, 4), c(1, 1, 2, 2, 2), 0.1, 0), 2),
    c(1.2, 1.2, rep(1.33, 3))
  )
  expect_equal(
    round(update_Rt(5, 1.2, rep(0, 5), c(1, 1, 2, 2, 2), 0.1, 1), 2),
    c(1.2, 1.2, rep(1.33, 3))
  )
  expect_equal(
    round(update_Rt(5, 1.2, rep(0.1, 4), c(1, 1, 2, 2, 2), 0.1, 0), 2),
    c(1.20, 1.33, 1.62, 1.79, 1.98)
  )
})
