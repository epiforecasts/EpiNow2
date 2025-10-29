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

# Helper function for R_to_r tests
# Calculates negative moment generating function for verification.
neg_MGF <- function(r, pmf) {
  n <- length(pmf)
  sum(pmf * exp(-r * (0:(n - 1))))
}

# Test R_to_r_newton_step
test_that("R_to_r_newton_step calculates correct Newton step", {
  pmf <- discretised_pmf(c(4, 2), 10, 1)
  step <- R_to_r_newton_step(1.5, 0.1, pmf)
  expect_type(step, "double")
  expect_length(step, 1)
  expect_true(is.finite(step))
})

# Test R_to_r basic correctness
test_that("R_to_r correctly handles R = 1", {
  pmf <- discretised_pmf(c(4, 2), 10, 1)
  gt_rev_pmf <- rev(pmf)
  r <- R_to_r(1.0, gt_rev_pmf, 1e-6)
  expect_equal(r, 0.0, tolerance = 1e-5)
})

test_that("R_to_r gives positive r for R > 1", {
  pmf <- discretised_pmf(c(4, 2), 10, 1)
  gt_rev_pmf <- rev(pmf)
  r <- R_to_r(1.5, gt_rev_pmf, 1e-6)
  expect_gt(r, 0)
})

test_that("R_to_r gives negative r for R < 1", {
  pmf <- discretised_pmf(c(4, 2), 10, 1)
  gt_rev_pmf <- rev(pmf)
  r <- R_to_r(0.8, gt_rev_pmf, 1e-6)
  expect_lt(r, 0)
})

# Test round trip consistency
test_that("R_to_r round trip is consistent", {
  pmf <- discretised_pmf(c(4, 2), 10, 1)
  gt_rev_pmf <- rev(pmf)
  test_Rs <- c(0.5, 0.8, 1.0, 1.2, 1.5, 2.0)
  for (R_test in test_Rs) {
    r <- R_to_r(R_test, gt_rev_pmf, 1e-6)
    R_recovered <- 1 / neg_MGF(r, pmf)
    expect_equal(R_recovered, R_test, tolerance = 1e-5)
  }
})

# Test with different generation time distributions
test_that("R_to_r works with different generation time distributions", {
  pmf_short <- discretised_pmf(c(2, 1), 8, 1)
  gt_rev_pmf_short <- rev(pmf_short)
  r_short <- R_to_r(1.5, gt_rev_pmf_short, 1e-6)
  expect_true(is.finite(r_short))
  pmf_long <- discretised_pmf(c(6, 3), 15, 1)
  gt_rev_pmf_long <- rev(pmf_long)
  r_long <- R_to_r(1.5, gt_rev_pmf_long, 1e-6)
  expect_true(is.finite(r_long))
  expect_gt(r_short, r_long)
})

# Test tolerance parameter
test_that("R_to_r respects tolerance parameter", {
  pmf <- discretised_pmf(c(4, 2), 10, 1)
  gt_rev_pmf <- rev(pmf)
  r_tight <- R_to_r(1.5, gt_rev_pmf, 1e-8)
  r_loose <- R_to_r(1.5, gt_rev_pmf, 1e-4)
  expect_equal(r_tight, r_loose, tolerance = 1e-3)
})
