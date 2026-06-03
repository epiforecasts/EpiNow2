skip_on_cran()
skip_on_os("windows")

# Test update_Rt
test_that("update_Rt returns R0 everywhere when GP noise is zero", {
  expect_equal(
    update_Rt(10, 1.2, rep(0, 9), integer(0), numeric(0), 0, 10),
    rep(1.2, 10)
  )
})

test_that("update_Rt with non-stationary GP applies centred cumulative noise", {
  noise <- rep(0.1, 9)
  n_centre <- 10
  gp <- cumsum(c(0, noise))
  expected <- 1.2 * exp(gp - mean(gp[1:n_centre]))
  expect_equal(
    update_Rt(10, 1.2, noise, integer(0), numeric(0), 0, n_centre),
    expected
  )
})

test_that("update_Rt with stationary GP returns R0 * exp(noise) with no centring", {
  noise <- rep(0.1, 10)
  expected <- 1.2 * exp(noise)
  expect_equal(
    update_Rt(10, 1.2, noise, integer(0), numeric(0), 1, 10),
    expected
  )
})

test_that("update_Rt centring sets the mean of log Rt over the centring window to log R0", {
  noise <- runif(9, -0.05, 0.05)
  result <- update_Rt(10, 1.2, noise, integer(0), numeric(0), 0, 10)
  expect_equal(mean(log(result[1:10])), log(1.2))
})

test_that("update_Rt is invariant in the centring window when t is extended", {
  noise <- runif(14, -0.05, 0.05)
  n_centre <- 10
  fit_short <- update_Rt(10, 1.2, noise[1:9], integer(0), numeric(0), 0, n_centre)
  fit_long  <- update_Rt(15, 1.2, noise,     integer(0), numeric(0), 0, n_centre)
  expect_equal(fit_long[1:n_centre], fit_short)
})

test_that("update_Rt returns R0 + breakpoint multipliers when noise is empty", {
  expect_equal(
    update_Rt(5, 1.2, numeric(0), c(1, 1, 2, 2, 2), 0.1, 0, 5),
    c(1.2, 1.2, 1.2 * exp(rep(0.1, 3)))
  )
  expect_equal(
    update_Rt(5, 1.2, numeric(0), c(1, 1, 2, 2, 2), 0.1, 1, 5),
    c(1.2, 1.2, 1.2 * exp(rep(0.1, 3)))
  )
  expect_equal(
    update_Rt(5, 1.2, numeric(0), c(1, 2, 3, 3, 3), rep(0.1, 2), 0, 5),
    c(1.2, 1.2 * exp(0.1), rep(1.2 * exp(0.2), 3))
  )
})

test_that("update_Rt with non-stationary GP and breakpoint adds centred GP + step", {
  noise <- rep(0.1, 4)
  bps <- c(1, 1, 2, 2, 2)
  bp_effects <- 0.1
  gp <- cumsum(c(0, noise))
  bp0 <- c(0, cumsum(bp_effects))
  expected <- 1.2 * exp(gp - mean(gp[1:5]) + bp0[bps])
  expect_equal(
    update_Rt(5, 1.2, noise, bps, bp_effects, 0, 5),
    expected
  )
})

# Helper function for R_to_r tests
# Calculates negative moment generating function for verification.
neg_MGF <- function(r, pmf) {
  n <- length(pmf)
  sum(pmf * exp(-r * (0:(n - 1))))
}

test_that("R_to_r_newton_step calculates correct Newton step", {
  pmf <- discretised_pmf(c(4, 2), 10, 2, 0)
  step <- R_to_r_newton_step(1.5, 0.1, pmf)
  expect_type(step, "double")
  expect_length(step, 1)
  expect_true(is.finite(step))
})

test_that("R_to_r correctly handles R = 1", {
  pmf <- discretised_pmf(c(4, 2), 10, 2, 0)
  gt_rev_pmf <- rev(pmf)
  r <- R_to_r(1.0, gt_rev_pmf, 1e-6)
  expect_equal(r, 0.0, tolerance = 1e-5)
})

test_that("R_to_r gives positive r for R > 1", {
  pmf <- discretised_pmf(c(4, 2), 10, 2, 0)
  gt_rev_pmf <- rev(pmf)
  r <- R_to_r(1.5, gt_rev_pmf, 1e-6)
  expect_gt(r, 0)
})

test_that("R_to_r gives negative r for R < 1", {
  pmf <- discretised_pmf(c(4, 2), 10, 2, 0)
  gt_rev_pmf <- rev(pmf)
  r <- R_to_r(0.8, gt_rev_pmf, 1e-6)
  expect_lt(r, 0)
})

test_that("R_to_r round trip is consistent", {
  test_Rs <- c(0.5, 0.8, 1.0, 1.2, 1.5, 2.0)
  test_pmfs <- list(
    short = discretised_pmf(c(2, 1), 8, 2, 0),
    medium = discretised_pmf(c(4, 2), 10, 2, 0),
    long = discretised_pmf(c(6, 3), 15, 2, 0)
  )
  for (pmf in test_pmfs) {
    gt_rev_pmf <- rev(pmf)
    for (R_test in test_Rs) {
      r <- R_to_r(R_test, gt_rev_pmf, 1e-6)
      R_recovered <- 1 / neg_MGF(r, pmf)
      expect_equal(R_recovered, R_test, tolerance = 1e-5)
    }
  }
})

test_that("R_to_r works with different generation time distributions", {
  pmf_short <- discretised_pmf(c(2, 1), 8, 2, 0)
  gt_rev_pmf_short <- rev(pmf_short)
  r_short <- R_to_r(1.5, gt_rev_pmf_short, 1e-6)
  expect_true(is.finite(r_short))
  pmf_long <- discretised_pmf(c(6, 3), 15, 2, 0)
  gt_rev_pmf_long <- rev(pmf_long)
  r_long <- R_to_r(1.5, gt_rev_pmf_long, 1e-6)
  expect_true(is.finite(r_long))
  expect_gt(r_short, r_long)
})

test_that("R_to_r respects tolerance parameter", {
  pmf <- discretised_pmf(c(4, 2), 10, 2, 0)
  gt_rev_pmf <- rev(pmf)
  R_true <- 1.5
  r_tight <- R_to_r(R_true, gt_rev_pmf, 1e-8)
  r_loose <- R_to_r(R_true, gt_rev_pmf, 1e-4)
  R_recovered_tight <- 1 / neg_MGF(r_tight, pmf)
  R_recovered_loose <- 1 / neg_MGF(r_loose, pmf)
  error_tight <- abs(R_recovered_tight - R_true)
  error_loose <- abs(R_recovered_loose - R_true)
  expect_lt(error_tight, error_loose)
})
