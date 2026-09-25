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

test_that("update_Rt produces expected output for centred breakpoints with empty noise", {
  bps1 <- c(1, 1, 2, 2, 2)
  bp_effects1 <- 0.1
  bp0_1 <- c(0, cumsum(bp_effects1))
  bp_path1 <- bp0_1[bps1]
  expected1 <- 1.2 * exp(bp_path1 - mean(bp_path1[1:5]))
  expect_equal(
    update_Rt(5, 1.2, numeric(0), bps1, bp_effects1, 0, 5),
    expected1
  )
  expect_equal(
    update_Rt(5, 1.2, numeric(0), bps1, bp_effects1, 1, 5),
    expected1
  )

  bps2 <- c(1, 2, 3, 3, 3)
  bp_effects2 <- rep(0.1, 2)
  bp0_2 <- c(0, cumsum(bp_effects2))
  bp_path2 <- bp0_2[bps2]
  expected2 <- 1.2 * exp(bp_path2 - mean(bp_path2[1:5]))
  expect_equal(
    update_Rt(5, 1.2, numeric(0), bps2, bp_effects2, 0, 5),
    expected2
  )
})

test_that("update_Rt correctly handles centred non-stationary GP and breakpoint effects", {
  noise <- rep(0.1, 4)
  bps <- c(1, 1, 2, 2, 2)
  bp_effects <- 0.1
  gp <- cumsum(c(0, noise))
  bp0 <- c(0, cumsum(bp_effects))
  bp_path <- bp0[bps]
  expected <- 1.2 * exp(
    gp - mean(gp[1:5]) + bp_path - mean(bp_path[1:5])
  )
  expect_equal(
    update_Rt(5, 1.2, noise, bps, bp_effects, 0, 5),
    expected
  )
})

# Reference implementation of update_Rt that builds the centred paths
# explicitly
update_Rt_ref <- function(t, R0, noise, bps, bp_effects, stationary,
                          n_centre) {
  logR <- rep(log(R0), t)
  if (length(bp_effects) > 0) {
    bp <- c(0, cumsum(bp_effects))[bps]
    logR <- logR + bp - mean(bp[1:n_centre])
  }
  gp_n <- length(noise)
  if (gp_n > 0) {
    gp <- rep(0, t)
    if (stationary) {
      gp[1:gp_n] <- noise
      if (t > gp_n) gp[(gp_n + 1):t] <- noise[gp_n]
    } else {
      gp[2:(gp_n + 1)] <- noise
      gp <- cumsum(gp)
      gp <- gp - mean(gp[1:n_centre])
    }
    logR <- logR + gp
  }
  exp(logR)
}

test_that("update_Rt matches the reference across edge cases", {
  set.seed(123)
  weekly <- floor((1:20) / 7) + 1
  cases <- list(
    # non-stationary GP shorter than t - 1, so it holds forward
    list(noise = rnorm(12, 0, 0.1), bps = integer(0), stationary = 0,
         n_centre = 15),
    # centring window shorter than the GP
    list(noise = rnorm(19, 0, 0.1), bps = integer(0), stationary = 0,
         n_centre = 5),
    # stationary GP covering all time points
    list(noise = rnorm(20, 0, 0.1), bps = integer(0), stationary = 1,
         n_centre = 20),
    # stationary GP with breakpoints and hold forward
    list(noise = rnorm(14, 0, 0.1), bps = weekly, stationary = 1,
         n_centre = 14),
    # breakpoints not starting at 1, with non-unit and backward jumps
    list(noise = numeric(0), bps = c(2, 2, 4, 4, 1, 1, 3, 3, 3, 3,
                                     4, 4, 2, 2, 2, 4, 4, 4, 4, 4),
         stationary = 0, n_centre = 15),
    list(noise = rnorm(19, 0, 0.1), bps = c(2, 2, 4, 4, 1, 1, 3, 3, 3, 3,
                                            4, 4, 2, 2, 2, 4, 4, 4, 4, 4),
         stationary = 0, n_centre = 15),
    # non-stationary GP plus breakpoints with hold forward
    list(noise = rnorm(12, 0, 0.1), bps = weekly, stationary = 0,
         n_centre = 13)
  )
  for (case in cases) {
    bp_effects <- if (length(case$bps)) {
      rnorm(max(case$bps) - 1, 0, 0.1)
    } else {
      numeric(0)
    }
    expect_equal(
      update_Rt(
        20, 1.3, case$noise, case$bps, bp_effects, case$stationary,
        case$n_centre
      ),
      update_Rt_ref(
        20, 1.3, case$noise, case$bps, bp_effects, case$stationary,
        case$n_centre
      ),
      tolerance = 1e-12
    )
  }
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
