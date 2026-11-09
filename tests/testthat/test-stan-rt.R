skip_on_cran()
skip_on_os("windows")

# Test update_Rt helpers
test_that("hold_forward repeats the last value up to length t", {
  expect_equal(hold_forward(c(1, 2, 3), 5), c(1, 2, 3, 3, 3))
  expect_equal(hold_forward(c(1, 2, 3), 3), c(1, 2, 3))
})

test_that("gp_log_path produces expected output", {
  noise <- c(0.1, 0.2)
  expect_equal(gp_log_path(noise, 5, 0), c(0, 0.1, 0.3, 0.3, 0.3))
  expect_equal(gp_log_path(noise, 4, 1), c(0.1, 0.2, 0.2, 0.2))
  expect_equal(gp_log_path(numeric(0), 3, 0), rep(0, 3))
  expect_equal(gp_log_path(numeric(0), 3, 1), rep(0, 3))
})

test_that("bp_log_levels starts at 0 and accumulates the effects", {
  expect_equal(bp_log_levels(c(0.1, -0.3)), c(0, 0.1, -0.2))
  expect_equal(bp_log_levels(numeric(0)), 0)
})

test_that("centred_log_intercept subtracts the window means", {
  gp <- c(0, 0.1, 0.3, 0.3)
  bp <- c(0, 0.2)
  bps <- c(1, 1, 2, 2)
  # Window of 3 days: GP mean 0.4 / 3, breakpoint mean 0.2 / 3
  expect_equal(centred_log_intercept(2, gp, bp, bps, 0, 3), log(2) - 0.2)
  # The stationary GP is not centred
  expect_equal(
    centred_log_intercept(2, gp, bp, bps, 1, 3), log(2) - 0.2 / 3
  )
  # A single level means no breakpoints
  expect_equal(
    centred_log_intercept(2, gp, 0, rep(1, 4), 0, 3), log(2) - 0.4 / 3
  )
})

# Test update_Rt
test_that("update_Rt returns R0 everywhere when GP noise is zero", {
  expect_equal(
    update_Rt(10, 1.2, rep(0, 9), integer(0), numeric(0), 0, 10),
    rep(1.2, 10)
  )
})

test_that("update_Rt sets the mean of log Rt over the centring window to log R0", {
  noise <- c(0.1, -0.05, 0.2, 0.03, -0.1, 0.07, 0.02, -0.04, 0.05)
  bps <- c(1, 1, 2, 2, 2, 3, 3, 1, 4, 4)
  bp_effects <- c(0.1, -0.2, 0.3)
  n_centre <- 7
  gp_only <- update_Rt(10, 1.2, noise, integer(0), numeric(0), 0, n_centre)
  bp_only <- update_Rt(10, 1.2, numeric(0), bps, bp_effects, 0, n_centre)
  both <- update_Rt(10, 1.2, noise, bps, bp_effects, 0, n_centre)
  for (R in list(gp_only, bp_only, both)) {
    expect_equal(mean(log(R[1:n_centre])), log(1.2))
  }
})

test_that("update_Rt with non-stationary GP has log Rt increments equal to noise", {
  noise <- c(0.1, -0.05, 0.2, 0.03, -0.1, 0.07, 0.02, -0.04, 0.05)
  R <- update_Rt(10, 1.2, noise, integer(0), numeric(0), 0, 8)
  expect_equal(diff(log(R)), noise)
})

test_that("update_Rt with stationary GP returns R0 * exp(noise) with no centring", {
  noise <- c(0.1, -0.05, 0.2, 0.03, -0.1, 0.07, 0.02, -0.04, 0.05, 0.01)
  R <- update_Rt(10, 1.2, noise, integer(0), numeric(0), 1, 8)
  expect_equal(log(R) - log(1.2), noise)
})

test_that("update_Rt holds the last GP value after the GP ends", {
  noise <- c(0.1, -0.05, 0.2, 0.03, -0.1)
  R_ns <- update_Rt(10, 1.2, noise, integer(0), numeric(0), 0, 8)
  expect_equal(R_ns[7:10], rep(R_ns[6], 4))
  R_st <- update_Rt(10, 1.2, noise, integer(0), numeric(0), 1, 8)
  expect_equal(R_st[6:10], rep(R_st[5], 5))
})

test_that("update_Rt with breakpoints jumps by the matching effects", {
  # Levels not starting at 1, with multi-level and backward jumps
  bps <- c(2, 2, 4, 4, 1, 1, 3, 3, 3, 3)
  bp_effects <- c(0.1, -0.2, 0.3)
  # Level 2 to 4 adds effects 2 and 3, 4 to 1 subtracts effects 1 to 3,
  # and 1 to 3 adds effects 1 and 2
  jumps <- c(0, 0.1, 0, -0.2, 0, -0.1, 0, 0, 0)
  R_bp <- update_Rt(10, 1.2, numeric(0), bps, bp_effects, 0, 8)
  expect_equal(diff(log(R_bp)), jumps)
  R_bp_st <- update_Rt(10, 1.2, numeric(0), bps, bp_effects, 1, 8)
  expect_equal(R_bp_st, R_bp)
  # With a non-stationary GP the jumps add to the GP increments, and only
  # the jumps remain once the GP has ended
  noise <- c(0.1, -0.05, 0.2, 0.03, -0.1, 0.07)
  R_both <- update_Rt(10, 1.2, noise, bps, bp_effects, 0, 8)
  expect_equal(diff(log(R_both)), jumps + c(noise, 0, 0, 0))
  # With a stationary GP log Rt is the breakpoint level plus the GP, with
  # the last GP value held
  R_st <- update_Rt(10, 1.2, noise, bps, bp_effects, 1, 8)
  expect_equal(log(R_st) - log(R_bp), c(noise, rep(noise[6], 4)))
})

test_that("update_Rt is invariant in the centring window when t is extended", {
  noise <- runif(14, -0.05, 0.05)
  n_centre <- 10
  fit_short <- update_Rt(10, 1.2, noise[1:9], integer(0), numeric(0), 0, n_centre)
  fit_long  <- update_Rt(15, 1.2, noise,     integer(0), numeric(0), 0, n_centre)
  expect_equal(fit_long[1:n_centre], fit_short)
})

# Helper function for R_to_r tests
# Calculates negative moment generating function for verification.
neg_MGF <- function(r, pmf) {
  n <- length(pmf)
  sum(pmf * exp(-r * (0:(n - 1))))
}

test_that("R_to_r_newton_step_stan in the reference gives a finite step", {
  pmf <- discretised_pmf(c(4, 2), 10, 2, 0)
  step <- R_to_r_newton_step_stan(1.5, 0.1, pmf)
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

# Cases for comparing the C++ R_to_r() with the pure Stan reference: R below,
# near and above one, a large R, generation times of length one and two, and
# short and long generation times.
rt_pmfs <- list(
  one = 1,
  one_day = c(1, 0),
  two = c(0.95, 0.05),
  short = rev(discretised_pmf(c(2, 1), 8, 2, 0)),
  medium = rev(discretised_pmf(c(4, 2), 15, 2, 0)),
  long = rev(discretised_pmf(c(4, 0.4), 30, 2, 0))
)
rt_cases <- expand.grid(
  R = c(0.5, 0.9, 1, 1.01, 1.5, 2, 3, 5),
  gt = names(rt_pmfs),
  abs_tol = c(1e-3, 1e-8),
  stringsAsFactors = FALSE
)

# Gradient of the root r of R sum_k p_k exp(-r k) = 1 from the implicit
# function theorem, with p the reverse of gt_rev_pmf.
R_to_r_ift <- function(r, R, gt_rev_pmf) {
  k <- rev(seq_along(gt_rev_pmf) - 1)
  e <- exp(-r * k)
  s0 <- sum(gt_rev_pmf * e)
  s1 <- sum(k * gt_rev_pmf * e)
  list(R = s0 / (R * s1), gt = e / s1)
}

test_that("R_to_r matches the pure Stan implementation", {
  for (i in seq_len(nrow(rt_cases))) {
    case <- rt_cases[i, ]
    gt <- rt_pmfs[[case$gt]]
    expect_equal(
      R_to_r(case$R, gt, case$abs_tol),
      R_to_r_stan(case$R, gt, case$abs_tol),
      tolerance = 1e-12
    )
  }
})

test_that("R_to_r is exact for a one-day generation time", {
  expect_equal(R_to_r(2, c(1, 0), 1e-8), log(2), tolerance = 1e-12)
})

test_that("R_to_r returns NaN when all generation time mass is at zero", {
  expect_true(is.nan(R_to_r(1.5, 1, 1e-3)))
  expect_true(is.nan(R_to_r(0.5, c(0, 1), 1e-3)))
})

# The Euler-Lotka equation R sum_k p_k exp(-r k) = 1 is solved by the growth
# rate r, with p the reverse of gt_rev_pmf. Its left-hand side minus one is
# decreasing in r.
euler_lotka <- function(r, R, gt_rev_pmf) {
  R * neg_MGF(r, rev(gt_rev_pmf)) - 1
}
# Generation times with mass away from zero, where r is defined
rt_cases_finite <- rt_cases[rt_cases$gt != "one", ]

test_that("R_to_r solves the Euler-Lotka equation to the tolerance", {
  for (i in seq_len(nrow(rt_cases_finite))) {
    case <- rt_cases_finite[i, ]
    gt <- rt_pmfs[[case$gt]]
    tol <- case$abs_tol
    r <- R_to_r(case$R, gt, tol)
    # The root lies within the tolerance of r
    expect_gte(euler_lotka(r - tol, case$R, gt), 0)
    expect_lte(euler_lotka(r + tol, case$R, gt), 0)
    if (tol == 1e-8) {
      expect_lt(abs(euler_lotka(r, case$R, gt)), 1e-8)
    }
  }
})

test_that("R_to_r gives r = 0 for R = 1 and the sign of R - 1 otherwise", {
  for (i in seq_len(nrow(rt_cases_finite))) {
    case <- rt_cases_finite[i, ]
    r <- R_to_r(case$R, rt_pmfs[[case$gt]], case$abs_tol)
    if (case$R == 1) {
      expect_equal(r, 0)
    } else {
      expect_identical(sign(r), sign(case$R - 1))
    }
  }
})

test_that("R_to_r gives r = log(R) for a one-day generation time", {
  Rs <- c(0.2, 0.5, 0.9, 1.01, 1.5, 2, 5)
  for (R in Rs) {
    expect_equal(R_to_r(R, c(1, 0), 1e-8), log(R), tolerance = 1e-12)
    expect_equal(R_to_r(R, c(0, 0, 1, 0), 1e-8), log(R), tolerance = 1e-12)
  }
})

test_that("R_to_r is increasing in R", {
  Rs <- seq(0.3, 5, by = 0.1)
  for (gt in rt_pmfs[names(rt_pmfs) != "one"]) {
    r <- vapply(Rs, R_to_r, numeric(1), gt_rev_pmf = gt, abs_tol = 1e-8)
    expect_true(all(diff(r) > 0))
  }
})

test_that("R_to_r gradients match the pure Stan implementation", {
  skip_if_not_installed("rstan")
  model <- stan_test_model("rt_gradient.stan")
  params <- list(c(R = 1, gt = 1), c(R = 1, gt = 0), c(R = 0, gt = 1))
  # Generation times with mass away from zero, where the gradient is finite
  cases <- rt_cases[rt_cases$gt != "one", ]
  set.seed(123)
  for (i in seq_len(nrow(cases))) {
    case <- cases[i, ]
    gt <- rt_pmfs[[case$gt]]
    data <- list(
      G = length(gt), R_data = case$R, gt_data = as.array(gt),
      abs_tol = case$abs_tol, w = rnorm(1)
    )
    for (p in params) {
      data$R_param <- p[["R"]]
      data$gt_param <- p[["gt"]]
      fits <- lapply(c(cpp = 1, stan = 0), function(use_cpp) {
        data$use_cpp <- use_cpp
        suppressMessages(rstan::sampling(model, data = data, chains = 0))
      })
      # Log-scale parameters near the data values keep each case's regime
      R <- case$R * exp(rnorm(1, sd = 0.01))
      gt_par <- pmax(gt, 1e-10) * exp(rnorm(length(gt), sd = 0.01))
      upars <- c(if (p[["R"]]) log(R), if (p[["gt"]]) log(gt_par))
      R <- if (p[["R"]]) R else case$R
      gt_par <- if (p[["gt"]]) gt_par else gt
      lp <- rstan::log_prob(fits$cpp, upars)
      expect_equal(lp, rstan::log_prob(fits$stan, upars), tolerance = 1e-10)
      # The C++ gradient is the implicit function theorem gradient at the
      # returned root
      ift <- R_to_r_ift(lp / data$w, R, gt_par)
      expected <- data$w * c(
        if (p[["R"]]) ift$R * R, if (p[["gt"]]) ift$gt * gt_par
      )
      grad <- rstan::grad_log_prob(fits$cpp, upars)
      expect_equal(as.vector(grad), expected, tolerance = 1e-10)
      # At the tolerance the model uses, the returned r is close enough to
      # the root that the gradient matches finite differences of it
      if (case$abs_tol == 1e-8) {
        h <- 1e-5
        fd <- vapply(seq_along(upars), function(j) {
          e <- h * (seq_along(upars) == j)
          (rstan::log_prob(fits$cpp, upars + e) -
            rstan::log_prob(fits$cpp, upars - e)) / (2 * h)
        }, numeric(1))
        expect_equal(as.vector(grad), fd, tolerance = 1e-6)
      }
      # The Stan version differentiates the Newton steps, which agrees
      # with it to within the solver tolerance
      expect_equal(
        as.vector(grad),
        as.vector(rstan::grad_log_prob(fits$stan, upars)),
        tolerance = 10 * case$abs_tol
      )
    }
  }
})
