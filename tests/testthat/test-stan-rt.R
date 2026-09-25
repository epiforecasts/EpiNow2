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

# Test the C++ primitives used by update_Rt
test_that("exp_add produces expected output", {
  expect_equal(exp_add(0.5, c(0, 1, -2)), exp(c(0.5, 1.5, -1.5)))
  expect_equal(exp_add(0.5, numeric(0)), numeric(0))
})

test_that("exp_add_indexed produces expected output", {
  # Indices not starting at 1, repeated and moving backwards
  expect_equal(
    exp_add_indexed(0.1, c(0, 0.2, -0.3), c(2, 2, 3, 1, 2), rep(0.05, 5)),
    exp(0.1 + c(0.2, 0.2, -0.3, 0, 0.2) + 0.05)
  )
})

test_that("exp_add_indexed errors for bad indices", {
  expect_error(exp_add_indexed(0, c(0, 1), c(1, 3), c(0, 0)))
  expect_error(exp_add_indexed(0, c(0, 1), c(1, 2), 0))
})

test_that("cumsum_hold produces expected output", {
  expect_equal(cumsum_hold(c(0.1, 0.2), 5), c(0, 0.1, 0.3, 0.3, 0.3))
  expect_equal(cumsum_hold(c(0.1, 0.2), 3), c(0, 0.1, 0.3))
  expect_equal(cumsum_hold(numeric(0), 3), rep(0, 3))
})

test_that("cumsum_hold errors when t is shorter than x plus one", {
  expect_error(cumsum_hold(c(0.1, 0.2), 2))
})

test_that("C++ primitives match pure Stan in value and gradient", {
  skip_if_not_installed("rstan")
  model <- stan_test_model("rt_primitives_gradient.stan")
  # Arguments each primitive uses: fn 1 exp_add, 2 exp_add_indexed,
  # 3 cumsum_hold
  used <- list(c("c", "x"), c("c", "levels", "x"), "x")
  cases <- list(
    list(fn = 1, t = 10, n = 10, idx = rep(1, 10), len = 10),
    list(fn = 2, t = 12, n = 12, len = 12,
         idx = c(2, 2, 4, 4, 5, 3, 3, 1, 5, 5, 5, 5)),
    list(fn = 3, t = 10, n = 9, idx = rep(1, 9), len = 10),
    list(fn = 3, t = 12, n = 6, idx = rep(1, 6), len = 12)
  )
  set.seed(123)
  for (case in cases) {
    L <- max(case$idx)
    data <- c(case, list(
      L = L, idx = as.array(case$idx), c_data = rnorm(1, 0, 0.2),
      levels_data = as.array(rnorm(L, 0, 0.1)),
      x_data = as.array(rnorm(case$n, 0, 0.1)),
      r = as.array(rnorm(case$len))
    ))
    args <- used[[case$fn]]
    params <- expand.grid(rep(list(0:1), length(args)))[-1, , drop = FALSE]
    names(params) <- args
    for (i in seq_len(nrow(params))) {
      for (a in c("c", "levels", "x")) {
        data[[paste0(a, "_param")]] <- if (a %in% args) params[i, a] else 0L
      }
      fits <- lapply(c(cpp = 1, stan = 0), function(use_cpp) {
        data$use_cpp <- use_cpp
        suppressMessages(rstan::sampling(model, data = data, chains = 0))
      })
      for (k in 1:3) {
        upars <- c(
          if (data$c_param) rnorm(1, 0, 0.2),
          if (data$levels_param) rnorm(L, 0, 0.1),
          if (data$x_param) rnorm(case$n, 0, 0.1)
        )
        expect_equal(
          rstan::log_prob(fits$cpp, upars),
          rstan::log_prob(fits$stan, upars),
          tolerance = 1e-12
        )
        expect_equal(
          rstan::grad_log_prob(fits$cpp, upars),
          rstan::grad_log_prob(fits$stan, upars),
          tolerance = 1e-12
        )
      }
    }
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
