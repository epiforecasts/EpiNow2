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

# Cases for comparing the C++ update_Rt() with the pure Stan reference:
# each Gaussian process branch with and without breakpoints, a stationary
# GP with and without a held forecast, a non-stationary GP shorter than
# t - 1, the edges of n_centre and uneven breakpoints.
weekly_bps <- function(t) (seq_len(t) - 1) %/% 7 + 1
rt_cases <- list(
  list(t = 30, stationary = 0, n_centre = 23, gp_n = 29, bps = NULL),
  list(t = 30, stationary = 1, n_centre = 23, gp_n = 23, bps = NULL),
  list(t = 30, stationary = 1, n_centre = 30, gp_n = 30, bps = NULL),
  list(t = 30, stationary = 1, n_centre = 23, gp_n = 1, bps = NULL),
  list(t = 30, stationary = 0, n_centre = 23, gp_n = 0, bps = weekly_bps(30)),
  list(t = 30, stationary = 0, n_centre = 23, gp_n = 29, bps = weekly_bps(30)),
  list(t = 30, stationary = 1, n_centre = 23, gp_n = 23, bps = weekly_bps(30)),
  list(t = 30, stationary = 0, n_centre = 30, gp_n = 20, bps = NULL),
  list(t = 30, stationary = 0, n_centre = 1, gp_n = 29, bps = weekly_bps(30)),
  list(
    t = 12, stationary = 0, n_centre = 10, gp_n = 11,
    bps = c(1, 1, 2, 2, 2, 2, 3, 4, 4, 4, 4, 4)
  )
)

rt_inputs <- function(case) {
  bp_n <- if (is.null(case$bps)) 0 else max(case$bps) - 1
  list(
    R0 = exp(rnorm(1, 0, 0.2)),
    noise = as.array(rnorm(case$gp_n, 0, 0.1)),
    bps = if (bp_n) as.array(case$bps) else as.array(rep(1L, case$t)),
    bp_effects = as.array(rnorm(bp_n, 0, 0.1))
  )
}

test_that("update_Rt matches the pure Stan implementation", {
  set.seed(123)
  for (case in rt_cases) {
    for (i in 1:3) {
      x <- rt_inputs(case)
      args <- list(
        case$t, x$R0, x$noise, x$bps, x$bp_effects, case$stationary,
        case$n_centre
      )
      expect_equal(
        do.call(update_Rt, args), do.call(update_Rt_stan, args),
        tolerance = 1e-14
      )
    }
  }
})

test_that("update_Rt errors for inputs that cannot be indexed", {
  expect_error(update_Rt(5, 1, rep(0, 5), 1:5, numeric(0), 0, 5))
  expect_error(update_Rt(5, 1, rep(0, 6), 1:5, numeric(0), 1, 5))
  expect_error(update_Rt(5, 1, rep(0, 4), 1:5, numeric(0), 0, 6))
  expect_error(update_Rt(5, 1, numeric(0), c(1, 1, 2, 2, 3), 0.1, 0, 5))
  expect_error(update_Rt(5, 1, numeric(0), c(1, 2), 0.1, 0, 5))
})

test_that("update_Rt gradients match the pure Stan implementation", {
  skip_if_not_installed("rstan")
  model <- stan_test_model("rt_gradient.stan")
  # Every combination of R0, noise and bp_effects as parameters, except none
  params <- expand.grid(R0 = 0:1, noise = 0:1, bp = 0:1)[-1, ]
  set.seed(123)
  for (case in rt_cases) {
    x <- rt_inputs(case)
    data <- list(
      t = case$t, stationary = case$stationary, n_centre = case$n_centre,
      gp_n = length(x$noise), bp_n = length(x$bp_effects), bps = x$bps,
      R0_data = x$R0, noise_data = x$noise, bp_data = x$bp_effects,
      r = as.array(rnorm(case$t))
    )
    for (i in seq_len(nrow(params))) {
      p <- params[i, ]
      data[paste0(names(p), "_param")] <- as.list(as.integer(p))
      fits <- lapply(c(cpp = 1, stan = 0), function(use_cpp) {
        data$use_cpp <- use_cpp
        suppressMessages(rstan::sampling(model, data = data, chains = 0))
      })
      for (k in 1:3) {
        upars <- c(
          if (p$R0) rnorm(1, 0, 0.2),
          if (p$noise) rnorm(data$gp_n, 0, 0.1),
          if (p$bp) rnorm(data$bp_n, 0, 0.1)
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
