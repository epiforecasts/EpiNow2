skip_on_cran()
skip_on_os("windows")

# test update_infectiousness
test_that("update_infectiousness works as expected with default settings", {
  expect_equal(
    update_infectiousness(rep(1, 20), rep(0.1, 10), 5, 10),
    1
  )
  expect_equal(
    update_infectiousness(rep(1, 20), rep(0.1, 5), 5, 10),
    0.5
  )
  expect_error(update_infectiousness(rep(1, 20), rep(0.1, 5), 5, 10, 10))
})

pmf <- discretised_pmf(c(2.25, 0.75), 15, 2, 0)
gt_rev_pmf <- get_delay_rev_pmf(
  1L, 15L, array(0L), array(1L),
  array(c(1L, 2L)), array(15L), pmf,
  array(c(1L, 16L)), numeric(0), 1L, 0L,
  1L, 1L, 0L
)

# test generate infections
test_that("generate_infections works as expected", {
  expect_equal(
    round(generate_infections(c(1, rep(1, 9)), 10, gt_rev_pmf, log(1000), 0, 0, 1.0, 0, 0, 0, 1), 0),
    c(rep(1000, 10), 995, 996, rep(997, 8))
  )
  expect_equal(
    round(generate_infections(c(1, rep(1.1, 9)), 10, gt_rev_pmf, log(20), 0, 0, 1.0, 0, 0, 0, 1), 0),
    c(rep(20, 10), 20, 22, 22, 23, 24, 25, 25, 26, 27, 28)
  )
  expect_equal(
    round(generate_infections(c(1, rep(1.1, 9)), 10, gt_rev_pmf, log(100), 0, 0, 1.0, 0, 0, 0, 1), 0),
    c(rep(100, 10), 99, 110, 112, 115, 119, 123, 126, 130, 135, 139)
  )
  expect_equal(
    round(generate_infections(c(1, rep(1, 9)), 4, gt_rev_pmf, log(500), 0, 0, 1.0, 0, 0, 0, 1), 0),
    c(rep(500, 4), 398, 420, 425, 426, 426, 427, 427, 427, 427, 427)
  )
  expect_equal(
    round(generate_infections(c(1, rep(1.1, 9)), 4, gt_rev_pmf, log(500), 0, 0, 1.0, 0, 0, 0, 1), 0),
    c(rep(500, 4), 398, 462, 478, 493, 508, 524, 541, 558, 575, 594)
  )
  expect_equal(
    round(generate_infections(c(1, rep(1, 9)), 1, gt_rev_pmf, log(40), 0, 0, 1.0, 0, 0, 0, 1), 0),
    c(40, 8, 12, 13, rep(13, 7))
  )
  expect_equal(
    round(generate_infections(c(1, rep(1.1, 9)), 1, gt_rev_pmf, log(100), 0, 0, 1.0, 0, 0, 0, 1), 0),
    c(100, 21, 32, 35, 37, 38, 39, 40, 42, 43, 44)
  )
  expect_equal(
    round(generate_infections(c(1, rep(1, 9)), 10, gt_rev_pmf, log(1000), 100000, 2, 1.0, 4, 0, 0, 1), 0),
    c(rep(1000, 10), 989, 990, 989, 987, 985, 983, 981, 980, 978, 976)
  )
})

test_that("generate_infections respects pop_floor with population adjustment", {
  # Test with higher pop_floor to verify floor behavior
  expect_equal(
    round(generate_infections(c(1, rep(1, 9)), 10, gt_rev_pmf, log(1000), 100000, 2, 10.0, 4, 0, 0, 1), 0),
    c(rep(1000, 10), 989, 990, 989, 987, 985, 983, 981, 980, 978, 976)
  )

  # Test with very small population where floor matters
  result <- generate_infections(c(1, rep(1.5, 9)), 10, gt_rev_pmf, log(100), 500, 2, 50.0, 4, 0, 0, 1)
  # With pop_floor = 50, susceptible population should never go below 50
  # This allows infections to continue even when pop - cum_infections < 50
  expect_true(all(result >= 0))
  expect_true(all(is.finite(result)))
})

# Cases for renewal_infections(), also compared with the pure Stan
# reference: each depletion mode, the edges of nht, a binding pop_floor, a
# generation time of length one, one longer than the series and one seed.
renewal_cases <- list(
  list(uot = 14, ot = 30, G = 15, use_pop = 0, pop = 1e4, floor = 1, nht = 0),
  list(uot = 14, ot = 30, G = 15, use_pop = 2, pop = 1e4, floor = 1, nht = 0),
  list(uot = 14, ot = 30, G = 15, use_pop = 1, pop = 1e4, floor = 1, nht = 20),
  list(uot = 14, ot = 30, G = 15, use_pop = 1, pop = 1e4, floor = 1, nht = 0),
  list(uot = 14, ot = 30, G = 15, use_pop = 1, pop = 1e4, floor = 1, nht = 29),
  list(uot = 14, ot = 30, G = 15, use_pop = 1, pop = 1e4, floor = 1, nht = 30),
  list(uot = 3, ot = 40, G = 10, use_pop = 2, pop = 100, floor = 50, nht = 0),
  list(uot = 5, ot = 20, G = 1, use_pop = 2, pop = 1e3, floor = 1, nht = 0),
  list(uot = 3, ot = 10, G = 20, use_pop = 2, pop = 1e3, floor = 1, nht = 0),
  list(uot = 1, ot = 20, G = 7, use_pop = 1, pop = 1e3, floor = 1, nht = 10)
)

renewal_inputs <- function(case) {
  gt <- rexp(case$G)
  list(
    seed = as.array(5 * exp(rnorm(case$uot))),
    R = as.array(exp(0.2 * rnorm(case$ot) + 0.2)),
    gt = as.array(gt / sum(gt))
  )
}

run_case <- function(case, x) {
  renewal_infections(
    x$seed, x$R, x$gt, case$pop, case$use_pop, case$floor, case$nht
  )
}

# The defining identity of each new infection, evaluated on the past
# infections of the output itself: I_u = R_t * sum_tau g_tau I_{u - tau},
# or S_t * (1 - exp(-R_t * lambda_t / S_t)) with depletion, where g_tau is
# gt_rev_pmf[G - tau] and S_t = max(pop_floor, pop - sum of past infections).
renewal_identity <- function(inf, case, x) {
  G <- length(x$gt)
  vapply(seq_along(x$R), function(t) {
    u <- case$uot + t
    tau <- seq_len(min(G, u) - 1)
    lambda <- sum(x$gt[G - tau] * inf[u - tau])
    if (case$use_pop == 2 || (case$use_pop == 1 && t > case$nht)) {
      S <- max(case$floor, case$pop - sum(inf[seq_len(u - 1)]))
      S * (1 - exp(-x$R[t] * lambda / S))
    } else {
      x$R[t] * lambda
    }
  }, numeric(1))
}

test_that("renewal_infections passes the seeds through unchanged", {
  set.seed(123)
  for (case in renewal_cases) {
    x <- renewal_inputs(case)
    inf <- run_case(case, x)
    expect_length(inf, case$uot + case$ot)
    expect_identical(inf[seq_len(case$uot)], as.vector(x$seed))
  }
})

test_that("renewal_infections grows as R^t with a one-day generation time", {
  seed <- c(2, 3, 5)
  # gt_rev_pmf[G - 1] is the one-day weight
  inf <- renewal_infections(seed, rep(1.5, 10), c(1, 0), 1, 0, 1, 0)
  expect_equal(inf, c(seed, 5 * 1.5^(1:10)), tolerance = 1e-12)
  R <- c(0.5, 2, 1.2, 0.9, 3)
  inf <- renewal_infections(seed, R, c(1, 0), 1, 0, 1, 0)
  expect_equal(inf[-(1:3)], 5 * cumprod(R), tolerance = 1e-12)
  # A generation time longer than the series with all its mass at one day
  inf <- renewal_infections(seed, rep(1.5, 10), c(rep(0, 18), 1, 0), 1, 0, 1, 0)
  expect_equal(inf, c(seed, 5 * 1.5^(1:10)), tolerance = 1e-12)
})

test_that("renewal_infections matches closed-form exponential growth", {
  # With g_1 = g_2 = 1/2, growth at rate r solves R (e^-r + e^-2r) / 2 = 1,
  # so R = 8 / 3 doubles and R = 1 / 3 halves daily, given seeds on that path.
  gt <- c(0.5, 0.5, 0)
  inf <- renewal_infections(c(1, 2), rep(8 / 3, 10), gt, 1, 0, 1, 0)
  expect_equal(inf, 2^(0:11), tolerance = 1e-12)
  inf <- renewal_infections(c(2, 1), rep(1 / 3, 10), gt, 1, 0, 1, 0)
  expect_equal(inf, 2^(1:-10), tolerance = 1e-12)
})

test_that("renewal_infections satisfies the renewal identity", {
  set.seed(123)
  for (case in renewal_cases) {
    x <- renewal_inputs(case)
    inf <- run_case(case, x)
    expect_equal(
      inf[-seq_len(case$uot)], renewal_identity(inf, case, x),
      tolerance = 1e-12
    )
  }
})

test_that("renewal_infections matches the pure Stan implementation", {
  set.seed(123)
  for (case in renewal_cases) {
    x <- renewal_inputs(case)
    args <- list(
      x$seed, x$R, x$gt, case$pop, case$use_pop, case$floor, case$nht
    )
    expect_equal(
      do.call(renewal_infections, args),
      do.call(renewal_infections_stan, args),
      tolerance = 1e-12
    )
  }
})

test_that("renewal_infections is zero after the seeds when G = 1", {
  # gt_rev_pmf of length one only weights the current day, which is unset
  inf <- renewal_infections(c(3, 4), rep(2, 5), 1, 1e3, 2, 1, 0)
  expect_identical(inf, c(3, 4, rep(0, 5)))
})

test_that("renewal_infections depletion behaves as expected", {
  set.seed(123)
  x <- renewal_inputs(list(uot = 5, ot = 30, G = 10))
  ren <- function(pop, use_pop, nht = 0, floor = 1) {
    renewal_infections(x$seed, x$R, x$gt, pop, use_pop, floor, nht)
  }
  # A huge population is the same as no depletion
  expect_equal(ren(1e8, 2), ren(1e8, 0), tolerance = 1e-6)
  expect_equal(ren(1e8, 1, nht = 3), ren(1e8, 0), tolerance = 1e-6)
  # use_pop = 1 matches no depletion up to nht and use_pop = 2 from nht = 0
  expect_equal(ren(200, 1, nht = 12)[1:17], ren(200, 0)[1:17])
  expect_false(isTRUE(all.equal(ren(200, 1, nht = 12), ren(200, 0))))
  expect_equal(ren(200, 1, nht = 0), ren(200, 2))
  # Depletion only ever reduces infections
  expect_true(all(ren(200, 2) <= ren(200, 0)))
})

test_that("renewal_infections never exceeds the susceptible pool", {
  set.seed(123)
  for (case in renewal_cases) {
    if (case$use_pop == 0) next
    x <- renewal_inputs(case)
    inf <- run_case(case, x)
    t <- seq_len(case$ot)
    new <- inf[case$uot + t]
    susceptible <- pmax(case$floor, case$pop - cumsum(inf)[case$uot + t - 1])
    depleting <- case$use_pop == 2 | t > case$nht
    expect_true(all(new[depleting] <= susceptible[depleting]))
    expect_true(all(new >= 0))
  }
})

test_that("renewal_infections uses pop_floor once the pool falls below it", {
  set.seed(123)
  case <- renewal_cases[[7]]
  x <- renewal_inputs(case)
  inf <- run_case(case, x)
  t <- seq_len(case$ot)
  u <- case$uot + t
  remaining <- case$pop - cumsum(inf)[u - 1]
  floored <- remaining < case$floor
  # The floor binds, including once more have been infected than pop
  expect_true(any(floored))
  expect_true(any(remaining < 0))
  # On those days the susceptible pool is pop_floor, so infections carry on
  G <- case$G
  lambda <- vapply(u, function(v) {
    tau <- seq_len(min(G, v) - 1)
    sum(x$gt[G - tau] * inf[v - tau])
  }, numeric(1))
  expected <- case$floor * (1 - exp(-as.vector(x$R) * lambda / case$floor))
  expect_equal(inf[u][floored], expected[floored], tolerance = 1e-12)
  expect_true(all(inf[u][floored] > 0))
  expect_true(all(inf[u][floored] < case$floor))
})

test_that("renewal_infections gradients match the reference and finite differences", {
  skip_if_not_installed("rstan")
  model <- stan_test_model("renewal_gradient.stan")
  # Central finite differences of log_prob on the unconstrained scale. The
  # step balances truncation error against rounding in log_prob.
  fd_grad <- function(fit, upars, h = 1e-4) {
    vapply(seq_along(upars), function(i) {
      e <- replace(numeric(length(upars)), i, h)
      (rstan::log_prob(fit, upars + e) - rstan::log_prob(fit, upars - e)) /
        (2 * h)
    }, numeric(1))
  }
  # Every combination of seed, R, gt and pop as parameters, except none
  params <- expand.grid(seed = 0:1, R = 0:1, gt = 0:1, pop = 0:1)[-1, ]
  set.seed(123)
  for (case in renewal_cases) {
    x <- renewal_inputs(case)
    data <- list(
      uot = case$uot, ot = case$ot, G = case$G, use_pop = case$use_pop,
      pop_floor = case$floor, nht = case$nht, seed_data = x$seed,
      R_data = x$R, gt_data = x$gt, pop_data = case$pop,
      r = as.array(rnorm(case$uot + case$ot))
    )
    for (i in seq_len(nrow(params))) {
      p <- params[i, ]
      data[paste0(names(p), "_param")] <- as.list(as.integer(p))
      fits <- lapply(c(cpp = 1, stan = 0), function(use_cpp) {
        data$use_cpp <- use_cpp
        suppressMessages(rstan::sampling(model, data = data, chains = 0))
      })
      # Log-scale parameters near the data values keep each case's regime
      upars <- c(
        if (p$seed) log(x$seed), if (p$R) log(x$R), if (p$gt) log(x$gt),
        if (p$pop) log(case$pop)
      )
      upars <- upars + rnorm(length(upars), sd = 0.01)
      grad <- as.vector(rstan::grad_log_prob(fits$cpp, upars))
      expect_equal(
        rstan::log_prob(fits$cpp, upars),
        rstan::log_prob(fits$stan, upars),
        tolerance = 1e-10
      )
      expect_equal(
        grad, as.vector(rstan::grad_log_prob(fits$stan, upars)),
        tolerance = 1e-8
      )
      expect_equal(grad, fd_grad(fits$cpp, upars), tolerance = 1e-4)
    }
  }
})

# test deconvolve_infections
test_that("deconvolve_infections with fixed mode returns shifted cases", {
  shifted_cases <- c(10, 20, 30, 40, 50)
  noise <- numeric(0) # Noise not used in fixed mode
  result <- deconvolve_infections(shifted_cases, noise, fixed = 1, prior = 0)

  # With fixed = 1, should return shifted_cases + small offset
  expect_equal(result, shifted_cases + 1e-5, tolerance = 1e-6)
})

test_that("deconvolve_infections with prior=0 applies noise only", {
  shifted_cases <- rep(100, 10)
  noise <- rep(0.1, 10) # Small positive noise

  result <- deconvolve_infections(shifted_cases, noise, fixed = 0, prior = 0)

  # Should be close to exp(noise) since prior=0
  expected <- 1e-5 + exp(noise)
  expect_equal(result, expected, tolerance = 1e-6)
})

test_that("deconvolve_infections with prior=1 scales cases by noise", {
  shifted_cases <- c(10, 20, 30, 40, 50)
  noise <- rep(0, 5) # Zero noise for simple test

  result <- deconvolve_infections(shifted_cases, noise, fixed = 0, prior = 1)

  # With zero noise and prior=1, should be cases * exp(0) = cases
  expect_equal(result, shifted_cases + 1e-5, tolerance = 1e-6)
})

test_that("deconvolve_infections with prior=2 implements random walk", {
  shifted_cases <- c(100, 110, 120, 130, 140)
  noise <- c(0, 0.1, -0.05, 0.05, 0.1)

  result <- deconvolve_infections(shifted_cases, noise, fixed = 0, prior = 2)

  # First infection based on first case
  expect_equal(result[1], 1e-5 + shifted_cases[1] * exp(noise[1]), tolerance = 1e-6)

  # Subsequent infections follow random walk
  for (i in 2:5) {
    expect_equal(result[i], result[i - 1] * exp(noise[i]) + 1e-5, tolerance = 1e-6)
  }
})

test_that("deconvolve_infections handles different noise levels", {
  shifted_cases <- rep(50, 10)
  noise_high <- rep(0.5, 10)
  noise_low <- rep(0.1, 10)

  result_high <- deconvolve_infections(shifted_cases, noise_high, fixed = 0, prior = 1)
  result_low <- deconvolve_infections(shifted_cases, noise_low, fixed = 0, prior = 1)

  # Higher noise should produce higher infections
  expect_true(all(result_high > result_low))
})

test_that("deconvolve_infections always returns positive values", {
  shifted_cases <- c(1, 5, 10, 20, 50)
  noise <- c(-1, -0.5, 0, 0.5, 1)

  result <- deconvolve_infections(shifted_cases, noise, fixed = 0, prior = 1)

  # Should all be positive due to 1e-5 offset and exponential transform
  expect_true(all(result > 0))
})
