skip_on_cran()
skip_on_os("windows")

# Test calc_conv_indices_xlen function
test_that("calc_conv_indices_xlen calculates correct indices", {
  expect_equal(calc_conv_indices_xlen(1, 5, 3), c(1, 1, 3, 3))
  expect_equal(calc_conv_indices_xlen(3, 5, 3), c(1, 3, 1, 3))
  expect_equal(calc_conv_indices_xlen(5, 5, 3), c(3, 5, 1, 3))
})

# Test calc_conv_indices_len function
test_that("calc_conv_indices_len calculates correct indices", {
  expect_equal(calc_conv_indices_len(6, 5, 3), c(4, 5, 1, 2))
  expect_equal(calc_conv_indices_len(7, 5, 3), c(5, 5, 1, 1))
  expect_equal(calc_conv_indices_len(8, 5, 3), c(6, 5, 1, 0))
})

test_that("convolve_with_rev_pmf can combine two pmfs as expected", {
  expect_equal(
    convolve_with_rev_pmf(c(0.1, 0.2, 0.7), rev(c(0.1, 0.2, 0.7)), 5),
    c(0.01, 0.04, 0.18, 0.28, 0.49),
    tolerance = 0.01
  )
  expect_equal(
    sum(convolve_with_rev_pmf(
      c(0.05, 0.55, 0.4), rev(c(0.1, 0.2, 0.7)), 5
    )), 1
  )
})

test_that("convolve_with_rev_pmf performs the same as a numerical convolution", {
  # Sample and analytical PMFs for two Poisson distributions
  x <- rpois(100000, 3)
  xpmf <- dpois(0:20, 3)
  y <- rpois(100000, 5)
  ypmf <- dpois(0:20, 5)
  # Add sampled Poisson distributions up to get combined distribution
  z <- x + y
  # Analytical convolution of PMFs
  conv_pmf <- convolve_with_rev_pmf(xpmf, rev(ypmf), 41)
  conv_cdf <- cumsum(conv_pmf)
  # Empirical convolution of PMFs
  cdf <- ecdf(z)(0:40)
  # Test analytical and numerical convolutions are similar with a small error
  # allowed
  expect_lte(sum(abs(conv_cdf - cdf)), 0.1)
})

test_that("convolve_with_rev_pmf can combine vectors as we expect", {
  expect_equal(
    convolve_with_rev_pmf(c(0.1, 0.2, 0.7), rev(c(0.1, 0.2, 0.7)), 3),
    c(0.01, 0.04, 0.18),
    tolerance = 0.01
  )
  expect_equal(
    convolve_with_rev_pmf(
      seq_len(10), rev(c(0.1, 0.4, 0.3, 0.2)), 10
    ),
    c(0.1, 0.6, 1.4, 2.4, 3.4, 4.4, 5.4, 6.4, 7.4, 8.4)
  )
  x <- seq_len(10)
  x[2:10] <- x[1:9] / 2
  x[1] <- 0
  expect_equal(
    convolve_with_rev_pmf(
      seq_len(10), rev(c(0, 0.5, 0, 0)), 10
    ),
    x
  )
})

test_that("convolve_dot_product can combine two vectors where x > y and len = x", {
  x <- c(1, 2, 3, 4, 5)
  y <- c(1, 2, 3)
  expect_equal(
    convolve_with_rev_pmf(x, rev(y), 5),
    c(1, 4, 10, 16, 22)
  )
})

# Cases for comparing the C++ convolve_with_rev_pmf() with the pure Stan
# reference: len equal to, longer than and between the lengths of x and the
# full convolution, a short pmf, a pmf of length one and a pmf longer than x.
convolve_cases <- list(
  list(n = 20, D = 6, len = 20),
  list(n = 20, D = 6, len = 25),
  list(n = 20, D = 6, len = 22),
  list(n = 15, D = 2, len = 16),
  list(n = 10, D = 1, len = 10),
  list(n = 5, D = 9, len = 5),
  list(n = 5, D = 9, len = 13),
  list(n = 1, D = 1, len = 1)
)

test_that("convolve_with_rev_pmf matches the pure Stan implementation", {
  set.seed(123)
  for (case in convolve_cases) {
    x <- rexp(case$n)
    y <- rexp(case$D)
    expect_equal(
      convolve_with_rev_pmf(x, y, case$len),
      convolve_with_rev_pmf_stan(x, y, case$len),
      tolerance = 1e-12
    )
  }
})

test_that("convolve_with_rev_pmf errors for bad 'len' specifications", {
  expect_error(
    convolve_with_rev_pmf(c(1, 2, 3), c(0.5, 0.5), 5),
    "len is longer than x and y convolved"
  )
  expect_error(
    convolve_with_rev_pmf(c(1, 2, 3), c(0.5, 0.5), 2),
    "len is shorter than x"
  )
})

test_that("convolve_with_rev_pmf gradients match the pure Stan implementation", {
  skip_if_not_installed("rstan")
  # Compile the test model with the package header included before the
  # model code, as the package models are compiled.
  stanc_ret <- rstan::stanc(
    test_path("stan", "convolve_gradient.stan"),
    allow_undefined = TRUE,
    isystem = c(system.file("stan", package = "EpiNow2"), test_path("stan"))
  )
  code <- strsplit(stanc_ret$cppcode, "\n", fixed = TRUE)[[1]]
  at <- match("#include <stan/model/model_header.hpp>", trimws(code))
  stanc_ret$cppcode <- paste(
    append(code, paste0("#include \"", epinow2_stan_header(), "\""), at),
    collapse = "\n"
  )
  model <- suppressMessages(suppressWarnings(
    rstan::stan_model(stanc_ret = stanc_ret)
  ))

  set.seed(123)
  params <- list(c(1, 1), c(1, 0), c(0, 1))
  for (case in convolve_cases) {
    data <- c(case, list(
      x_data = rexp(case$n), y_data = rexp(case$D), r = rnorm(case$len)
    ))
    for (p in params) {
      data$x_param <- p[1]
      data$y_param <- p[2]
      fits <- lapply(c(cpp = 1, stan = 0), function(use_cpp) {
        data$use_cpp <- use_cpp
        suppressMessages(rstan::sampling(model, data = data, chains = 0))
      })
      upars <- rnorm(p[1] * case$n + p[2] * case$D)
      expect_equal(
        rstan::log_prob(fits$cpp, upars),
        rstan::log_prob(fits$stan, upars),
        tolerance = 1e-10
      )
      expect_equal(
        rstan::grad_log_prob(fits$cpp, upars),
        rstan::grad_log_prob(fits$stan, upars),
        tolerance = 1e-8
      )
    }
  }
})

test_that("epinow2_cmdstan_model compiles a model using the C++ header", {
  skip_if_not_installed("cmdstanr")
  skip_if(
    is.null(suppressWarnings(suppressMessages(
      tryCatch(cmdstanr::cmdstan_path(), error = function(e) NULL)
    ))),
    "CmdStan is not installed"
  )
  model <- epinow2_cmdstan_model("estimate_truncation")
  expect_s3_class(model, "CmdStanModel")
  expect_true(file.exists(model$exe_file()))
})

# Test convolve_to_report function
test_that("convolve_to_report convolves infections with delay distribution", {
  infections <- rep(100, 10)
  delay_rev_pmf <- discretised_pmf(c(log(3), 0.5), 5, 1, 0)
  seeding_time <- 3

  result <- convolve_to_report(infections, delay_rev_pmf, seeding_time)

  # Result should exclude seeding time
  expect_equal(length(result), 10 - seeding_time)

  # All values should be positive
  expect_true(all(result > 0))

  # With constant infections, reported cases should stabilise
  expect_equal(result[length(result) - 1], result[length(result)], tolerance = 0.1)
})

test_that("convolve_to_report handles zero delay correctly", {
  infections <- c(10, 20, 30, 40, 50)
  delay_rev_pmf <- numeric(0) # Empty delay
  seeding_time <- 2

  result <- convolve_to_report(infections, delay_rev_pmf, seeding_time)

  # With no delay, should just drop seeding time
  expect_equal(result, infections[(seeding_time + 1):5])
})

test_that("convolve_to_report produces correct length output", {
  infections <- rep(50, 15)
  delay_rev_pmf <- discretised_pmf(c(log(2), 0.3), 7, 1, 0)
  seeding_time <- 5

  result <- convolve_to_report(infections, delay_rev_pmf, seeding_time)

  expect_equal(length(result), 15 - seeding_time)
})

test_that("convolve_to_report with increasing infections shows delay", {
  # Growing infections
  infections <- exp(0.1 * (1:20))
  delay_rev_pmf <- discretised_pmf(c(log(3), 0.4), 8, 1, 0)
  seeding_time <- 5

  result <- convolve_to_report(infections, delay_rev_pmf, seeding_time)

  # Reports should show overall increasing trend
  # (last report should be higher than first, though not necessarily monotonic)
  expect_gt(result[length(result)], result[1])

  # Most differences should be positive
  expect_true(sum(diff(result) > 0) > sum(diff(result) < 0))

  # But reports should lag behind infections due to delay
  expect_lt(result[1], infections[seeding_time + 1])
})

test_that("convolve_to_report handles decreasing infections", {
  # Declining epidemic
  infections <- exp(-0.1 * (1:20))
  delay_rev_pmf <- discretised_pmf(c(log(3), 0.4), 8, 1, 0)
  seeding_time <- 5

  result <- convolve_to_report(infections, delay_rev_pmf, seeding_time)

  # Reports should show overall declining trend
  # (last report should be lower than first, though not necessarily monotonic)
  expect_lt(result[length(result)], result[1])

  # Most differences should be negative
  expect_true(sum(diff(result) < 0) > sum(diff(result) > 0))
})

test_that("convolve_to_report handles step change in infections", {
  # Abrupt increase
  infections <- c(rep(50, 10), rep(200, 10))
  delay_rev_pmf <- discretised_pmf(c(log(3), 0.4), 8, 1, 0)
  seeding_time <- 5

  result <- convolve_to_report(infections, delay_rev_pmf, seeding_time)

  # Reports should show gradual increase due to delay smoothing
  expect_true(any(diff(result) > 0))
})
