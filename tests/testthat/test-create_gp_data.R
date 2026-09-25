default_data <- list(
  t = 30, seeding_time = 7, horizon = 7, future_fixed = 0, fixed_from = 0,
  stationary = 0, estimate_r = 1
)

# Truncated quantiles of the default lengthscale prior
default_ls_quantiles <- function(probs = c(0.05, 0.95)) {
  pars <- get_parameters(LogNormal(mean = 21, sd = 7))
  p_max <- plnorm(60, pars$meanlog, pars$sdlog)
  qlnorm(probs * p_max, pars$meanlog, pars$sdlog)
}

test_that("create_gp_data returns correct default values when GP is disabled", {
  data <- default_data
  data$stationary <- NULL
  gp_data <- create_gp_data(NULL, data)
  expect_equal(gp_data$fixed, 1)
  expect_equal(gp_data$stationary, 1)
  expect_gte(gp_data$L, 1.2)
  expect_gte(gp_data$M, 1)
  expect_equal(gp_data$gp_type, 2) # Default to Matern
  expect_equal(gp_data$nu, 3 / 2)
  expect_equal(gp_data$w0, 1.0)
})

test_that("create_gp_data sets correct gp_type and nu for different kernels", {
  gp_data <- create_gp_data(gp_opts(kernel = "se"), default_data)
  expect_equal(gp_data$gp_type, 0)
  expect_equal(gp_data$nu, Inf)

  gp_data <- create_gp_data(gp_opts(kernel = "periodic"), default_data)
  expect_equal(gp_data$gp_type, 1)
  expect_equal(gp_data$nu, 3 / 2) # Default Matern order
  expect_equal(gp_data$w0, 1.0)

  gp_data <- create_gp_data(gp_opts(kernel = "ou"), default_data)
  expect_equal(gp_data$gp_type, 2)
  expect_equal(gp_data$nu, 1 / 2)
})

test_that("gp_noise_terms matches the Stan noise dimensions", {
  expect_equal(gp_noise_terms(default_data), 22)
  expect_equal(
    gp_noise_terms(modifyList(default_data, list(stationary = 1))), 23
  )
  expect_equal(
    gp_noise_terms(
      modifyList(default_data, list(future_fixed = 1, fixed_from = 2))
    ),
    17
  )
  expect_equal(
    gp_noise_terms(modifyList(default_data, list(estimate_r = 0))), 30
  )
})

test_that("create_gp_data chooses L and M from the lengthscale prior", {
  gp_data <- create_gp_data(gp_opts(), default_data)
  S <- (22 - 1) / 2
  q <- default_ls_quantiles()
  expected_L <- max(1.2, 4.5 * q[2] / S)
  expect_equal(gp_data$L, expected_L)
  expect_equal(gp_data$M, ceiling(3.42 * expected_L * S / q[1]))
})

test_that("create_gp_data uses kernel-specific constants", {
  S <- (22 - 1) / 2
  q <- default_ls_quantiles()
  se <- create_gp_data(gp_opts(kernel = "se"), default_data)
  expect_equal(se$L, max(1.2, 3.2 * q[2] / S))
  expect_equal(se$M, ceiling(1.75 * se$L * S / q[1]))
  m52 <- create_gp_data(gp_opts(matern_order = 5 / 2), default_data)
  expect_equal(m52$L, max(1.2, 4.1 * q[2] / S))
  expect_equal(m52$M, ceiling(2.65 * m52$L * S / q[1]))
})

test_that("create_gp_data enforces a minimum boundary factor of 1.2", {
  gp_data <- create_gp_data(
    gp_opts(ls = LogNormal(mean = 3, sd = 0.5)),
    modifyList(default_data, list(t = 300))
  )
  expect_equal(gp_data$L, 1.2)
})

test_that("create_gp_data respects user-specified basis_prop and boundary_scale", {
  gp_data <- create_gp_data(
    gp_opts(basis_prop = 0.2, boundary_scale = 1.5), default_data
  )
  expect_equal(gp_data$L, 1.5)
  expect_equal(gp_data$M, ceiling(22 * 0.2))

  # a user boundary is used when choosing the number of basis functions
  gp_data <- create_gp_data(gp_opts(boundary_scale = 3), default_data)
  expect_equal(gp_data$L, 3)
  expect_equal(
    gp_data$M, ceiling(3.42 * 3 * 10.5 / default_ls_quantiles()[1])
  )
})

test_that("create_gp_data falls back to basis_prop for the periodic kernel", {
  gp_data <- create_gp_data(gp_opts(kernel = "periodic"), default_data)
  expect_equal(gp_data$M, ceiling(22 * 0.2))
})

test_that("create_gp_data correctly handles future_fixed", {
  data <- modifyList(default_data, list(future_fixed = 1, fixed_from = 2))
  gp_data <- create_gp_data(gp_opts(basis_prop = 0.2), data)
  expect_equal(gp_data$M, ceiling(17 * 0.2))
})

test_that("gp_ls_quantiles returns truncated quantiles of supported priors", {
  expect_equal(
    gp_ls_quantiles(LogNormal(mean = 21, sd = 7, max = 60)),
    default_ls_quantiles()
  )
  expect_equal(gp_ls_quantiles(Fixed(10)), c(10, 10))
  gamma_pars <- get_parameters(Gamma(mean = 21, sd = 7))
  expect_equal(
    gp_ls_quantiles(Gamma(mean = 21, sd = 7)),
    qgamma(c(0.05, 0.95), gamma_pars$shape, gamma_pars$rate)
  )
  p0 <- pnorm(0, 21, 7)
  expect_equal(
    gp_ls_quantiles(Normal(mean = 21, sd = 7)),
    qnorm(p0 + c(0.05, 0.95) * (1 - p0), 21, 7)
  )
})

test_that("gp_ls_quantiles errors for unsupported priors", {
  expect_error(
    gp_ls_quantiles(LogNormal(meanlog = Normal(3, 0.1), sdlog = 0.3)),
    "boundary_scale"
  )
})

test_that("check_gp_lengthscale warns when the lengthscale is outside the approximation range", {
  stan_data <- create_gp_data(gp_opts(), default_data)
  range <- gp_ls_range(stan_data)
  expect_silent(check_gp_lengthscale(rep(mean(range), 10), stan_data))
  expect_warning(
    check_gp_lengthscale(rep(range[1] / 2, 10), stan_data),
    "shorter"
  )
  expect_warning(
    check_gp_lengthscale(rep(range[2] * 2, 10), stan_data),
    "longer"
  )
})

test_that("check_gp_lengthscale is silent when the GP is not used", {
  stan_data <- create_gp_data(NULL, default_data)
  expect_silent(check_gp_lengthscale(rep(1e-3, 10), stan_data))
  stan_data <- create_gp_data(gp_opts(kernel = "periodic"), default_data)
  expect_silent(check_gp_lengthscale(rep(1e-3, 10), stan_data))
})

test_that("check_gp_fit checks the lengthscale column of the params samples", {
  stan_data <- create_gp_data(gp_opts(), default_data)
  stan_data$param_id_rho <- 2
  stan_data$params_variable_lookup <- c(1, 3, 2)
  ls_range <- gp_ls_range(stan_data)
  params <- cbind(
    rep(ls_range[1] / 4, 10), rep(ls_range[2] * 4, 10), rep(mean(ls_range), 10)
  )
  local_mocked_bindings(
    extract_samples = function(...) list(params = params)
  )
  expect_silent(check_gp_fit(list(), stan_data))
  params[, 3] <- ls_range[1] / 4
  expect_warning(check_gp_fit(list(), stan_data), "shorter")
})

test_that("check_gp_fit is silent when the lengthscale is fixed", {
  stan_data <- create_gp_data(gp_opts(), default_data)
  stan_data$param_id_rho <- 1
  stan_data$params_variable_lookup <- 0
  expect_silent(check_gp_fit(list(), stan_data))
})

test_that("gp_half_range returns the half-range of the GP time points", {
  expect_equal(gp_half_range(22), 10.5)
  expect_equal(gp_half_range(1), 0.5)
})
