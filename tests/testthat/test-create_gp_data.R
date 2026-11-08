# Truncated quantiles of the default lengthscale prior
default_ls_quantiles <- function(probs = c(0.05, 0.95)) {
  pars <- get_parameters(LogNormal(mean = 21, sd = 7))
  p_max <- plnorm(60, pars$meanlog, pars$sdlog)
  qlnorm(probs * p_max, pars$meanlog, pars$sdlog)
}

test_that("gp_half_range returns the half-range of the GP time points", {
  expect_equal(gp_half_range(22), 10.5)
  expect_equal(gp_half_range(1), 0.5)
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
  expect_equal(
    gp_ls_quantiles(Weibull(shape = 2, scale = 10)),
    qweibull(c(0.05, 0.95), shape = 2, scale = 10)
  )
})

test_that("gp_ls_quantiles errors for unsupported priors", {
  expect_error(
    gp_ls_quantiles(LogNormal(meanlog = Normal(3, 0.1), sdlog = 0.3)),
    "boundary_scale"
  )
  expect_error(
    gp_ls_quantiles(NonParametric(c(0.5, 0.5))),
    "boundary_scale"
  )
})

test_that("check_gp_ls_prior errors for unsupported priors", {
  expect_invisible(check_gp_ls_prior(Gamma(mean = 21, sd = 7)))
  expect_silent(check_gp_ls_prior(Weibull(shape = 2, scale = 10)))
  expect_error(
    check_gp_ls_prior(NonParametric(c(0.5, 0.5))), "boundary_scale"
  )
})

test_that("gp_basis_settings chooses L and M from the lengthscale prior", {
  gp <- new_gp_settings()
  S <- gp_half_range(22)
  q <- default_ls_quantiles()
  expected_L <- max(1.2, 4.5 * q[2] / S)
  basis <- gp_basis_settings(gp, 22, 2L)
  expect_equal(basis$L, expected_L)
  expect_equal(basis$M, ceiling(3.42 * expected_L * S / q[1]))
})

test_that("gp_basis_settings uses kernel-specific constants", {
  S <- gp_half_range(22)
  q <- default_ls_quantiles()

  se <- gp_basis_settings(new_gp_settings(kernel = "se"), 22, 0L)
  expect_equal(se$L, max(1.2, 3.2 * q[2] / S))
  expect_equal(se$M, ceiling(1.75 * se$L * S / q[1]))

  m52 <- gp_basis_settings(new_gp_settings(matern_order = 5 / 2), 22, 2L)
  expect_equal(m52$L, max(1.2, 4.1 * q[2] / S))
  expect_equal(m52$M, ceiling(2.65 * m52$L * S / q[1]))

  # Matern 1/2 (equivalently "ou") has its own derived m_factor, not the
  # Matern 3/2 fallback used for other orders the paper does not cover
  ou <- gp_basis_settings(new_gp_settings(kernel = "ou"), 22, 2L)
  expect_equal(ou$L, max(1.2, 4.5 * q[2] / S))
  expect_equal(ou$M, ceiling(8.6 * ou$L * S / q[1]))
})

test_that("gp_basis_settings enforces a minimum boundary factor of 1.2", {
  basis <- gp_basis_settings(
    new_gp_settings(ls = LogNormal(mean = 3, sd = 0.5)), 300, 2L
  )
  expect_equal(basis$L, 1.2)
})

test_that("gp_basis_settings respects user-specified basis_prop and boundary_scale", {
  basis <- gp_basis_settings(
    new_gp_settings(basis_prop = 0.2, boundary_scale = 1.5), 22, 2L
  )
  expect_equal(basis$L, 1.5)
  expect_equal(basis$M, ceiling(22 * 0.2))

  # a user boundary is used when choosing the number of basis functions
  S <- gp_half_range(22)
  basis <- gp_basis_settings(new_gp_settings(boundary_scale = 3), 22, 2L)
  expect_equal(basis$L, 3)
  expect_equal(basis$M, ceiling(3.42 * 3 * S / default_ls_quantiles()[1]))
})

test_that("gp_basis_settings chooses M for the periodic kernel from the prior", {
  S <- gp_half_range(22)
  basis <- gp_basis_settings(new_gp_settings(kernel = "periodic"), 22, 1L)
  expect_equal(basis$M, ceiling(3.72 * S / default_ls_quantiles()[1]))

  basis <- gp_basis_settings(
    new_gp_settings(kernel = "periodic", basis_prop = 0.2), 22, 1L
  )
  expect_equal(basis$M, ceiling(22 * 0.2))
})

test_that("gp_ls_range has no upper limit for the periodic kernel", {
  basis <- gp_basis_settings(new_gp_settings(kernel = "periodic"), 22, 1L)
  expect_equal(
    gp_ls_range(1L, 3 / 2, basis$L, basis$M, 22),
    c(3.72 * gp_half_range(22) / basis$M, Inf)
  )
})

test_that("resolve_gp_component chooses basis settings from the free window", {
  params <- list(
    make_param("fraction_observed", GP(mean = Normal(0.5, 0.1)),
      lower_bound = 0
    )
  )
  out <- create_stan_params(
    params, states_supported = "fraction_observed", t = 37, horizon = 7
  )
  # a non-R, non-I state's free window is t - seeding_time (0 here), held at
  # its "latest" value through the horizon by default
  free <- 37 - 7
  basis <- gp_basis_settings(new_gp_settings(), free, 2L)
  expect_equal(out$gp_basis_prop, array(basis$M / free))
  expect_equal(out$gp_boundary_scale, array(basis$L))
})

test_that("check_gp_lengthscale warns when the lengthscale is outside the approximation range", {
  basis <- gp_basis_settings(new_gp_settings(), 22, 2L)
  range <- gp_ls_range(2L, 3 / 2, basis$L, basis$M, 22)
  expect_silent(
    check_gp_lengthscale(rep(mean(range), 10), 2L, 3 / 2, basis$L, basis$M, 22)
  )
  expect_warning(
    check_gp_lengthscale(
      rep(range[1] / 2, 10), 2L, 3 / 2, basis$L, basis$M, 22
    ),
    "shorter"
  )
  expect_warning(
    check_gp_lengthscale(
      rep(range[2] * 2, 10), 2L, 3 / 2, basis$L, basis$M, 22
    ),
    "longer"
  )
})

test_that("check_gp_lengthscale warns for short periodic lengthscales", {
  basis <- gp_basis_settings(new_gp_settings(kernel = "periodic"), 22, 1L)
  range <- gp_ls_range(1L, 3 / 2, basis$L, basis$M, 22)
  expect_silent(
    check_gp_lengthscale(rep(range[1] * 10, 10), 1L, 3 / 2, basis$L, basis$M, 22)
  )
  expect_warning(
    check_gp_lengthscale(
      rep(range[1] / 2, 10), 1L, 3 / 2, basis$L, basis$M, 22
    ),
    "shorter"
  )
})

test_that("check_gp_fit checks every GP component's own lengthscale column", {
  params <- list(
    make_param("fraction_observed", GP(mean = Normal(0.5, 0.1)),
      lower_bound = 0
    )
  )
  stan_data <- create_stan_params(
    params, states_supported = "fraction_observed", t = 37, horizon = 7
  )
  stan_data$t <- 37
  stan_data$seeding_time <- 0
  stan_data$horizon <- 7
  stan_data$param_id_I <- 0L # no latent-infections state in this test
  stan_data$params_variable_lookup <- c(1, 2, 3) # rho is gp_rho_id[1] == 3
  free <- 37 - 7
  m <- ceiling(free * stan_data$gp_basis_prop[1])
  ls_range <- gp_ls_range(
    stan_data$gp_kernel[1], stan_data$gp_nu[1], stan_data$gp_boundary_scale[1],
    m, free
  )
  params_samples <- cbind(
    rep(ls_range[1] / 4, 10), rep(ls_range[2] * 4, 10), rep(mean(ls_range), 10)
  )
  local_mocked_bindings(
    extract_stan_samples = function(...) list(params = params_samples)
  )
  expect_silent(check_gp_fit(list(), stan_data))
  params_samples[, 3] <- ls_range[1] / 4
  local_mocked_bindings(
    extract_stan_samples = function(...) list(params = params_samples)
  )
  expect_warning(check_gp_fit(list(), stan_data), "shorter")
  stan_data$params_variable_lookup <- c(1, 2, 4)
  expect_silent(check_gp_fit(list(), stan_data))
})

test_that("check_gp_fit is silent when there are no GP components", {
  expect_silent(check_gp_fit(list(), list(n_gp_components = 0L)))
})

test_that("check_gp_fit is silent when the fit is NULL", {
  expect_silent(check_gp_fit(NULL, list(n_gp_components = 1L)))
})

test_that("gp_half_range returns the half-range of the GP time points", {
  expect_equal(gp_half_range(22), 10.5)
  expect_equal(gp_half_range(1), 0.5)
})

test_that("check_gp_ls_prior errors for unsupported priors", {
  expect_invisible(check_gp_ls_prior(Gamma(mean = 21, sd = 7)))
  expect_silent(check_gp_ls_prior(Weibull(shape = 2, scale = 10)))
  expect_error(
    check_gp_ls_prior(NonParametric(c(0.5, 0.5))), "boundary_scale"
  )
})
