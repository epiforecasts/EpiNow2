skip_on_cran()
skip_on_os("windows")

test_that("assemble_state holds a random-walk deviation beyond the free window", {
  steps <- rep(0.1, 9)
  dev <- rw_dev(n_free = 10, n_centre = 10, steps = steps, period = 1)
  traj <- assemble_state(t = 15, n_free = 10, level = 1, dev = dev, link = 0)
  expect_length(traj, 15)
  # the walk is held constant from n_free + 1 through the forecast horizon
  expect_true(all(abs(traj[11:15] - traj[10]) < 1e-9))
})

test_that("assemble_state keeps a random-walk deviation varying when projected", {
  steps <- rep(c(0.2, -0.2), length.out = 14)
  dev <- rw_dev(n_free = 15, n_centre = 10, steps = steps, period = 1)
  traj <- assemble_state(t = 15, n_free = 15, level = 1, dev = dev, link = 0)
  expect_length(traj, 15)
  expect_gt(max(abs(traj[11:15] - traj[10])), 1e-6)
})

test_that("assemble_state holds a Gaussian process deviation beyond the free window", {
  noise <- seq(-0.2, 0.2, length.out = 9)
  dev <- gp_dev(n_free = 10, n_centre = 10, noise = noise, anchor = 1)
  traj <- assemble_state(t = 15, n_free = 10, level = 1, dev = dev, link = 0)
  expect_length(traj, 15)
  expect_true(all(abs(traj[11:15] - traj[10]) < 1e-9))
})

test_that("get_state_trajectory halves M for the periodic kernel's doubled coefficients", {
  nf <- 10
  m_base <- 3
  L <- 1.5
  w0 <- 1
  alpha <- 0.5
  rho <- 2
  eta <- rnorm(2 * m_base)

  x <- seq_len(nf)
  x <- 2 * (x - mean(x)) / (max(x) - 1)
  phi <- PHI_periodic(nf, m_base, w0, x) # nf x (2 * m_base)
  rescaled_rho <- rescale_gp_lengthscale(rho, nf)
  expected_dev <- gp_dev(
    n_free = nf, n_centre = nf,
    noise = update_gp(phi, m_base, L, alpha, rescaled_rho, eta, 1, 0),
    anchor = 0
  )

  traj <- get_state_trajectory(
    id = 1, t = nf, level = 1,
    state_param_id = 1L, state_link = 0L, state_anchor = 0L,
    state_comp_offset = 0L, state_comp_n = 1L,
    state_n_free = nf, state_n_centre = nf,
    comp_type = 1L, comp_pos = 1L,
    comp_rw_n = 0L, comp_rw_offset = 0L, state_rw_period = 1L,
    rw_knots_n = 0L, rw_knots_offset = 0L, rw_knots = integer(0),
    state_rw_steps = numeric(0),
    comp_gp_M = 2L * m_base, comp_gp_offset = 0L, state_gp_eta = eta,
    gp_boundary_scale = L, gp_kernel = 1L, gp_nu = 0,
    state_gp_alpha = alpha, state_gp_rho = rho,
    gp_phi = list(phi)
  )

  expect_equal(traj, exp(expected_dev))
})

test_that("get_state_trajectory returns a constant trajectory when no state is attached", {
  traj <- get_state_trajectory(
    id = 1, t = 12, level = 2,
    state_param_id = integer(0), state_link = integer(0),
    state_anchor = integer(0),
    state_comp_offset = integer(0), state_comp_n = integer(0),
    state_n_free = integer(0), state_n_centre = integer(0),
    comp_type = integer(0), comp_pos = integer(0),
    comp_rw_n = integer(0), comp_rw_offset = integer(0), state_rw_period = 1,
    rw_knots_n = integer(0), rw_knots_offset = integer(0),
    rw_knots = integer(0),
    state_rw_steps = numeric(0),
    comp_gp_M = integer(0), comp_gp_offset = integer(0),
    state_gp_eta = numeric(0),
    gp_boundary_scale = numeric(0), gp_kernel = integer(0), gp_nu = numeric(0),
    state_gp_alpha = numeric(0), state_gp_rho = numeric(0),
    gp_phi = list()
  )
  expect_length(traj, 12)
  expect_true(all(traj == 2))
})

test_that("gp_dev with the init anchor starts at zero and accumulates the increments", {
  noise <- c(0.1, -0.3, 0.2, 0.05, 0.4)
  dev <- gp_dev(n_free = 6, n_centre = 4, noise = noise, anchor = 1)
  expect_length(dev, 6)
  expect_equal(diff(dev), noise)
  # centred over the observation window
  expect_equal(mean(dev[1:4]), 0)
})

test_that("gp_dev centring does not depend on the forecast horizon", {
  noise <- c(0.1, -0.3, 0.2, 0.05, 0.4, -0.1, 0.2)
  short <- gp_dev(n_free = 6, n_centre = 4, noise = noise[1:5], anchor = 1)
  long <- gp_dev(n_free = 8, n_centre = 4, noise = noise, anchor = 1)
  expect_equal(long[1:6], short)
})

test_that("gp_dev with the mean anchor returns the noise unchanged", {
  noise <- c(0.1, -0.3, 0.2)
  expect_equal(gp_dev(n_free = 3, n_centre = 3, noise = noise, anchor = 0), noise)
})

test_that("gp_noise_points has one fewer point for the init anchor", {
  expect_equal(gp_noise_points(10, 0), 10)
  expect_equal(gp_noise_points(10, 1), 9)
})

test_that("rw_dev steps every period days and is centred", {
  steps <- c(0.5, -0.2)
  dev <- rw_dev(n_free = 15, n_centre = 15, steps = steps, period = 7)
  uncentred <- c(rep(0, 7), rep(0.5, 7), 0.3)
  expect_equal(dev, uncentred - mean(uncentred))
})

test_that("rw_dev_knots steps on the knot days and is centred", {
  steps <- c(0.5, -0.2)
  dev <- rw_dev_knots(n_free = 8, n_centre = 6, steps = steps, knots = c(3L, 6L))
  uncentred <- c(0, 0, 0.5, 0.5, 0.5, 0.3, 0.3, 0.3)
  expect_equal(dev, uncentred - mean(uncentred[1:6]))
})

test_that("rw_dev_knots matches a legacy rw = 7 breakpoint vector", {
  steps <- c(0.5, -0.2)
  n <- 20
  dev <- rw_dev_knots(
    n_free = n, n_centre = n, steps = steps,
    knots = as.integer(seq(7, n, by = 7))
  )
  # previous implementation: level index floor(i / rw)
  levels <- c(0, cumsum(steps))[floor(seq_len(n) / 7) + 1]
  expect_equal(dev, levels - mean(levels))
})
