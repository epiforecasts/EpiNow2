skip_on_cran()
skip_on_os("windows")

test_that("rw_trajectory returns the requested length and holds beyond the free window", {
  steps <- rep(0.1, 9)
  traj <- rw_trajectory(
    t = 15, n_free = 10, n_centre = 10, level = 1, steps = steps,
    link = 0, period = 1
  )
  expect_length(traj, 15)
  # the walk is held constant from n_free + 1 through the forecast horizon
  expect_true(all(abs(traj[11:15] - traj[10]) < 1e-9))
})

test_that("rw_trajectory keeps varying across the horizon when projected", {
  steps <- rep(c(0.2, -0.2), length.out = 14)
  traj <- rw_trajectory(
    t = 15, n_free = 15, n_centre = 10, level = 1, steps = steps,
    link = 0, period = 1
  )
  expect_length(traj, 15)
  expect_gt(max(abs(traj[11:15] - traj[10])), 1e-6)
})

test_that("gp_trajectory returns the requested length and holds beyond the free window", {
  noise <- seq(-0.2, 0.2, length.out = 10)
  traj <- gp_trajectory(
    t = 15, n_free = 10, n_centre = 10, level = 1, noise = noise,
    link = 0, anchor = 1
  )
  expect_length(traj, 15)
  expect_true(all(abs(traj[11:15] - traj[10]) < 1e-9))
})

test_that("get_state_trajectory returns a constant trajectory when no state is attached", {
  traj <- get_state_trajectory(
    id = 1, t = 12, n_free = 8, n_centre = 8, level = 2,
    state_param_id = integer(0), state_type = integer(0),
    state_link = integer(0), state_pos = integer(0), state_anchor = integer(0),
    state_rw_steps = numeric(0), state_rw_n = 0, state_rw_period = 1,
    state_gp_eta = numeric(0), gp_M = 0, gp_PHI = matrix(0, 1, 1),
    gp_boundary_scale = 1.5, gp_kernel = integer(0), gp_nu = numeric(0),
    state_gp_alpha = numeric(0), state_gp_rho = numeric(0)
  )
  expect_length(traj, 12)
  expect_true(all(traj == 2))
})
