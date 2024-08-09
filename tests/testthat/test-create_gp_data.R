test_that("create_gp_data returns correct default values when GP is disabled", {
  data <- list(t = 30, seeding_time = 7, horizon = 7, future_fixed = 0, fixed_from = 0)
  gp_data <- create_gp_data(NULL, data)
  expect_true(gp_data$fixed)
  expect_equal(gp_data$stationary, 1)
  expect_equal(gp_data$M, 18)  # (30 - 7) * 0.2
  expect_equal(gp_data$L, 1.5)
  expect_equal(gp_data$ls_meanlog, convert_to_logmean(21, 7))
  expect_equal(gp_data$ls_sdlog, convert_to_logsd(21, 7))
  expect_equal(gp_data$ls_min, 0)
  expect_equal(gp_data$ls_max, 16)  # 30 - 7 - 7
  expect_equal(gp_data$alpha_sd, 0.05)
  expect_equal(gp_data$gp_type, 2)  # Default to Matern
  expect_equal(gp_data$nu, 3 / 2)
  expect_equal(gp_data$w0, 1.0)
})

test_that("create_gp_data sets correct gp_type and nu for different kernels", {
  data <- list(t = 30, seeding_time = 7, horizon = 7, future_fixed = 0, fixed_from = 0)

  gp <- gp_opts(kernel = "se")
  gp_data <- create_gp_data(gp, data)
  expect_equal(gp_data$gp_type, 0)
  expect_equal(gp_data$nu, Inf)

  gp <- gp_opts(kernel = "periodic")
  gp_data <- create_gp_data(gp, data)
  expect_equal(gp_data$gp_type, 1)
  expect_equal(gp_data$nu, 3 / 2)  # Default Matern order
  expect_equal(gp_data$w0, 1.0)

  gp <- gp_opts(kernel = "ou")
  gp_data <- create_gp_data(gp, data)
  expect_equal(gp_data$gp_type, 2)
  expect_equal(gp_data$nu, 1 / 2)
})

test_that("create_gp_data correctly adjusts ls_max", {
  data <- list(t = 30, seeding_time = 7, horizon = 7, future_fixed = 0, fixed_from = 0)
  gp <- gp_opts(ls_max = 50)
  gp_data <- create_gp_data(gp, data)
  expect_equal(gp_data$ls_max, 16)  # 30 - 7 - 7
})

test_that("create_gp_data correctly handles future_fixed", {
  data <- list(t = 30, seeding_time = 7, horizon = 7, future_fixed = 1, fixed_from = 2)
  gp_data <- create_gp_data(gp_opts(), data)
  expect_equal(gp_data$M, 16)  # (30 - 7 - (7 - 2)) * 0.2
})