test_that("create_gp_data returns correct default values when GP is disabled", {
  data <- list(t = 30, seeding_time = 7, horizon = 7, future_fixed = 0, fixed_from = 0)
  gp_data <- create_gp_data(NULL, data)
  expect_equal(gp_data$fixed, 1)
  expect_equal(gp_data$stationary, 1)
  expect_equal(gp_data$M, 5) # (30 - 7) * 0.2
  expect_equal(gp_data$L, 1.5)
  expect_equal(gp_data$gp_type, 2) # Default to Matern
  expect_equal(gp_data$nu, 3 / 2)
  expect_equal(gp_data$w0, 1.0)
})

test_that("create_gp_data sets correct gp_type and nu for different kernels", {
  data <- list(t = 30, seeding_time = 7, horizon = 7, future_fixed = 0, fixed_from = 0, stationary = 0)

  gp <- gp_opts(kernel = "se")
  gp_data <- create_gp_data(gp, data)
  expect_equal(gp_data$gp_type, 0)
  expect_equal(gp_data$nu, Inf)

  gp <- gp_opts(kernel = "periodic")
  gp_data <- create_gp_data(gp, data)
  expect_equal(gp_data$gp_type, 1)
  expect_equal(gp_data$nu, 3 / 2) # Default Matern order
  expect_equal(gp_data$w0, 1.0)

  gp <- gp_opts(kernel = "ou")
  gp_data <- create_gp_data(gp, data)
  expect_equal(gp_data$gp_type, 2)
  expect_equal(gp_data$nu, 1 / 2)
})

test_that("create_gp_data correctly handles future_fixed", {
  data <- list(t = 30, seeding_time = 7, horizon = 7, future_fixed = 1, fixed_from = 2, stationary = 0)
  gp_data <- create_gp_data(gp_opts(), data)
  expect_equal(gp_data$M, 4)
})
