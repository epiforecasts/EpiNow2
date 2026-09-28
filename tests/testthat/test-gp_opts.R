test_that("gp_opts returns correct default values", {
  gp <- gp_opts()
  expect_equal(gp$basis_prop, 0.2)
  expect_equal(gp$boundary_scale, 1.5)
  expect_equal(gp$alpha, Normal(0, 0.01))
  expect_equal(gp$ls, LogNormal(mean = 21, sd = 7, max = 60))
  expect_equal(gp$kernel, "matern")
  expect_equal(gp$matern_order, 3 / 2)
  expect_equal(gp$w0, 1.0)
})

test_that("gp_opts sets matern_order to Inf for squared exponential kernel", {
  gp <- gp_opts(kernel = "se")
  expect_equal(gp$matern_order, Inf)
})

test_that("gp_opts sets matern_order to 1/2 for Ornstein-Uhlenbeck kernel", {
  gp <- gp_opts(kernel = "ou")
  expect_equal(gp$matern_order, 1 / 2)
})

test_that("gp_opts warns for uncommon Matern kernel orders", {
  expect_warning(gp_opts(matern_order = 2), "Uncommon Matern kernel order")
})

test_that("gp_opts warns about uncommon Matern kernel orders", {
  expect_warning(gp_opts(matern_order = 2), "Uncommon Matern kernel order")
})

test_that("gp_opts flags whether alpha was left at its default", {
  expect_true(attr(gp_opts(), "alpha_default"))
  expect_true(attr(gp_opts(kernel = "periodic"), "alpha_default"))
  expect_false(
    attr(gp_opts(alpha = Normal(mean = 0, sd = 0.01)), "alpha_default")
  )
})

test_that("apply_default_gp_alpha widens alpha for the nonmechanistic model", {
  widened <- apply_default_gp_alpha(gp_opts(), rt = NULL)
  expect_equal(widened$alpha, Normal(mean = 0, sd = 0.05))
})

test_that("apply_default_gp_alpha leaves alpha unchanged for renewal model", {
  unchanged <- apply_default_gp_alpha(gp_opts(), rt = rt_opts())
  expect_equal(unchanged$alpha, Normal(mean = 0, sd = 0.01))
})

test_that("apply_default_gp_alpha respects a user-specified alpha", {
  custom <- gp_opts(alpha = Normal(mean = 0, sd = 0.2))
  unchanged <- apply_default_gp_alpha(custom, rt = NULL)
  expect_equal(unchanged$alpha, Normal(mean = 0, sd = 0.2))
})

test_that("apply_default_gp_alpha leaves a disabled Gaussian process alone", {
  expect_null(apply_default_gp_alpha(NULL, rt = NULL))
})
