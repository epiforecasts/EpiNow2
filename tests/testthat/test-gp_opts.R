test_that("GP() builds correct default Gaussian process settings", {
  gp <- GP(init = LogNormal(mean = 1, sd = 1))$settings
  expect_equal(gp$basis_prop, 0.2)
  expect_equal(gp$boundary_scale, 1.5)
  expect_equal(gp$alpha, Normal(0, 0.01))
  expect_equal(gp$ls, LogNormal(mean = 21, sd = 7, max = 60))
  expect_equal(gp$kernel, "matern")
  expect_equal(gp$matern_order, 3 / 2)
  expect_equal(gp$w0, 1.0)
})

test_that("GP() sets matern_order to Inf for squared exponential kernel", {
  gp <- GP(init = LogNormal(mean = 1, sd = 1), kernel = "se")$settings
  expect_equal(gp$matern_order, Inf)
})

test_that("GP() sets matern_order to 1/2 for Ornstein-Uhlenbeck kernel", {
  gp <- GP(init = LogNormal(mean = 1, sd = 1), kernel = "ou")$settings
  expect_equal(gp$matern_order, 1 / 2)
})

test_that("GP() warns for uncommon Matern kernel orders", {
  expect_warning(
    GP(init = LogNormal(mean = 1, sd = 1), matern_order = 2),
    "Uncommon Matern kernel order"
  )
})

test_that("gp_opts() is deprecated in favour of GP()", {
  lifecycle::expect_deprecated(gp_opts())
})
