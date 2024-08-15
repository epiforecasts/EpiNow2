test_that("gp_opts returns correct default values", {
  gp <- gp_opts()
  expect_equal(gp$basis_prop, 0.2)
  expect_equal(gp$boundary_scale, 1.5)
  expect_equal(gp$ls_mean, 21)
  expect_equal(gp$ls_sd, 7)
  expect_equal(gp$ls_min, 0)
  expect_equal(gp$ls_max, 60)
  expect_equal(gp$alpha_sd, 0.01)
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

test_that("gp_opts handles deprecated matern_type parameter", {
  lifecycle::expect_deprecated(gp_opts(matern_type = 5 / 2))
  gp <- gp_opts(matern_type = 5 / 2)
  expect_equal(gp$matern_order, 5 / 2)
})

test_that("gp_opts stops for incompatible matern_order and matern_type", {
  expect_error(
    gp_opts(matern_order = 3 / 2, matern_type = 5 / 2),
    "Incompatible `matern_order` and `matern_type`"
  )
})

test_that("gp_opts warns about uncommon Matern kernel orders", {
  expect_warning(gp_opts(matern_order = 2), "Uncommon Matern kernel order")
})

test_that("gp_opts handles linear kernel correctly", {
  expect_silent(gp_opts(kernel = "linear"))
  expect_warning(
    gp_opts(kernel = "linear", ls_mean = 30),
    "Length scale parameters are not used for the linear kernel"
  )
  expect_warning(
    gp_opts(kernel = "linear", ls_sd = 10),
    "Length scale parameters are not used for the linear kernel"
  )
  expect_warning(
    gp_opts(kernel = "linear", ls_min = 1),
    "Length scale parameters are not used for the linear kernel"
  )
  expect_warning(
    gp_opts(kernel = "linear", ls_max = 100),
    "Length scale parameters are not used for the linear kernel"
  )
  
  linear_gp <- gp_opts(kernel = "linear")
  expect_true(all(c("ls_mean", "ls_sd", "ls_min", "ls_max") %in% names(linear_gp)))

  expect_equal(linear_gp$kernel, "linear")
})