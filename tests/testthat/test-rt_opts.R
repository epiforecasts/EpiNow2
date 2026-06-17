test_that("rt_opts returns expected default values", {
  result <- rt_opts()

  expect_s3_class(result, "rt_opts")
  # the default Rt prior is a first-difference Gaussian process
  expect_s3_class(result$prior, "state_spec")
  expect_identical(result$prior$type, "gp")
  expect_identical(result$prior$anchor, "init")
  expect_true(result$use_rt)
  expect_equal(result$rw, 0)
  expect_equal(result$future, "latest")
  expect_equal(result$pop, Fixed(0))
  expect_equal(result$gp_on, "R_t-1")
})

test_that("rt_opts accepts a constant or time-varying Rt prior", {
  expect_s3_class(rt_opts(prior = LogNormal(mean = 1, sd = 1))$prior, "dist_spec")
  expect_s3_class(rt_opts(prior = RW(init = LogNormal(1, 1)))$prior, "rw_state")
})

test_that("rt_opts handles custom inputs correctly", {
  result <- suppressWarnings(rt_opts(
    prior = LogNormal(mean = 2, sd = 0.5),
    use_rt = FALSE,
    future = "project",
    pop = Normal(mean = 1000000, sd = 100)
  ))

  expect_null(result$prior)
  expect_false(result$use_rt)
  expect_equal(result$future, "project")
  expect_equal(result$pop, Normal(mean = 1000000, sd = 100))
})

test_that("rt_opts errors when pop is passed as numeric", {
  expect_error(
    rt_opts(pop = 1000),
    "must be a `<dist_spec>`"
  )
})

test_that("the rw argument is deprecated and ignored", {
  expect_warning(result <- rt_opts(rw = 7), "deprecated")
  expect_equal(result$rw, 0)
})

test_that("gp_on sets the anchor of the default Rt prior", {
  expect_identical(rt_opts()$prior$anchor, "init") # R_t-1 default
  expect_identical(rt_opts(gp_on = "R0")$prior$anchor, "mean")
  # an explicit prior overrides gp_on
  expect_identical(
    rt_opts(prior = GP(init = LogNormal(1, 1)), gp_on = "R0")$prior$anchor,
    "init"
  )
})

test_that("rt_opts validates gp_on argument", {
  expect_error(rt_opts(gp_on = "invalid"), "must be one")
})

test_that("rt_opts returns object of correct class", {
  result <- rt_opts()
  expect_s3_class(result, "rt_opts")
  expect_true("list" %in% class(result))
})
