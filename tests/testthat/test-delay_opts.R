test_that("gt_opts warns and uses a fixed 1 day delay if dist is missing", {
  expect_warning(gt <- gt_opts(), "No generation time distribution given")
  expect_s3_class(gt, "generation_time_opts")
  expect_equal(get_distribution(gt), "fixed")
  expect_equal(max(gt), 1)
  expect_silent(gt_opts(Fixed(1)))
})

test_that("gt_opts, delay_opts and trunc_opts set class and prior weight", {
  dist <- LogNormal(mean = 3, sd = 1, max = 10)
  gt <- gt_opts(dist)
  expect_s3_class(gt, "generation_time_opts")
  expect_s3_class(gt, "dist_spec")
  expect_true(attr(gt, "weight_prior"))
  delay <- delay_opts(dist, weight_prior = FALSE)
  expect_s3_class(delay, "delay_opts")
  expect_s3_class(delay, "dist_spec")
  expect_false(attr(delay, "weight_prior"))
  trunc <- trunc_opts(dist)
  expect_s3_class(trunc, "trunc_opts")
  expect_s3_class(trunc, "dist_spec")
  expect_false(attr(trunc, "weight_prior"))
  expect_identical(generation_time_opts, gt_opts)
})

test_that("gt_opts and trunc_opts map default_cdf_cutoff to default_cdf_max", {
  withr::local_options(lifecycle_verbosity = "warning")
  dist <- LogNormal(mean = 3, sd = 1)
  expect_warning(
    gt <- suppressMessages(gt_opts(dist, default_cdf_cutoff = 0.01)),
    class = "lifecycle_warning_deprecated"
  )
  expect_equal(attr(gt, "cdf_max"), 0.99)
  expect_warning(
    trunc <- suppressMessages(trunc_opts(dist, default_cdf_cutoff = 0.01)),
    class = "lifecycle_warning_deprecated"
  )
  expect_equal(attr(trunc, "cdf_max"), 0.99)
  expect_equal(
    max(trunc),
    max(suppressMessages(trunc_opts(dist, default_cdf_max = 0.99)))
  )
})

test_that("unconstrained distributions are bounded at the default CDF level", {
  dist <- LogNormal(mean = 3, sd = 1)
  expect_message(
    delay <- delay_opts(dist, default_cdf_max = 0.9),
    "Constraining with default CDF level 0.9"
  )
  expect_equal(attr(delay, "cdf_max"), 0.9)
  # the maximum is the smallest integer delay with at least 90% of the mass
  expect_equal(max(delay), ceiling(qlnorm(0.9, get_parameters(dist)$meanlog,
    get_parameters(dist)$sdlog
  )))
  withr::local_options(EpiNow2.cdf_max = 0.5)
  delay <- suppressMessages(delay_opts(dist))
  expect_equal(attr(delay, "cdf_max"), 0.5)
})

test_that("a given default CDF level is ignored for constrained delays", {
  dist <- LogNormal(mean = 3, sd = 1, max = 10)
  expect_warning(
    delay <- delay_opts(dist, default_cdf_max = 0.9),
    "Ignoring given default CDF level"
  )
  expect_equal(max(delay), 10)
  expect_warning(
    gt_opts(dist, default_cdf_max = 0.9), "Ignoring given default CDF level"
  )
  expect_silent(delay_opts(dist))
})

test_that("delay_opts and trunc_opts error for bad 'dist' specifications", {
  expect_error(delay_opts(list(mean = 3)), "dist_spec")
  expect_error(trunc_opts(3), "dist_spec")
  expect_error(trunc_opts(Normal(mean = 3, sd = 1, max = 10)), "lognormal")
})
