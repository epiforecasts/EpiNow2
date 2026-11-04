test_that("obs_opts returns expected default values", {
  result <- suppressWarnings(obs_opts())

  expect_s3_class(result, "obs_opts")
  expect_equal(result$family, "negbin")
  expect_equal(result$weight, 1)
  expect_true(result$week_effect)
  expect_equal(result$week_length, 7L)
  expect_equal(result$scale, Normal(mean = 1, sd = 0))
  expect_true(result$likelihood)
  expect_false(result$return_likelihood)
  expect_equal(result$type, "incidence")
})

test_that("obs_opts can be used with prevalence-type observations", {
  result <- suppressWarnings(obs_opts(type = "prevalence"))
  expect_equal(result$type, "prevalence")
  expect_null(result$duration)
})

test_that("obs_opts accepts a duration distribution for prevalence-type
          observations", {
  duration <- LogNormal(mean = 5, sd = 2, max = 20)
  result <- suppressWarnings(
    obs_opts(type = "prevalence", duration = duration)
  )
  expect_equal(result$duration, duration)
})

test_that("obs_opts drops duration, with a warning, when type is not
          prevalence", {
  expect_warning(
    result <- obs_opts(duration = LogNormal(mean = 5, sd = 2, max = 20)),
    "duration"
  )
  expect_null(result$duration)
})

test_that("obs_opts errors if duration is not a <dist_spec>", {
  expect_error(
    obs_opts(type = "prevalence", duration = 5), "dist_spec"
  )
})

test_that("obs_opts errors with an invalid type", {
  expect_error(obs_opts(type = "cumulative"))
})

test_that("obs_opts drops dispersion for the poisson family", {
  expect_silent(obs <- obs_opts(family = "poisson"))
  expect_equal(obs$family, "poisson")
  expect_null(obs$dispersion)
  expect_warning(
    obs <- obs_opts(family = "poisson", dispersion = Normal(0, 0.1)),
    "dispersion"
  )
  expect_null(obs$dispersion)
})

test_that("obs_opts errors for bad specifications", {
  expect_error(obs_opts(family = "binomial"), "family")
  expect_error(obs_opts(scale = 0.5), "dist_spec")
  expect_error(obs_opts(dispersion = 0.1), "dist_spec")
})
