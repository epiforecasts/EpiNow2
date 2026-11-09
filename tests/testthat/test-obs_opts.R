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
