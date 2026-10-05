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
})

test_that("obs_opts errors with an invalid type", {
  expect_error(obs_opts(type = "cumulative"))
})
