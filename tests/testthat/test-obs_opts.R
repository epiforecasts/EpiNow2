test_that("obs_opts returns expected default values", {
  result <- suppressWarnings(obs_opts())
  
  expect_s3_class(result, "obs_opts")
  expect_equal(result$family, "negbin")
  expect_equal(result$weight, 1)
  expect_true(result$week_effect)
  expect_equal(result$week_length, 7L)
  expect_equal(result$scale, list(mean = 1, sd = 0))
  expect_equal(result$accumulate, 0)
  expect_true(result$likelihood)
  expect_false(result$return_likelihood)
})

test_that("obs_opts returns expected messages", {
  expect_message(
    obs_opts(),
    "NA values will be treated as missing"
  )
  expect_message(
    obs_opts(na = "accumulate"),
    "modelled values that correspond to NA values"
  )
})
