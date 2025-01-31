test_that("obs_opts returns expected default values", {
  result <- suppressWarnings(obs_opts())

  expect_s3_class(result, "obs_opts")
  expect_equal(result$family, "negbin")
  expect_equal(result$weight, 1)
  expect_true(result$week_effect)
  expect_equal(result$week_length, 7L)
  expect_equal(result$scale, Normal(mean = 1, sd = 0))
  expect_equal(result$accumulate, 0)
  expect_true(result$likelihood)
  expect_false(result$return_likelihood)
})

test_that("obs_opts returns expected messages", {
  # The option na = "accumulate" informs the user of what is
  # going to be done once every 8 hours, so hard to test regularly.
  # NB: We change the local setting here to throw the message on demand, rather
  # than every 8 hours, for the sake of multiple runs of the test within
  # 8 hours.
  suppressMessages(expect_deprecated(obs_opts(na = "accumulate")))
})

test_that("obs_opts behaves as expected for user specified na treatment", {
  # If user explicitly specifies NA as missing, then don't throw message
  expect_false(
    suppressWarnings(obs_opts(na = "missing"))$na_as_missing_default_used
  )
})
