# Test secondary_opts function

test_that("secondary_opts returns incidence settings by default", {
  result <- secondary_opts("incidence")

  expect_s3_class(result, "secondary_opts")
  expect_equal(result$cumulative, 0)
  expect_equal(result$historic, 1)
  expect_equal(result$primary_hist_additive, 1)
  expect_equal(result$current, 0)
  expect_equal(result$primary_current_additive, 0)
})

test_that("secondary_opts returns prevalence settings", {
  result <- secondary_opts("prevalence")

  expect_s3_class(result, "secondary_opts")
  expect_equal(result$cumulative, 1)
  expect_equal(result$historic, 1)
  expect_equal(result$primary_hist_additive, 0)
  expect_equal(result$current, 1)
  expect_equal(result$primary_current_additive, 1)
})

test_that("secondary_opts allows overriding defaults", {
  result <- secondary_opts("incidence", cumulative = 1, current = 1)

  expect_equal(result$cumulative, 1)
  expect_equal(result$current, 1)
  # Other values should remain as incidence defaults
  expect_equal(result$historic, 1)
})

test_that("secondary_opts errors on invalid type", {
  expect_error(secondary_opts("invalid"))
})
