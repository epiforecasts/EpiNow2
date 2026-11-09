test_that("secondary_opts returns the incidence model by default", {
  opts <- secondary_opts()
  expect_s3_class(opts, "secondary_opts")
  expect_equal(
    unclass(opts),
    list(
      cumulative = 0, historic = 1, primary_hist_additive = 1,
      current = 0, primary_current_additive = 0
    )
  )
})

test_that("secondary_opts returns the prevalence model", {
  opts <- secondary_opts("prevalence")
  expect_s3_class(opts, "secondary_opts")
  expect_equal(
    unclass(opts),
    list(
      cumulative = 1, historic = 1, primary_hist_additive = 0,
      current = 1, primary_current_additive = 1
    )
  )
})

test_that("secondary_opts overrides options with those passed in ...", {
  opts <- secondary_opts("prevalence", cumulative = 0)
  expect_equal(opts$cumulative, 0)
  expect_equal(opts$current, 1)
})

test_that("secondary_opts errors for bad 'type' specifications", {
  expect_error(secondary_opts("cumulative"), "type")
})
