test_that("secondary_opts returns expected default values for incidence", {
  secondary <- secondary_opts()
  expect_s3_class(secondary, "secondary_opts")
  expect_equal(secondary$cumulative, 0)
  expect_equal(secondary$historic, 1)
  expect_equal(secondary$primary_hist_additive, 1)
  expect_equal(secondary$current, 0)
  expect_equal(secondary$primary_current_additive, 0)
})

test_that("secondary_opts returns expected values for prevalence", {
  secondary <- secondary_opts(type = "prevalence")
  expect_equal(secondary$cumulative, 1)
  expect_equal(secondary$historic, 1)
  expect_equal(secondary$primary_hist_additive, 0)
  expect_equal(secondary$current, 1)
  expect_equal(secondary$primary_current_additive, 1)
})

test_that("secondary_opts warns when overriding low-level options", {
  expect_warning(
    secondary_opts(cumulative = 1),
    "deprecated"
  )
  expect_warning(
    secondary_opts(historic = 0, current = 1),
    "deprecated"
  )
  expect_no_warning(secondary_opts())
  expect_no_warning(secondary_opts(type = "prevalence"))
})

test_that("secondary_opts still applies overridden low-level options", {
  secondary <- suppressWarnings(secondary_opts(cumulative = 1))
  expect_equal(secondary$cumulative, 1)
})
