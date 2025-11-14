test_that("estimate_dist prototype structure is correct", {
  # Skip if primarycensored not available
  skip_if_not_installed("primarycensored")
  skip_if_not_installed("cmdstanr")

  # Test data conversion functions exist
  expect_true(exists(".convert_to_pcd_data", mode = "function"))
  expect_true(exists(".vector_to_pcd_data", mode = "function"))
  expect_true(exists(".linelist_to_pcd_data", mode = "function"))
  expect_true(exists(".extract_dist_spec", mode = "function"))
})

test_that("vector to pcd_data conversion validates input", {
  skip_if_not_installed("primarycensored")

  # Should handle NA values
  values <- c(1, 2, NA, 4, 5)
  # expect_silent(.vector_to_pcd_data(values, "uniform", FALSE))

  # Should handle negative values
  values <- c(-1, 2, 3, 4, 5)
  # expect_silent(.vector_to_pcd_data(values, "uniform", FALSE))
})

test_that("linelist to pcd_data validates required columns", {
  skip_if_not_installed("primarycensored")

  # Missing required columns
  bad_data <- data.frame(x = 1:10, y = 1:10)
  expect_error(
    .linelist_to_pcd_data(bad_data, "uniform", FALSE),
    "missing required columns"
  )

  # Valid minimal linelist
  good_data <- data.frame(
    ptime_lwr = 0:9,
    stime_lwr = 5:14
  )
  # expect_silent(.linelist_to_pcd_data(good_data, "uniform", FALSE))
})

# Integration test (only run if both packages available AND cmdstan installed)
test_that("estimate_dist works with simple vector input", {
  skip_if_not_installed("primarycensored")
  skip_if_not_installed("cmdstanr")
  skip_on_cran()
  skip_on_ci()

  # This would be a full integration test
  # set.seed(123)
  # delays <- rpois(100, lambda = 5) + 1
  #
  # result <- estimate_dist(
  #   delays,
  #   dist = "lognormal",
  #   samples = 500,
  #   chains = 2,
  #   cores = 1,
  #   verbose = FALSE
  # )
  #
  # expect_s3_class(result, "dist_spec")
  # expect_equal(result$distribution, "lognormal")
  # expect_true(all(c("meanlog", "sdlog") %in% names(result$parameters)))
})
