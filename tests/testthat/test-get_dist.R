test_that("get_dist is deprecated", {
  data <- data.table::data.table(mean = 1, mean_sd = 1, sd = 1, sd_sd = 1, source = "test", disease = "test", dist = "lognormal")
  expect_deprecated(
    get_dist(data, disease = "test", source = "test")
  )
})
