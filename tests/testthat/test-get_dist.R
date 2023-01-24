test_that("get_dist returns distributional definition data in the format expected by EpiNow2", {
  data <- data.table::data.table(mean = 1, mean_sd = 1, sd = 1, sd_sd = 1, source = "test", disease = "test", dist = "gamma")

  expect_equal(
    get_dist(data, disease = "test", source = "test"),
    list(mean = 1, mean_sd = 1, sd = 1, sd_sd = 1, max = 15, dist = "gamma")
  )
})

test_that("get_dist returns distributional definition data in the format expected by EpiNow2 with no uncertainty", {
  data <- data.table::data.table(mean = 1, mean_sd = 1, sd = 1, sd_sd = 1, source = "test", disease = "test", dist = "lognormal")

  expect_equal(
    get_dist(data, disease = "test", source = "test", fixed = TRUE),
    list(mean = 1, mean_sd = 0, sd = 1, sd_sd = 0, max = 15, dist = "lognormal")
  )
})
