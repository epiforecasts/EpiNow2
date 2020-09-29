context("get_dist")


test_that("get_dist returns distributional definition data in the format expected by EpiNow2", {
  
  data <- data.table::data.table(mean = 1, mean_sd = 1, sd = 1, sd_sd = 1, source = "test", disease = "test")

  expect_equal(get_dist(data, disease = "test", source = "test"),
               list(mean= 1, mean_sd = 1, sd = 1, sd_sd = 1, max = 30))
})