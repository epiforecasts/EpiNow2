test_that("get_dist returns distributional definition data in the format expected by EpiNow2", {
  data <- data.table::data.table(mean = 1, mean_sd = 1, sd = 1, sd_sd = 1, source = "test", disease = "test", dist = "gamma")
  dist <- get_dist(data, disease = "test", source = "test")
  expect_equal(
    dist[c("mean_mean", "mean_sd", "sd_mean", "sd_sd", "max")],
    list(mean_mean = array(1), mean_sd = array(1),
         sd_mean = array(1), sd_sd = array(1), max = array(15L))
  )
})

test_that("get_dist returns distributional definition data in the format expected by EpiNow2 with no uncertainty", {
  data <- data.table::data.table(mean = 1, mean_sd = 1, sd = 1, sd_sd = 1, source = "test", disease = "test", dist = "lognormal")
  dist <- get_dist(data, disease = "test", source = "test", fixed = TRUE)
  expect_equal(
    round(dist$np_pmf[1:7], digits = 2),
    array(c(0.17, 0.23, 0.17, 0.12, 0.08, 0.06, 0.04))
  )
})
