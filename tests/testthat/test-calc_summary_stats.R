context("calc_summary_stats")

samples <- data.frame(value = 1:10, type = "car")

test_that("calc_summary_stats works as expected with default arguments", {
  expect_known_output(calc_summary_stats(samples),
    file = testthat::test_path("test-data/calc_summary_stats_default.rds")
  )
})

test_that("calc_summary_stats works as expected when grouping", {
  expect_known_output(calc_summary_stats(samples, summarise_by = "type"),
    file = testthat::test_path("test-data/calc_summary_stats_grouping.rds")
  )
})
