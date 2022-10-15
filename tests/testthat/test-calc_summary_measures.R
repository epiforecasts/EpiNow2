samples <- data.frame(value = 1:10, type = "car")

test_that("calc_summary_measures works as expected with default arguments", {
  expect_snapshot_output(calc_summary_measures(samples),
    file = testthat::test_path("test-data/calc_summary_measures_default.rds")
  )
})

test_that("calc_CrI works as expected when grouping", {
  expect_snapshot_output(calc_summary_measures(samples, summarise_by = "type"),
    file = testthat::test_path("test-data/calc_summary_measures_grouping.rds")
  )
})

test_that("calc_CrI works as expected when given a custom CrI list", {
  expect_snapshot_output(calc_summary_measures(samples, CrIs = c(0.1, 0.4, 0.95)),
    file = testthat::test_path("test-data/calc_summary_measures_custom_CrI.rds")
  )
})
