samples <- data.frame(value = 1:10, type = "car")

test_that("calc_summary_stats works as expected with default arguments", {
  expect_snapshot_output(calc_summary_stats(samples))
})

test_that("calc_summary_stats works as expected when grouping", {
  expect_snapshot_output(calc_summary_stats(samples, summarise_by = "type"))
})
