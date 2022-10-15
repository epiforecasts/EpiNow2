samples <- data.frame(value = 1:10, type = "car")

test_that("calc_CrI works as expected with default arguments", {
  expect_snapshot_output(calc_CrI(samples))
})

test_that("calc_CrI works as expected when grouping", {
  expect_snapshot_output(calc_CrI(samples, summarise_by = "type"))
})
