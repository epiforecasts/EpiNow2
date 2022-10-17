samples <- data.frame(value = 1:10, type = "car")

test_that("calc_CrI works as expected with default arguments", {
  expect_snapshot_output(calc_CrIs(samples))
})

test_that("calc_CrI works as expected when grouping", {
  expect_snapshot_output(calc_CrIs(samples, summarise_by = "type"))
})


test_that("calc_CrI works as expected when given a custom CrI list", {
  expect_snapshot_output(calc_CrIs(samples, CrIs = c(0.1, 0.4, 0.95)))
})
