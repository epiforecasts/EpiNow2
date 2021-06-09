samples <- data.frame(value = 1:10, type = "car")

test_that("calc_CrI works as expected with default arguments", {
  expect_known_output(calc_CrI(samples),
    file = testthat::test_path("test-data/calc_CrI_default.rds")
  )
})

test_that("calc_CrI works as expected when grouping", {
  expect_known_output(calc_CrI(samples, summarise_by = "type"),
    file = testthat::test_path("test-data/calc_CrI_grouping.rds")
  )
})
