samples <- data.frame(value = 1:10, type = "car")

test_that("calc_CrI works as expected with default arguments", {
  expect_known_output(calc_CrIs(samples),
    file = testthat::test_path("test-data/calc_CrIs_default.rds")
  )
})

test_that("calc_CrI works as expected when grouping", {
  expect_known_output(calc_CrIs(samples, summarise_by = "type"),
    file = testthat::test_path("test-data/calc_CrIs_grouping.rds")
  )
})


test_that("calc_CrI works as expected when given a custom CrI list", {
  expect_known_output(calc_CrIs(samples, CrIs = c(0.1, 0.4, 0.95)),
    file = testthat::test_path("test-data/calc_CrIs_custom_CrI.rds")
  )
})
