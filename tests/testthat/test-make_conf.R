# Test make_conf function

test_that("make_conf formats credible intervals correctly", {
  value <- list(median = 2, lower_90 = 1, upper_90 = 3)
  result <- make_conf(value)

  expect_equal(result, "2 (1 -- 3)")
})

test_that("make_conf handles different CrI levels", {
  value <- list(
    median = 5,
    lower_90 = 3, upper_90 = 7,
    lower_50 = 4, upper_50 = 6
  )

  result_90 <- make_conf(value, CrI = 90)
  expect_equal(result_90, "5 (3 -- 7)")

  result_50 <- make_conf(value, CrI = 50)
  expect_equal(result_50, "5 (4 -- 6)")
})

test_that("make_conf reverse option swaps bounds", {
  value <- list(median = 2, lower_90 = 1, upper_90 = 3)

  result_normal <- make_conf(value, reverse = FALSE)
  expect_equal(result_normal, "2 (1 -- 3)")

  result_reversed <- make_conf(value, reverse = TRUE)
  expect_equal(result_reversed, "2 (3 -- 1)")
})

test_that("make_conf handles decimal values", {
  value <- list(median = 1.23, lower_90 = 0.98, upper_90 = 1.56)
  result <- make_conf(value)

  expect_equal(result, "1.23 (0.98 -- 1.56)")
})

test_that("make_conf handles negative values", {
  value <- list(median = -0.5, lower_90 = -1.2, upper_90 = 0.1)
  result <- make_conf(value)

  expect_equal(result, "-0.5 (-1.2 -- 0.1)")
})

test_that("make_conf handles integer values", {
  value <- list(median = 100L, lower_90 = 80L, upper_90 = 120L)
  result <- make_conf(value)

  expect_equal(result, "100 (80 -- 120)")
})
