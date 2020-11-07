context("create_future_rt")

test_that("create_future_rt works as expected", {
  test_frt <- function(test, true) {
    expect_equal(test$from, true$from)
    expect_equal(test$fixed, true$fixed)
  }
  
  test_frt(EpiNow2:::create_future_rt(), list(fixed = TRUE, from = 0))
  test_frt(EpiNow2:::create_future_rt("project"), list(fixed = FALSE, from = 0))
  test_frt(EpiNow2:::create_future_rt("est", 10), list(fixed = TRUE, from = -10))
  test_frt(EpiNow2:::create_future_rt(-3), list(fixed = TRUE, from = -3))
  test_frt(EpiNow2:::create_future_rt(3), list(fixed = TRUE, from = 3))
})