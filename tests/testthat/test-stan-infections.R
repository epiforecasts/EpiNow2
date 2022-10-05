skip_on_cran()

# test update_infectiousness
test_that("update_infectiousness works as expected with default settings", {
  expect_equal(
    update_infectiousness(rep(1, 20), rep(0.1, 10), 5, 10),
    1
  )
  expect_equal(
    update_infectiousness(rep(1, 20), rep(0.1, 5), 5, 10),
    0.5
  )
  expect_error(update_infectiousness(rep(1, 20), rep(0.1, 5), 5, 10, 10))
})

gt_pmf <- discretised_pmf(3, 2, 15, 1)

# test generate infections
test_that("generate_infections works as expected", {
  expect_equal(
    round(generate_infections(c(1, rep(1, 9)), 10, gt_pmf, log(1000), 0, 0, 0), 0),
    c(rep(1000, 10), 993, rep(996, 9))
  )
  expect_equal(
    round(generate_infections(c(1, rep(1.1, 9)), 10, gt_pmf, log(20), 0.03, 0, 0), 0),
    c(20, 21, 21, 22, 23, 23, 24, 25, 25, 26, 24, 27, 28, 28, 29, 30, 31, 32, 32, 33)
  )
  expect_equal(
    round(generate_infections(c(1, rep(1.1, 9)), 10, gt_pmf, log(100), 0, 0, 0), 0),
    c(rep(100, 10), 99, 110, 111, 114, 117, 120, 124, 127, 131, 134)
  )
  expect_equal(
    round(generate_infections(c(1, rep(1, 9)), 4, gt_pmf, log(500), -0.02, 0, 0), 0),
    c(500, 490, 480, 471, 364, 397, 402, 403, rep(402, 4), rep(403, 2))
  )
  expect_equal(
    round(generate_infections(c(1, rep(1.1, 9)), 4, gt_pmf, log(500), 0, 0, 0), 0),
    c(rep(500, 4), 375, 452, 464, 477, 490, 504, 518, 533, 547, 563)
  )
  expect_equal(
    round(generate_infections(c(1, rep(1, 9)), 1, gt_pmf, log(40), numeric(0), 0, 0), 0),
    c(40, 5, 10, rep(11, 8))
  )
  expect_equal(
    round(generate_infections(c(1, rep(1.1, 9)), 1, gt_pmf, log(100), 0.01, 0, 0), 0),
    c(100, 12, 28, 32, rep(33, 2), 34, 35, 36, 37, 38)
  )
  expect_equal(
    round(generate_infections(c(1, rep(1, 9)), 10, gt_pmf, log(1000), 0, 100000, 4), 0),
    c(rep(1000, 10), 993, rep(996, 5), 979, 965, 949, 929)
  )
})
