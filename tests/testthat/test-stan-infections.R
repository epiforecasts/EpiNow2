skip_on_cran()
files <- c("pmfs.stan", "infections.stan")
suppressMessages(expose_stan_fns(files, target_dir = system.file("stan/functions", package = "EpiNow2")))


# test update_infectiousness
test_that("update_infectiousness works as expected with default settings", {
  expect_equal(
    update_infectiousness(rep(1, 20), rep(0.1, 10), 5, 10, 10),
    1
  )
  expect_equal(
    update_infectiousness(rep(1, 20), rep(0.1, 5), 5, 5, 10),
    0.5
  )
  expect_error(update_infectiousness(rep(1, 20), rep(0.1, 5), 5, 10, 10))
})


# test generate infections
test_that("generate_infections works as expected", {
  expect_equal(
    round(generate_infections(c(1, rep(1, 9)), 10, 3, 2, 15, log(1000), 0, 0, 0), 0),
    c(rep(1000, 10), 996, rep(997, 3), rep(998, 6))
  )
  expect_equal(
    round(generate_infections(c(1, rep(1.1, 9)), 10, 3, 2, 15, log(20), 0.03, 0, 0), 0),
    c(20, 21, 21, 22, 23, 23, 24, 25, 25, 26, 25, 27, 28, 29, 30, 31, 32, 33, 35, 36)
  )
  expect_equal(
    round(generate_infections(c(1, rep(1.1, 9)), 10, 3, 2, 15, log(100), 0, 0, 0), 0),
    c(rep(100, 11), 110, 113, 116, 120, 125, 129, 133, 138, 143)
  )
  expect_equal(
    round(generate_infections(c(1, rep(1, 9)), 4, 3, 2, 15, log(500), -0.02, 0, 0), 0),
    c(500, 490, 480, 471, 402, 413, 417, 417, rep(418, 6))
  )
  expect_equal(
    round(generate_infections(c(1, rep(1.1, 9)), 4, 3, 2, 15, log(500), 0, 0, 0), 0),
    c(rep(500, 4), 416, 472, 490, 507, 524, 543, 562, 581, 601, 622)
  )
  expect_equal(
    round(generate_infections(c(1, rep(1, 9)), 1, 3, 2, 15, log(40), numeric(0), 0, 0), 0),
    c(40, 11, 13, rep(14, 8))
  )
  expect_equal(
    round(generate_infections(c(1, rep(1.1, 9)), 1, 3, 2, 15, log(100), 0.01, 0, 0), 0),
    c(100, 28, 36, 39, 41, 42, 44, 45, 47, 48, 50)
  )
  expect_equal(
    round(generate_infections(c(1, rep(1, 9)), 10, 3, 2, 15, log(1000), 0, 100000, 4), 0),
    c(rep(1000, 10), 996, 997, 997, 997, 998, 998, 980, 964, 945, 922)
  )
})
