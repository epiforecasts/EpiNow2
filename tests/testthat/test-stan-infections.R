context("estimate_infections")
if (!testthat:::on_cran()) {
  files <- c("pmfs.stan", "infections.stan")
  suppressMessages(expose_stan_fns(files, target_dir = system.file("stan/functions", package = "EpiNow2")))
}


# test update_infectiousness
test_that("update_infectiousness works as expected with default settings", {
  skip_on_cran()
  expect_equal(update_infectiousness(rep(1, 20), rep(0.1, 10), 5, 10, 10),
               1)
  expect_equal(update_infectiousness(rep(1, 20), rep(0.1, 5), 5, 5, 10),
               0.5)
  expect_error(update_infectiousness(rep(1, 20), rep(0.1, 5), 5, 10, 10))
})


# test generate infections
test_that("generate_infections works as expected", {
  expect_equal(round(generate_infections(c(1, rep(1, 9)), 10, 3, 2, 15, 1000), 0),
               c(1000, 278, 330, 345, 349, 350, rep(351, 14)))
  expect_equal(round(generate_infections(c(1, rep(1.1, 9)), 10, 3, 2, 15, 1000), 0),
               c(1000, 278, 330, 345, 349, 350, 351, 351, 351, 351, 351, 386, 397, 
                 410, 424, 438, 453, 469, 486, 502))
  expect_equal(round(generate_infections(c(1, rep(1.1, 9)), 10, 3, 2, 15, 1000), 0),
               c(1000, 278, 330, 345, 349, 350, 351, 351, 351, 351, 351, 386, 397, 
                 410, 424, 438, 453, 469, 486, 502))
  expect_equal(round(generate_infections(c(1, rep(1, 9)), 4, 3, 2, 15, 1000), 0),
               c(1000, 278, 330, 345, 349, 350, rep(351, 8)))
  expect_equal(round(generate_infections(c(1, rep(1.1, 9)), 4, 3, 2, 15, 1000), 0),
               c(1000, 278, 330, 345, 349, 385, 396, 409, 423, 438, 453, 469, 485, 502))
  expect_equal(round(generate_infections(c(1, rep(1, 9)), 1, 3, 2, 15, 1000), 0),
               c(1000, 278, 330, 345, 349, 350, rep(351, 5)))
  expect_equal(round(generate_infections(c(1, rep(1.1, 9)), 1, 3, 2, 15, 1000), 0),
               c(1000, 278, 363, 390, 407, 422, 437, 452, 468, 484, 501))
})



round(generate_infections(c(1, rep(1.1, 9)), 1, 3, 2, 15, 1000), 0)