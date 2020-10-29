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

# test initialise infections
test_that("initialise_infections works as expected", {
  skip_on_cran()
  expect_equal(initialise_infections(20, 20, rep(0.1, 5), 5, 10),
               c(rep(1e-05, 5), rep(2e+02 + 1e-5, 5), rep(1e-05, 10)))
  expect_equal(initialise_infections(20, 20, rep(0.1, 5), 10, 5),
               c(rep(2e+02 + 1e-5, 5), rep(1e-05, 15)))
  expect_equal(round(
    initialise_infections(20, 20, c(0.01, 0.02, 0.17, 0.4, 0.4), 10, 5), 0),
               c(2000, 1000, 118, 50, 50, rep(0, 15)))
})

