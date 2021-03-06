context("estimate_secondary")
if (!testthat:::on_cran()) {
  files <- c("pmfs.stan", "convolve.stan", "observation_model.stan", "secondary.stan")
  suppressMessages(
    expose_stan_fns(files,
      target_dir = system.file("stan/functions", package = "EpiNow2")
    )
  )
}

# test primary reports and observations
reports <- rep(10, 20)
obs <- rep(4, 20)

check_equal <- function(args, target, dof = 0, dev = FALSE) {
  out <- do.call(calculate_secondary, args)
  out <- round(out, dof)
  if (dev) {
    return(out)
  }
  expect_equal(out, target)
}

test_that("calculate_secondary can calculate prevalence as expected", {
  skip_on_cran()
  check_equal(
    args = list(reports, obs, 0.1, log(3), 0.1, 5, 1, 1, 1, 1, 1, 20),
    target = c(1, 5, 5.5, rep(6, 17)), dof = 1
  )
})

test_that("calculate_secondary can calculate incidence as expected", {
  skip_on_cran()
  check_equal(
    args = list(reports, obs, 0.1, log(3), 0.1, 5, 0, 1, 1, 1, 1, 20),
    target = c(1, 1, 1.5, rep(2.0, 17)), dof = 1
  )
})

test_that("calculate_secondary can calculate incidence as expected", {
  skip_on_cran()
  check_equal(
    args = list(reports, obs, 0.1, log(3), 0.1, 5, 0, 1, 1, 1, 1, 20),
    target = c(1, 1, 1.5, rep(2.0, 17)), dof = 1
  )
})

test_that("calculate_secondary can calculate incidence using only historic reports", {
  skip_on_cran()
  check_equal(
    args = list(reports, obs, 0.1, log(3), 0.1, 5, 0, 1, 1, 0, 1, 20),
    target = c(0, 0, rep(1, 18)), dof = 0
  )
})

test_that("calculate_secondary can calculate incidence using only current reports", {
  skip_on_cran()
  check_equal(
    args = list(reports, obs, 0.1, log(3), 0.1, 5, 0, 0, 1, 1, 1, 20),
    target = rep(1, 20), dof = 0
  )
})

test_that("calculate_secondary can switch into prediction mode as expected", {
  skip_on_cran()
  check_equal(
    args = list(reports, obs, 0.1, log(3), 0.1, 5, 1, 0, 1, 1, 1, 20),
    target = c(1, rep(5, 19)), dof = 0
  )
  check_equal(
    args = list(reports, obs, 0.1, log(3), 0.1, 5, 1, 0, 1, 1, 1, 10),
    target = c(1, rep(5, 9), 6:15), dof = 0
  )
})
