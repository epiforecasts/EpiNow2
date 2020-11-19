context("estimate_secondary")
if (!testthat:::on_cran()) {
  files <- c("pmfs.stan", "convolve.stan", "observation_model.stan", "secondary.stan")
  suppressMessages(expose_stan_fns(files, target_dir = "inst/stan/functions")) #system.file("stan/functions", package = "EpiNow2")))
}


reports <- rep(10, 20)
obs <- rep(4, 20)

check_equal <- function(args, target, dof = 0, dev = FALSE) {
  out <- do.call(calculate_secondary, args)
  if (dev) {
    return(out)
  }
  expect_equal(round(out, dof), target)
}

test_that("calculate_secondary can calculate prevalence as expected", {
  check_equal(args = list(reports, obs, 0.1, log(3), 0.1, 5, 1, 1, 1, 1, 1, 20),
              target = c(1, 5, 5.5, rep(6, 17)), dof = 1)
})

test_that("calculate_secondary can calculate incidence as expected", {
  check_equal(args = list(reports, obs, 0.1, log(3), 0.1, 5, 0, 1, 1, 1, 1, 20),
              target = c(1, 1, 1.5, rep(2.0, 17)), dof = 1)
})

test_that("calculate_secondary can calculate incidence as expected", {
  check_equal(args = list(reports, obs, 0.1, log(3), 0.1, 5, 0, 1, 1, 1, 1, 20),
              target = c(1, 1, 1.5, rep(2.0, 17)), dof = 1)
})

# test only historic incidence
calculate_secondary(reports, obs, 0.1, log(3), 0.1, 5, 0, 1, 1, 0, 1, 20) 

# test only current incidence
calculate_secondary(reports, obs, 0.1, log(3), 0.1, 5, 0, 0, 1, 1, 1, 20) 

# test prediction switch
calculate_secondary(reports, obs, 0.1, log(3), 0.1, 5, 1, 0, 1, 1, 1, 20)
calculate_secondary(reports, obs, 0.1, log(3), 0.1, 5, 1, 0, 1, 1, 1, 10) 

