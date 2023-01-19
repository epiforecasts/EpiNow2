skip_on_cran()
skip_on_os("windows")

# test primary reports and observations
reports <- rep(10, 20)
obs <- rep(4, 20)
delay_pmf <- discretised_pmf(log(3), 0.1, 5, 0, 0)

check_equal <- function(args, target, dof = 0, dev = FALSE) {
  out <- do.call(calculate_secondary, args)
  out <- round(out, dof)
  if (dev) {
    return(out)
  }
  expect_equal(out, target)
}

test_that("calculate_secondary can calculate prevalence as expected", {
  check_equal(
    args = list(reports, obs, 0.1, delay_pmf, 1, 1, 1, 1, 1, 20),
    target = c(1, 5, 5.5, rep(6, 17)), dof = 1
  )
})

test_that("calculate_secondary can calculate incidence as expected", {
  check_equal(
    args = list(reports, obs, 0.1, delay_pmf, 0, 1, 1, 1, 1, 20),
    target = c(1, 1, 1.5, rep(2.0, 17)), dof = 1
  )
})

test_that("calculate_secondary can calculate incidence as expected", {
  check_equal(
    args = list(reports, obs, 0.1, delay_pmf, 0, 1, 1, 1, 1, 20),
    target = c(1, 1, 1.5, rep(2.0, 17)), dof = 1
  )
})

test_that("calculate_secondary can calculate incidence using only historic reports", {
  check_equal(
    args = list(reports, obs, 0.1, delay_pmf, 0, 1, 1, 0, 1, 20),
    target = c(0, 0, rep(1, 18)), dof = 0
  )
})

test_that("calculate_secondary can calculate incidence using only current reports", {
  check_equal(
    args = list(reports, obs, 0.1, delay_pmf, 0, 0, 1, 1, 1, 20),
    target = rep(1, 20), dof = 0
  )
})

test_that("calculate_secondary can switch into prediction mode as expected", {
  check_equal(
    args = list(reports, obs, 0.1, delay_pmf, 1, 0, 1, 1, 1, 20),
    target = c(1, rep(5, 19)), dof = 0
  )
  check_equal(
    args = list(reports, obs, 0.1, delay_pmf, 1, 0, 1, 1, 1, 10),
    target = c(1, rep(5, 9), 6:15), dof = 0
  )
})
