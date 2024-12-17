test_that("estimate_early_dynamics works", {
  cases <- EpiNow2::example_confirmed[1:30]
  prior_estimates <- estimate_early_dynamics(cases$confirm, 7)
  # Check dimensions
  expect_identical(
    names(prior_estimates),
    c("prior_infections", "prior_growth")
  )
  expect_identical(length(prior_estimates), 2L)
  # Check values
  expect_identical(
    round(prior_estimates$prior_infections, 2),
    4.53
  )
  expect_identical(
    round(prior_estimates$prior_growth, 2),
    0.35
  )
})

test_that("estimate_early_dynamics handles NA values correctly", {
  cases <- c(10, 20, NA, 40, 50, NA, 70)
  prior_estimates <- estimate_early_dynamics(cases, 7)
  expect_equal(
    prior_estimates$prior_infections,
    log(mean(c(10, 20, 40, 50, 70), na.rm = TRUE))
  )
  expect_true(!is.na(prior_estimates$prior_growth))
})

test_that("estimate_early_dynamics handles exponential growth", {
  cases <- 2^(c(0:6)) # Exponential growth
  prior_estimates <- estimate_early_dynamics(cases, 7)
  expect_equal(prior_estimates$prior_infections, log(mean(cases[1:7])))
  expect_true(prior_estimates$prior_growth > 0) # Growth should be positive
})

test_that("estimate_early_dynamics handles exponential decline", {
  cases <- rev(2^(c(0:6))) # Exponential decline
  prior_estimates <- estimate_early_dynamics(cases, 7)
  expect_equal(prior_estimates$prior_infections, log(mean(cases[1:7])))
  expect_true(prior_estimates$prior_growth < 0) # Growth should be negative
})

test_that("estimate_early_dynamics correctly handles seeding time less than 2", {
  cases <- c(5, 10, 20) # Less than 7 days of data
  prior_estimates <- estimate_early_dynamics(cases, 1)
  expect_equal(prior_estimates$prior_growth, 0) # Growth should be 0 if seeding time is <= 1
})