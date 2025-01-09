test_that("estimate_early_dynamics works", {
  cases <- EpiNow2::example_confirmed[1:30]
  early_estimates <- estimate_early_dynamics(cases$confirm, 7)
  # Check dimensions
  expect_identical(
    names(early_estimates),
    c("initial_infections_estimate", "initial_growth_estimate")
  )
  expect_identical(length(early_estimates), 2L)
  # Check values
  expect_identical(
    round(early_estimates$initial_infections_estimate, 2),
    3.21
  )
  expect_identical(
    round(early_estimates$initial_growth_estimate, 2),
    0.35
  )
})

test_that("estimate_early_dynamics handles NA values correctly", {
  cases <- c(10, 20, NA, 40, 50, NA, 70)
  early_estimates <- estimate_early_dynamics(cases, 7)
  expect_identical(
    round(early_estimates$initial_infections_estimate, 2),
    2.55
  )
  expect_true(!is.na(early_estimates$initial_growth_estimate))
})

test_that("estimate_early_dynamics handles exponential growth", {
  cases <- 2^(c(0:6)) # Exponential growth
  early_estimates <- estimate_early_dynamics(cases, 7)
  expect_equal(early_estimates$initial_infections_estimate, log(2^0))
  expect_true(early_estimates$initial_growth_estimate > 0) # Growth should be positive
})

test_that("estimate_early_dynamics handles exponential decline", {
  cases <- rev(2^(c(0:6))) # Exponential decline
  early_estimates <- estimate_early_dynamics(cases, 7)
  expect_equal(early_estimates$initial_infections_estimate, log(2^6))
  expect_true(early_estimates$initial_growth_estimate < 0) # Growth should be negative
})

test_that("estimate_early_dynamics correctly handles seeding time less than 2", {
  cases <- c(5, 10, 20) # Less than 7 days of data
  early_estimates <- estimate_early_dynamics(cases, 1)
  expect_equal(early_estimates$initial_growth_estimate, 0) # Growth should be 0 if seeding time is <= 1
})
