# Test add_day_of_week function
# This is an internal function so we use ::: accessor

test_that("add_day_of_week returns weekday indices with week_effect = 7", {
  # Create a week of dates starting Monday
  dates <- seq(as.Date("2024-01-01"), by = "day", length.out = 7)  # Mon-Sun

  result <- EpiNow2:::add_day_of_week(dates, week_effect = 7)

  # Should return 1-7 for Monday-Sunday
  expect_equal(result, 1:7)
})

test_that("add_day_of_week wraps around for multiple weeks", {
  # Two weeks of dates
  dates <- seq(as.Date("2024-01-01"), by = "day", length.out = 14)

  result <- EpiNow2:::add_day_of_week(dates, week_effect = 7)

  expect_equal(result, c(1:7, 1:7))
})

test_that("add_day_of_week handles custom week lengths", {
  dates <- seq(as.Date("2024-01-01"), by = "day", length.out = 10)

  # With week_effect = 5, should cycle through 1-5
  result <- EpiNow2:::add_day_of_week(dates, week_effect = 5)

  expect_equal(result, c(5, 1, 2, 3, 4, 5, 1, 2, 3, 4))
})

test_that("add_day_of_week handles week_effect = 1", {
  dates <- seq(as.Date("2024-01-01"), by = "day", length.out = 5)

  result <- EpiNow2:::add_day_of_week(dates, week_effect = 1)

  # All days should be day 1 of their "week"
  expect_equal(result, rep(1, 5))
})

test_that("add_day_of_week handles single date", {
  dates <- as.Date("2024-01-03")  # Wednesday

  result <- EpiNow2:::add_day_of_week(dates, week_effect = 7)

  expect_length(result, 1)
  expect_equal(result, 3)  # Wednesday = 3
})

test_that("add_day_of_week handles non-contiguous dates", {
  # Non-sequential dates
  dates <- as.Date(c("2024-01-01", "2024-01-03", "2024-01-08"))

  result <- EpiNow2:::add_day_of_week(dates, week_effect = 7)

  expect_equal(result, c(1, 3, 1))  # Mon, Wed, Mon
})
