horizon <- 7
cases <- add_horizon(example_confirmed[1:30], horizon)
cases <- add_breakpoints(cases)
## add zeroes initially
cases <- data.table::rbindlist(list(
  data.table::data.table(
    date = seq(
      min(cases$date) - 10,
      min(cases$date) - 1,
      by = "days"
    ),
    confirm = 0, breakpoint = 0
  ),
  cases
))

test_that("create_shifted_cases does not create discontinuities with exponential growth", {
  result <- create_shifted_cases(cases, 7, 14, horizon)
  expect_s3_class(result, "data.table")
  expect_true(all(diff(result$confirm) > 0))
})
