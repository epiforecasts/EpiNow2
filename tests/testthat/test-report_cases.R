test_that("report_cases can simulate infections forward", {
  set.seed(123)
  # define example cases
  cases <- example_confirmed[1:10]

  # Instead of running them model we use example
  # data for speed in this example.
  cases <- cases[, cases := as.integer(confirm)]
  cases <- cases[, confirm := NULL][, sample := 1]
  reported_cases <- report_cases(
    case_estimates = cases,
    delays = delay_opts(example_incubation_period + example_reporting_delay),
    type = "sample"
  )
  expect_equal(class(reported_cases), "list")
  expect_equal(class(reported_cases$samples), c("data.table", "data.frame"))
  expect_equal(class(reported_cases$summarised), c("data.table", "data.frame"))
  expect_equal(nrow(reported_cases$summarised), 10)
  expect_equal(class(reported_cases$summarised$median), "numeric")
  set.seed(Sys.time())
})

test_that("deprecated warnings are caught", {
  cases <- example_confirmed[1:40]
  # get example delays
  #' # Instead of running them model we use example
  #' # data for speed in this example.
  cases <- cases[, cases := as.integer(confirm)]
  cases <- cases[, confirm := NULL][, sample := 1]
  expect_deprecated(
    report_cases(
      case_estimates = cases,
      delays = delay_opts(example_incubation_period + example_reporting_delay),
      type = "sample"
    )
  )
})
