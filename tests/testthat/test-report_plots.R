

fit <- readRDS("test-models/estimate_infections/static.rds")
cases <- EpiNow2::example_confirmed[1:30]

test_that("report_plots works with  default settings", {
  expect_error(
    report_plots(
      summarised_estimates = fit$summarised,
      reported = cases),
    NA
  )
})