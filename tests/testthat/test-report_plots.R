fit <- readRDS(system.file(
  package = "EpiNow2", "extdata", "example_estimate_infections.rds"
))
cases <- fit$observations

test_that("report_plots works with  default settings", {
  expect_error(
    report_plots(
      summarised_estimates = fit$summarised,
      reported = cases
    ),
    NA
  )
})
