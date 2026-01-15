skip_on_cran()

# Uses shared fixtures from setup.R (regional_epinow run once)

test_that("report_plots works with default settings", {
  fixtures <- get_test_fixtures()
  fit <- fixtures$estimate_infections
  cases <- fit$observations

  expect_error(
    report_plots(
      summarised_estimates = summary(fit, type = "parameters"),
      reported = cases
    ),
    NA
  )
})
