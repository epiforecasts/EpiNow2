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

test_that("report_plots saves plots to a target folder", {
  fit <- get_test_fixtures()$estimate_infections
  tmp <- withr::local_tempdir()
  plots <- report_plots(
    summarised_estimates = summary(fit, type = "parameters"),
    reported = fit$observations,
    target_folder = tmp
  )
  expect_named(plots, c("infections", "reports", "R", "growth_rate", "summary"))
  expect_true(all(vapply(plots, ggplot2::is_ggplot, logical(1))))
  expect_setequal(
    list.files(tmp),
    c(
      "infections_plot.png", "reported_plot.png", "reff_plot.png",
      "growth_rate_plot.png", "summary_plot.png"
    )
  )
})
