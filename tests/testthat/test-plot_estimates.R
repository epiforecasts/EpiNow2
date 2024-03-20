fit <- readRDS(system.file(
  package = "EpiNow2", "extdata", "example_estimate_infections.rds"
))
cases <- fit$observations

test_that("plot_estimates can plot by type", {
  expect_error(
    plot_estimates(
      estimate = fit$summarised[variable == "infections"],
      ylab = "Cases", max_plot = 2
    ),
    NA
  )
  expect_error(
    plot_estimates(
      estimate = fit$summarised[variable == "infections"],
      reported = cases,
      ylab = "Cases", max_plot = 2
    ),
    NA
  )
  expect_error(
    plot_estimates(
      estimate = fit$summarised[variable == "infections"],
      reported = cases,
      ylab = "Cases", max_plot = 2
    ) +
      ggplot2::facet_wrap(~type, scales = "free_y"),
    NA
  )
})

test_that("plot_estimates can add a horizontal line ", {
  expect_error(
    plot_estimates(
      estimate = fit$summarised[variable == "R"],
      ylab = "Effective Reproduction No.",
      hline = 1
    ),
    NA
  )
})

test_that("plot_estimates can be restricted to only plot within the forecasting
           time horizon", {
  expect_error(
    plot_estimates(
      estimate = fit$summarised[variable == "R"],
      ylab = "Effective Reproduction No.",
      estimate_type = "Estimate"
    ),
    NA
  )
})
