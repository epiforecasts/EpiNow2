test_that("plot_estimates works with aggregation = 'weekly'", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")

  # Load example data
  out <- readRDS(system.file(
    package = "EpiNow2", "extdata", "example_estimate_infections.rds"
  ))

  # Plot with weekly aggregation
  p <- plot_estimates(
    estimate = out$summarised[variable == "reported_cases"],
    reported = out$observations,
    ylab = "Weekly cases",
    aggregation = "weekly"
  )

  # Check it returns a ggplot object
  expect_s3_class(p, "gg")
  expect_s3_class(p, "ggplot")
})

test_that("plot_estimates defaults to daily aggregation", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")

  out <- readRDS(system.file(
    package = "EpiNow2", "extdata", "example_estimate_infections.rds"
  ))

  # Plot without specifying aggregation (should default to daily)
  p <- plot_estimates(
    estimate = out$summarised[variable == "reported_cases"],
    reported = out$observations,
    ylab = "Cases"
  )

  expect_s3_class(p, "gg")
})

test_that("plot_estimates respects week_start parameter", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")

  out <- readRDS(system.file(
    package = "EpiNow2", "extdata", "example_estimate_infections.rds"
  ))

  # Plot with Monday start
  p1 <- plot_estimates(
    estimate = out$summarised[variable == "reported_cases"],
    reported = out$observations,
    ylab = "Weekly cases",
    aggregation = "weekly",
    week_start = 1
  )

  # Plot with Sunday start
  p2 <- plot_estimates(
    estimate = out$summarised[variable == "reported_cases"],
    reported = out$observations,
    ylab = "Weekly cases",
    aggregation = "weekly",
    week_start = 7
  )

  expect_s3_class(p1, "gg")
  expect_s3_class(p2, "gg")

  # Plots should be different (different week boundaries)
  expect_false(identical(p1, p2))
})

test_that("plot_estimates with weekly aggregation works without reported", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")

  out <- readRDS(system.file(
    package = "EpiNow2", "extdata", "example_estimate_infections.rds"
  ))

  # Plot R estimates with weekly aggregation (no reported data)
  p <- plot_estimates(
    estimate = out$summarised[variable == "R"],
    ylab = "Effective Reproduction No.",
    hline = 1,
    aggregation = "weekly"
  )

  expect_s3_class(p, "gg")
})

test_that("plot.estimate_infections accepts aggregation argument", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")

  out <- readRDS(system.file(
    package = "EpiNow2", "extdata", "example_estimate_infections.rds"
  ))

  # Use S3 method with weekly aggregation
  plots <- plot(out, type = "reports", aggregation = "weekly")

  expect_s3_class(plots, "gg")
})

test_that("aggregate_to_weekly is an internal function", {
  # This function should not be exported
  expect_false("aggregate_to_weekly" %in% getNamespaceExports("EpiNow2"))
})

test_that("aggregate_to_weekly handles estimate data correctly", {
  skip_on_cran()

  out <- readRDS(system.file(
    package = "EpiNow2", "extdata", "example_estimate_infections.rds"
  ))

  estimate <- out$summarised[variable == "reported_cases"]
  reported <- out$observations

  # Call internal function directly
  aggregated <- EpiNow2:::aggregate_to_weekly(
    estimate,
    reported = reported,
    week_start = 1
  )

  expect_type(aggregated, "list")
  expect_named(aggregated, c("estimate", "reported"))
  expect_s3_class(aggregated$estimate, "data.table")
  expect_s3_class(aggregated$reported, "data.table")

  # Weekly data should have fewer rows than daily
  expect_lt(nrow(aggregated$estimate), nrow(estimate))
  expect_lt(nrow(aggregated$reported), nrow(reported))
})
