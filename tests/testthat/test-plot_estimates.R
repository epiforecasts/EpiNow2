skip_on_cran()

# Uses shared fixtures from setup.R (regional_epinow run once)

test_that("plot_estimates can plot by type", {
  fixtures <- get_test_fixtures()
  fit <- fixtures$estimate_infections
  cases <- fit$observations

  expect_error(
    plot_estimates(
      estimate = summary(fit, type = "parameters", param = "infections"),
      ylab = "Cases", max_plot = 2
    ),
    NA
  )
  expect_error(
    plot_estimates(
      estimate = summary(fit, type = "parameters", param = "infections"),
      reported = cases,
      ylab = "Cases", max_plot = 2
    ),
    NA
  )
  expect_error(
    plot_estimates(
      estimate = summary(fit, type = "parameters", param = "infections"),
      reported = cases,
      ylab = "Cases", max_plot = 2
    ) +
      ggplot2::facet_wrap(~type, scales = "free_y"),
    NA
  )
})

test_that("plot_estimates can add a horizontal line ", {
  fixtures <- get_test_fixtures()
  fit <- fixtures$estimate_infections

  expect_error(
    plot_estimates(
      estimate = summary(fit, type = "parameters", param = "R"),
      ylab = "Effective Reproduction No.",
      hline = 1
    ),
    NA
  )
})

test_that("plot_estimates can be restricted to only plot within the forecasting
           time horizon", {
  fixtures <- get_test_fixtures()
  fit <- fixtures$estimate_infections

  expect_error(
    plot_estimates(
      estimate = summary(fit, type = "parameters", param = "R"),
      ylab = "Effective Reproduction No.",
      estimate_type = "Estimate"
    ),
    NA
  )
})

test_that("plot_estimates supports linerange style for error bars", {
  fixtures <- get_test_fixtures()
  fit <- fixtures$estimate_infections
  cases <- fit$observations
  expect_error(
    plot_estimates(
      estimate = summary(fit, type = "parameters", param = "R"),
      ylab = "Effective Reproduction No.",
      style = "linerange"
    ),
    NA
  )
  expect_error(
    plot_estimates(
      estimate = summary(fit, type = "parameters", param = "infections"),
      reported = cases,
      ylab = "Cases",
      style = "linerange"
    ),
    NA
  )
})

test_that("plot_estimates style defaults to ribbon", {
  fixtures <- get_test_fixtures()
  fit <- fixtures$estimate_infections
  p_default <- plot_estimates(
    estimate = summary(fit, type = "parameters", param = "R"),
    ylab = "R"
  )
  p_ribbon <- plot_estimates(
    estimate = summary(fit, type = "parameters", param = "R"),
    ylab = "R",
    style = "ribbon"
  )
  expect_s3_class(p_default, "ggplot")
  expect_s3_class(p_ribbon, "ggplot")
})

test_that("plot_estimates errors for invalid style", {
  fixtures <- get_test_fixtures()
  fit <- fixtures$estimate_infections
  expect_error(
    plot_estimates(
      estimate = summary(fit, type = "parameters", param = "R"),
      ylab = "R",
      style = "invalid_style"
    )
  )
})
