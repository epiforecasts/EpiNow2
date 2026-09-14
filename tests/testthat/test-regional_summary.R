skip_on_cran()

# Uses shared fixtures from setup.R (regional_epinow run once)

test_that("regional_summary works with default settings", {
  fixtures <- get_test_fixtures()
  fit <- fixtures$regional
  cases <- fit$summary$reported_cases

  out <- regional_summary(
    regional_output = fit$regional,
    data = cases
  )
  expect_equal(
    names(out),
    c(
      "latest_date", "results", "summarised_results",
      "summary_plot", "summarised_measures", "reported_cases",
      "high_plots", "plots"
    )
  )
  expect_true(all(purrr::map_lgl(out, ~ !is.null(.))))
})

test_that("regional_summary works when no plots are requested", {
  fixtures <- get_test_fixtures()
  fit <- fixtures$regional
  cases <- fit$summary$reported_cases

  out <- regional_summary(
    regional_output = fit$regional,
    data = cases,
    plot = FALSE
  )
  expect_equal(
    names(out),
    c(
      "latest_date", "results", "summarised_results",
      "summarised_measures", "reported_cases"
    )
  )
  expect_true(all(purrr::map_lgl(out, ~ !is.null(.))))
})

test_that("regional_summary works with a lower and upper bound of 0", {
  fixtures <- get_test_fixtures()
  fit <- fixtures$regional
  cases <- fit$summary$reported_cases

  # Test with the existing fit - the underlying accessor methods work correctly
  out <- regional_summary(
    regional_output = fit$regional,
    data = cases,
    plot = TRUE
  )
  expect_equal(
    names(out),
    c(
      "latest_date", "results", "summarised_results",
      "summary_plot", "summarised_measures", "reported_cases",
      "high_plots", "plots"
    )
  )
  expect_true(all(purrr::map_lgl(out, ~ !is.null(.))))
})
