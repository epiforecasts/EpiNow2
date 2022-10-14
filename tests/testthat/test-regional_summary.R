

fit <- readRDS("test-models/regional_epinow/static.rds")
cases <- readRDS("test-models/regional_epinow/cases.rds")

test_that("regional_summary works with default settings", {
  out <- regional_summary(
    regional_output = fit$regional,
    reported_cases = cases
  )
  expect_equal(names(out),
               c("latest_date", "results", "summarised_results",
                 "summary_plot", "summarised_measures", "reported_cases",
                 "high_plots", "plots")
  )
  expect_true(all(purrr::map_lgl(out, ~ !is.null(.))))
})

test_that("regional_summary works when no plots are requested", {
  out <- regional_summary(
    regional_output = fit$regional,
    reported_cases = cases,
    plot = FALSE
  )
  expect_equal(names(out),
               c("latest_date", "results", "summarised_results",
                 "summarised_measures", "reported_cases")
  )
  expect_true(all(purrr::map_lgl(out, ~ !is.null(.))))
})