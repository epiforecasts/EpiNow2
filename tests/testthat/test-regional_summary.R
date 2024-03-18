

fit <- readRDS("test-models/regional_epinow/static.rds")
cases <- readRDS("test-models/regional_epinow/cases.rds")

test_that("regional_summary works with default settings", {
  out <- regional_summary(
    regional_output = fit$regional,
    reported_cases = cases
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
  out <- regional_summary(
    regional_output = fit$regional,
    reported_cases = cases,
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
  regional_zero_fit <- lapply(fit$regional, function(x) {
    numeric_estimate <- x$summary[
        measure == "New infections per day"
      ]$numeric_estimate[[1]]
    uppers <- grep("upper_", colnames(numeric_estimate), value = TRUE)
    lowers <- grep("lower_", colnames(numeric_estimate), value = TRUE)
    numeric_estimate[, paste(uppers) := 0]
    numeric_estimate[, paste(lowers) := 0]
    x$summary[
        measure == "New infections per day",
        numeric_estimate := list(..numeric_estimate)
      ]
    return(x)
  })
  out <- regional_summary(
    regional_output = regional_zero_fit,
    reported_cases = cases,
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
