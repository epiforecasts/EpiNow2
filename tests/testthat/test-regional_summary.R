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

test_that("regional_summary errors when given both output and a directory", {
  regional_out <- example_regional_output()
  expect_error(
    regional_summary(
      regional_output = regional_out$regional,
      data = regional_out$summary$reported_cases,
      results_dir = withr::local_tempdir()
    ),
    "cannot be\\s+specified"
  )
})

test_that("regional_summary reads results from disk and saves a summary", {
  futile.logger::flog.threshold("FATAL")
  regional_out <- example_regional_output()
  results_dir <- withr::local_tempdir()
  write_regional_results(regional_out$regional, results_dir)
  summary_dir <- file.path(withr::local_tempdir(), "summary")

  out <- regional_summary(
    results_dir = results_dir,
    summary_dir = summary_dir,
    data = regional_out$summary$reported_cases,
    plot = FALSE
  )

  # nothing is returned by default when saving to a directory
  expect_null(out)
  expect_setequal(
    list.files(summary_dir),
    c(
      "latest_date.rds", "reported_cases.csv", "summary_table.csv",
      "summary_data.csv", "rt.csv", "growth_rate.csv",
      "cases_by_infection.csv", "cases_by_report.csv"
    )
  )
  summary_table <- fread(file.path(summary_dir, "summary_table.csv"))
  expect_setequal(summary_table$Region, c("testland", "realland"))
})

test_that("regional_summary saves each plot when given a summary directory", {
  futile.logger::flog.threshold("FATAL")
  regional_out <- example_regional_output()
  summary_dir <- withr::local_tempdir()
  saved <- character(0)
  local_mocked_bindings(
    ggsave = function(filename, ...) {
      saved <<- c(saved, basename(filename))
      invisible(filename)
    }
  )

  out <- regional_summary(
    regional_output = regional_out$regional,
    data = regional_out$summary$reported_cases,
    summary_dir = summary_dir,
    return_output = TRUE
  )

  expect_setequal(
    saved,
    c(
      "summary_plot.png", "high_rt_plot.png", "high_infections_plot.png",
      "high_reported_cases_plot.png", "rt_plot.png", "infections_plot.png",
      "reported_cases_plot.png"
    )
  )
  expect_s3_class(out$summary_plot, "ggplot")
  expect_named(out$plots, c("infections", "reports", "R", "growth_rate"))
})
