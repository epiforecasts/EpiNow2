skip_on_cran()

futile.logger::flog.threshold("FATAL")

regional_out <- example_regional_output()
results_dir <- withr::local_tempdir(.local_envir = teardown_env())
write_regional_results(regional_out$regional, results_dir)

sort_by_region <- function(dt) {
  setorderv(copy(dt), "region")
}

test_that("get_regions returns sorted, named region folders", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "b_region"))
  dir.create(file.path(dir, "a_region"))
  file.create(file.path(dir, "runtimes.csv"))

  expect_equal(
    get_regions(dir),
    c(a_region = "a_region", b_region = "b_region")
  )
})

test_that("get_raw_result reads a file saved for a region and date", {
  expect_equal(
    get_raw_result("runtime.rds", "testland", "latest", results_dir),
    regional_out$regional$testland$timing
  )
})

test_that("get_regional_results from disk matches in-memory results", {
  from_disk <- get_regional_results(
    results_dir = results_dir, forecast = TRUE
  )
  in_memory <- get_regional_results(regional_out$regional, forecast = TRUE)

  expect_named(from_disk, c("estimates", "estimated_reported_cases"))
  expect_named(from_disk$estimates, c("samples", "summarised"))
  expect_named(
    from_disk$estimated_reported_cases, c("samples", "summarised")
  )
  for (output in names(in_memory)) {
    for (type in c("samples", "summarised")) {
      expect_equal(
        sort_by_region(from_disk[[output]][[type]]),
        sort_by_region(in_memory[[output]][[type]]),
        ignore_attr = TRUE
      )
    }
  }
})

test_that("get_regional_results omits samples and forecasts on request", {
  out <- get_regional_results(
    results_dir = results_dir, samples = FALSE, forecast = FALSE
  )
  expect_named(out, "estimates")
  expect_named(out$estimates, "summarised")
  expect_setequal(
    unique(out$estimates$summarised$region), c("testland", "realland")
  )

  in_memory <- get_regional_results(
    regional_out$regional, samples = FALSE, forecast = TRUE
  )
  expect_named(in_memory$estimated_reported_cases, "summarised")
})

test_that("summarise_results errors for bad source specifications", {
  regions <- get_regions(results_dir)
  expect_error(
    summarise_results(regions),
    "must be supplied"
  )
  expect_error(
    summarise_results(regions, summaries = list(), results_dir = results_dir),
    "Cannot supply both"
  )
})

test_that("summarise_results reads summaries from a results directory", {
  regions <- get_regions(results_dir)
  out <- summarise_results(
    regions, results_dir = results_dir, region_scale = "Country"
  )

  expect_named(out, c("table", "data", "regions_by_inc"))
  expect_equal(
    colnames(out$table),
    c(
      "Country", "New infections per day", "Expected change in reports",
      "Effective reproduction no.", "Rate of growth",
      "Doubling/halving time (days)"
    )
  )
  expect_setequal(out$table$Country, c("testland", "realland"))
  expect_setequal(out$regions_by_inc, c("testland", "realland"))

  # regions are ranked by median new infections, highest first
  infections <- out$data[metric == "New infections per day"]
  expect_equal(
    out$regions_by_inc,
    as.character(infections[order(-median)]$region)
  )
  expect_equal(levels(out$data$region), out$regions_by_inc)
})

test_that("summarise_key_measures errors without results", {
  expect_error(
    summarise_key_measures(),
    "results_dir.*must be specified"
  )
})

test_that("summarise_key_measures reads and saves measures by region", {
  summary_dir <- withr::local_tempdir()
  out <- summarise_key_measures(
    results_dir = results_dir, summary_dir = summary_dir, type = "country"
  )

  measures <- c("rt", "growth_rate", "cases_by_infection", "cases_by_report")
  expect_named(out, measures)
  expect_setequal(
    list.files(summary_dir), paste0(measures, ".csv")
  )
  for (measure in measures) {
    expect_true("country" %in% colnames(out[[measure]]))
    expect_false("region" %in% colnames(out[[measure]]))
    expect_false("variable" %in% colnames(out[[measure]]))
    expect_setequal(unique(out[[measure]]$country), c("testland", "realland"))
  }
  # case counts are rounded to one decimal place
  expect_equal(
    out$cases_by_infection$median, round(out$cases_by_infection$median, 1)
  )
})
