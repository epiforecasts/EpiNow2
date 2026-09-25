skip_on_cran()

# estimate_infections() is replaced by a function returning a fitted fixture so
# that these tests only exercise regional_epinow() and its helpers.

futile.logger::flog.threshold("FATAL")
futile.logger::flog.threshold("FATAL", name = "EpiNow2")

cases <- EpiNow2::example_confirmed[1:30]
cases <- data.table::rbindlist(list(
  data.table::copy(cases)[, region := "testland"],
  data.table::copy(cases)[, region := "realland"]
))

# Returns a mock where testland fits and realland fails. The fit is created
# outside the mock because regions run inside futures, where the fixture cache
# cannot be updated.
mock_fit_by_region <- function() {
  fit <- canned_estimate_infections()
  function(..., id) {
    if (id == "realland") {
      cli::cli_abort("model failed")
    }
    fit
  }
}

mock_fit <- function() {
  fit <- canned_estimate_infections()
  function(...) fit
}

run_regional_epinow <- function(...) {
  regional_epinow(
    data = data.table::copy(cases),
    generation_time = gt_opts(example_generation_time),
    logs = NULL, verbose = FALSE, ...
  )
}

test_that("regional_epinow returns fits and errors by region", {
  local_mocked_bindings(estimate_infections = mock_fit_by_region())
  out <- run_regional_epinow(output = "regions")
  expect_named(out, c("regional", "timings"))
  expect_named(out$regional, c("testland", "realland"))
  expect_s3_class(out$regional$testland, "epinow")
  expect_s3_class(out$regional$testland$timing, "difftime")
  expect_match(out$regional$realland$error, "^realland: model failed")
  expect_equal(out$timings$region, c("testland", "realland"))
})

test_that("regional_epinow summarises only successful regions", {
  local_mocked_bindings(estimate_infections = mock_fit_by_region())
  out <- run_regional_epinow(
    output = c("regions", "summary"), summary_args = list(plot = FALSE)
  )
  expect_named(out, c("regional", "summary", "timings"))
  expect_equal(out$summary$summarised_results$table$Region, "testland")
})

test_that("regional_epinow continues when the summary fails", {
  local_mocked_bindings(estimate_infections = mock_fit())
  out <- run_regional_epinow(
    output = c("regions", "summary"),
    summary_args = list(not_an_argument = TRUE)
  )
  expect_named(out, c("regional", "timings"))
  expect_named(out$regional, c("testland", "realland"))
})

test_that("regional_epinow saves results to a folder per region", {
  local_mocked_bindings(estimate_infections = mock_fit())
  tmp <- withr::local_tempdir()
  expect_null(
    run_regional_epinow(target_folder = tmp, output = c("regions", "latest"))
  )
  target_date <- as.character(max(cases$date))
  expect_setequal(list.files(tmp), c("testland", "realland", "runtimes.csv"))
  for (region in c("testland", "realland")) {
    expect_setequal(
      list.files(file.path(tmp, region)), c(target_date, "latest")
    )
    expect_true(
      file.exists(file.path(tmp, region, target_date, "runtime.rds"))
    )
  }
  runtimes <- data.table::fread(file.path(tmp, "runtimes.csv"))
  expect_equal(runtimes$region, c("testland", "realland"))
})

test_that("clean_regions drops regions with too few non-zero points", {
  data <- data.table::data.table(
    date = rep(as.Date("2020-01-01") + 0:2, 4),
    region = rep(c("a", "b", "c", NA), each = 3),
    confirm = c(1, 2, 3, 0, 1, 0, 0, 0, 0, 1, 1, 1)
  )
  expect_equal(
    unique(clean_regions(data.table::copy(data), 1)$region), c("a", "b")
  )
  expect_equal(
    unique(clean_regions(data.table::copy(data), 2)$region), "a"
  )
  expect_equal(nrow(clean_regions(data.table::copy(data), 1)), 6)
})

test_that("clean_regions logs included and excluded regions", {
  old_threshold <- futile.logger::flog.threshold(name = "EpiNow2")
  futile.logger::flog.threshold("INFO", name = "EpiNow2")
  withr::defer(futile.logger::flog.threshold(old_threshold, name = "EpiNow2"))
  get_log <- capture_log("EpiNow2")

  few <- data.table::data.table(
    date = as.Date("2020-01-01"), region = c("a", "b"), confirm = c(1, 0)
  )
  clean_regions(few, 1)
  log <- trimws(get_log())
  expect_true(any(grepl("Producing estimates for: a$", log)))
  expect_true(any(grepl("Regions excluded: b$", log)))

  many <- data.table::data.table(
    date = as.Date("2020-01-01"),
    region = paste0("region_", 1:32),
    confirm = c(rep(1, 31), 0)
  )
  clean_regions(many, 1)
  log <- trimws(get_log())
  expect_true(any(grepl("Producing estimates for: 31 regions$", log)))
  expect_true(any(grepl("Regions excluded: 1 regions$", log)))
})
