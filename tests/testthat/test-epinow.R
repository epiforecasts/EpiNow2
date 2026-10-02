skip_on_cran()

# set example reporting delay
reporting_delay <- LogNormal(
  meanlog = Normal(0.6, 0.06),
  sdlog = Normal(0.5, 0.1),
  max = 10
)

reported_cases <- EpiNow2::example_confirmed[1:30]

futile.logger::flog.threshold("FATAL")

df_non_zero <- function(df) {
  expect_true(nrow(df) > 0)
}
expected_out <- c("fit", "args", "observations", "timing")

# Integration tests (MCMC-based) ------------------------------------------
# These tests run actual MCMC sampling and are slow. Tests are divided into:
# - Core tests: Essential tests that always run to catch critical failures
# - Variant tests: Configuration variations that only run weekly (gated by EPINOW2_SKIP_INTEGRATION)

# Variant test: epinow is tested via estimate_infections underneath.
# This test verifies wrapper-specific functionality (plots, CrIs).
test_that("epinow produces expected output when run with default settings", {
  skip_integration()
  outputs <- capture.output(suppressMessages(suppressWarnings(
    out <- epinow(
      data = reported_cases,
      generation_time = gt_opts(example_generation_time),
      delays = delay_opts(example_incubation_period + reporting_delay),
      stan = stan_opts(
        samples = 25, warmup = 25,
        cores = 1, chains = 2,
        control = list(adapt_delta = 0.8)
      ),
      CrIs = c(0.95),
      logs = NULL, verbose = FALSE
    )
  )))

  expect_equal(names(out), expected_out)
  # Test new accessor methods work correctly
  df_non_zero(get_samples(out))
  df_non_zero(summary(out, type = "parameters"))
  df_non_zero(estimates_by_report_date(out)$summarised)
  expect_true(!is.null(summary(out)))
  expect_equal(
    names(plot(out, type = "all")),
    c("summary", "infections", "reports", "R", "growth_rate")
  )

  # Verify CrIs are present in output
  expect_true(length(extract_CrIs(summary(out, type = "parameters"))) > 0)
  expect_true(length(extract_CrIs(estimates_by_report_date(out)$summarised)) > 0)
})

test_that("epinow produces expected output with cmdstanr backend", {
  skip_integration()
  skip_on_os("windows")
  output <- capture.output(suppressMessages(suppressWarnings(
    out <- epinow(
      data = reported_cases,
      generation_time = gt_opts(example_generation_time),
      delays = delay_opts(example_incubation_period + reporting_delay),
      stan = stan_opts(backend = "cmdstanr"),
      logs = NULL, verbose = FALSE
    )
  )))

  expect_equal(names(out), expected_out)
  # Test new accessor methods work correctly
  df_non_zero(get_samples(out))
  df_non_zero(summary(out, type = "parameters"))
  df_non_zero(estimates_by_report_date(out)$summarised)
  expect_true(!is.null(summary(out)))
  expect_equal(
    names(plot(out, type = "all")),
    c("summary", "infections", "reports", "R", "growth_rate")
  )
})

test_that("epinow produces expected output with laplace algorithm", {
  skip_integration()
  skip_on_os("windows")
  output <- capture.output(suppressMessages(suppressWarnings(
    out <- epinow(
      data = reported_cases,
      generation_time = gt_opts(example_generation_time),
      delays = delay_opts(example_incubation_period + reporting_delay),
      stan = stan_opts(method = "laplace", backend = "cmdstanr"),
      logs = NULL, verbose = FALSE
    )
  )))
  expect_equal(names(out), expected_out)
  # Test new accessor methods work correctly
  df_non_zero(get_samples(out))
  df_non_zero(summary(out, type = "parameters"))
  df_non_zero(estimates_by_report_date(out)$summarised)
  expect_true(!is.null(summary(out)))
  expect_equal(
    names(plot(out, type = "all")),
    c("summary", "infections", "reports", "R", "growth_rate")
  )
})

test_that("epinow produces expected output with pathfinder algorithm", {
  skip_integration()
  skip_on_os("windows")
  output <- capture.output(suppressMessages(suppressWarnings(
    out <- epinow(
      data = reported_cases,
      generation_time = gt_opts(example_generation_time),
      delays = delay_opts(example_incubation_period + reporting_delay),
      stan = stan_opts(method = "pathfinder", backend = "cmdstanr"),
      logs = NULL, verbose = FALSE
    )
  )))
  expect_equal(names(out), expected_out)
  # Test new accessor methods work correctly
  df_non_zero(get_samples(out))
  df_non_zero(summary(out, type = "parameters"))
  df_non_zero(estimates_by_report_date(out)$summarised)
  expect_true(!is.null(summary(out)))
  expect_equal(
    names(plot(out, type = "all")),
    c("summary", "infections", "reports", "R", "growth_rate")
  )
})

test_that("epinow runs without error when saving to disk", {
  skip_integration()
  output <- capture.output(suppressMessages(suppressWarnings(
    out <- epinow(
      data = reported_cases,
      generation_time = gt_opts(example_generation_time),
      delays = delay_opts(example_incubation_period + reporting_delay),
      stan = stan_opts(
        samples = 25, warmup = 25, cores = 1, chains = 2,
        control = list(adapt_delta = 0.8)
      ),
      target_folder = tempdir(check = TRUE),
      logs = NULL, verbose = FALSE
    )
  )))
  expect_null(out)
})

test_that("epinow can produce partial output as specified", {
  skip_integration()
  output <- capture.output(suppressMessages(suppressWarnings(
    out <- epinow(
      data = reported_cases,
      generation_time = gt_opts(
        example_generation_time,
        weight_prior = FALSE
      ),
      delays = delay_opts(example_incubation_period + reporting_delay),
      stan = stan_opts(
        samples = 25, warmup = 25,
        cores = 1, chains = 2,
        control = list(adapt_delta = 0.8)
      ),
      output = NULL,
      logs = NULL, verbose = FALSE
    )
  )))
  expect_equal(names(out), c("fit", "args", "observations"))
  # Test new accessor methods work correctly
  df_non_zero(get_samples(out))
  df_non_zero(summary(out, type = "parameters"))
  df_non_zero(estimates_by_report_date(out)$summarised)
  expect_true(!is.null(summary(out)))
})

test_that("epinow propagates target_date into the forecast horizon", {
  skip_integration()
  max_date <- max(reported_cases$date)
  extra_days <- 3
  target_date <- max_date + extra_days
  base_horizon <- 7
  expected_horizon <- base_horizon + extra_days

  output <- capture.output(suppressMessages(suppressWarnings(
    out <- epinow(
      data = reported_cases,
      generation_time = gt_opts(example_generation_time),
      delays = delay_opts(example_incubation_period + reporting_delay),
      forecast = forecast_opts(horizon = base_horizon),
      stan = stan_opts(
        samples = 25, warmup = 25,
        cores = 1, chains = 1,
        control = list(adapt_delta = 0.8)
      ),
      target_date = target_date,
      logs = NULL, verbose = FALSE
    )
  )))

  expect_equal(out$args$horizon, expected_horizon)
  reported <- estimates_by_report_date(out)$summarised
  expect_equal(max(reported$date), target_date + base_horizon)
})

test_that("epinow warns and coerces target_date given as a character string", {
  skip_integration()
  target_date <- as.character(max(reported_cases$date))

  output <- capture.output(suppressMessages(expect_warning(
    out <- epinow(
      data = reported_cases,
      generation_time = gt_opts(example_generation_time),
      delays = delay_opts(example_incubation_period + reporting_delay),
      stan = stan_opts(
        samples = 25, warmup = 25,
        cores = 1, chains = 1,
        control = list(adapt_delta = 0.8)
      ),
      target_date = target_date,
      logs = NULL, verbose = FALSE
    ),
    class = "lifecycle_warning_deprecated"
  )))

  reported <- estimates_by_report_date(out)$summarised
  expect_equal(max(reported$date), as.Date(target_date) + out$args$horizon)
})

test_that("epinow fails as expected when given a short timeout", {
  skip_integration()
  expect_error(suppressWarnings(x <- epinow(
    data = reported_cases,
    generation_time = gt_opts(example_generation_time),
    delays = delay_opts(example_incubation_period + reporting_delay),
    stan = stan_opts(
      samples = 100, warmup = 100,
      cores = 1, chains = 2,
      control = list(adapt_delta = 0.8),
      max_execution_time = 1
    ),
    logs = NULL, verbose = FALSE
  )))
})

# Argument validation tests (fast - no MCMC) ------------------------------


test_that("epinow errors if target_date is not a Date or character string", {
  expect_error(
    epinow(
      data = reported_cases,
      generation_time = gt_opts(example_generation_time),
      delays = delay_opts(example_incubation_period + reporting_delay),
      target_date = 123,
      logs = NULL, verbose = FALSE
    ),
    "Date"
  )
})


test_that("epinow errors if target_date is missing (NA)", {
  expect_error(
    epinow(
      data = reported_cases,
      generation_time = gt_opts(example_generation_time),
      delays = delay_opts(example_incubation_period + reporting_delay),
      target_date = as.Date(NA),
      logs = NULL, verbose = FALSE
    ),
    "missing"
  )
})


test_that("epinow fails if given NUTs arguments when using variational inference", {
  expect_error(capture.output(suppressMessages(suppressWarnings(
    epinow(
      data = reported_cases,
      generation_time = gt_opts(example_generation_time),
      delays = delay_opts(example_incubation_period + reporting_delay),
      stan = stan_opts(
        samples = 100, warmup = 100,
        cores = 1, chains = 2,
        method = "vb"
      ),
      logs = NULL, verbose = FALSE
    )
  ))))
})


test_that("epinow fails if given variational inference arguments when using NUTs", {
  expect_error(capture.output(suppressMessages(suppressWarnings(
    epinow(
      data = reported_cases,
      generation_time = gt_opts(example_generation_time),
      delays = delay_opts(example_incubation_period + reporting_delay),
      stan = stan_opts(method = "sampling", tol_rel_obj = 1),
      logs = NULL, verbose = FALSE
    )
  ))))
})

# Wrapper behaviour with a mocked fit (fast - no MCMC) ----------------------
# estimate_infections() is replaced by a function returning a fitted fixture so
# that these tests only exercise the code around it.

test_that("epinow returns an epinow object built from the fit", {
  fit <- canned_estimate_infections()
  local_mocked_bindings(estimate_infections = function(...) fit)
  out <- epinow(data = reported_cases, logs = NULL, verbose = FALSE)
  expect_s3_class(out, "epinow")
  expect_s3_class(out, "estimate_infections")
  expect_named(out, expected_out)
  expect_s3_class(out$timing, "difftime")
  expect_identical(out$fit, fit$fit)
  expect_identical(out$args, fit$args)
})

test_that("epinow extends the forecast horizon to cover the target date", {
  horizons <- numeric(0)
  local_mocked_bindings(estimate_infections = function(forecast, ...) {
    horizons <<- c(horizons, forecast$horizon)
    canned_estimate_infections()
  })
  max_date <- max(reported_cases$date)
  run <- function(...) {
    epinow(
      data = reported_cases, output = NULL, logs = NULL, verbose = FALSE, ...
    )
  }
  run(forecast = forecast_opts(horizon = 7))
  run(forecast = forecast_opts(horizon = 7), target_date = max_date + 3)
  run(forecast = forecast_opts(horizon = 0), target_date = max_date + 3)
  run(forecast = NULL)
  expect_equal(horizons, c(7, 10, 0, 0))
})

test_that("epinow warns and coerces a character target_date", {
  withr::local_options(lifecycle_verbosity = "warning")
  target_dates <- list()
  local_mocked_bindings(estimate_infections = function(forecast, ...) {
    canned_estimate_infections()
  })
  local_mocked_bindings(update_horizon = function(horizon, target_date, data) {
    target_dates <<- c(target_dates, list(target_date))
    horizon
  })
  expect_warning(
    epinow(
      data = reported_cases, target_date = "2020-03-22", output = NULL,
      logs = NULL, verbose = FALSE
    ),
    class = "lifecycle_warning_deprecated"
  )
  expect_equal(target_dates, list(as.Date("2020-03-22")))
})

test_that("epinow only returns output when requested", {
  local_mocked_bindings(
    estimate_infections = function(...) canned_estimate_infections()
  )
  tmp <- withr::local_tempdir()
  expect_null(
    epinow(
      data = reported_cases, target_folder = tmp, output = NULL,
      logs = NULL, verbose = FALSE
    )
  )
  expect_invisible(
    epinow(
      data = reported_cases, return_output = FALSE, output = NULL,
      logs = NULL, verbose = FALSE
    )
  )
  out <- epinow(
    data = reported_cases, target_folder = tmp, return_output = TRUE,
    output = NULL, logs = NULL, verbose = FALSE
  )
  expect_named(out, c("fit", "args", "observations"))
})

test_that("epinow saves requested output to a dated target folder", {
  fit <- canned_estimate_infections()
  local_mocked_bindings(estimate_infections = function(...) fit)
  tmp <- withr::local_tempdir()
  cases <- data.table::copy(reported_cases)
  # trailing zero so the latest date with cases differs from the last date
  cases[.N, confirm := 0]
  epinow(
    data = cases, target_folder = tmp,
    output = c("samples", "fit", "timing", "latest"),
    logs = NULL, verbose = FALSE
  )
  dated <- file.path(tmp, as.character(max(cases$date)))
  expected_files <- c(
    "estimate_samples.rds", "estimated_reported_cases_samples.rds",
    "latest_date.rds", "model_args.rds", "model_fit.rds",
    "reported_cases.rds", "runtime.rds",
    "summarised_estimated_reported_cases.rds", "summarised_estimates.rds",
    "summary.rds"
  )
  expect_setequal(list.files(dated), expected_files)
  expect_setequal(list.files(file.path(tmp, "latest")), expected_files)
  expect_equal(readRDS(file.path(dated, "reported_cases.rds")), cases)
  expect_equal(
    readRDS(file.path(dated, "latest_date.rds")), max(cases$date) - 1
  )
  expect_equal(
    readRDS(file.path(dated, "summarised_estimates.rds")),
    summary(fit, type = "parameters")
  )
  expect_equal(readRDS(file.path(dated, "model_args.rds")), fit$args)
  expect_s3_class(readRDS(file.path(dated, "runtime.rds")), "difftime")
  expect_equal(
    readRDS(file.path(dated, "summary.rds")),
    summary(fit, return_numeric = TRUE)
  )
})

test_that("epinow skips optional files that are not requested", {
  local_mocked_bindings(
    estimate_infections = function(...) canned_estimate_infections()
  )
  tmp <- withr::local_tempdir()
  epinow(
    data = reported_cases, target_folder = tmp, output = NULL,
    logs = NULL, verbose = FALSE
  )
  expect_equal(list.files(tmp), as.character(max(reported_cases$date)))
  expect_setequal(
    list.files(file.path(tmp, max(reported_cases$date))),
    c(
      "latest_date.rds", "reported_cases.rds",
      "summarised_estimated_reported_cases.rds", "summarised_estimates.rds",
      "summary.rds"
    )
  )
})

test_that("epinow re-throws errors when run with the default id", {
  local_mocked_bindings(estimate_infections = function(...) {
    cli::cli_abort("model failed")
  })
  expect_error(
    epinow(data = reported_cases, logs = NULL, verbose = FALSE),
    "model failed"
  )
})

test_that("epinow returns and saves errors when run with a custom id", {
  local_mocked_bindings(estimate_infections = function(...) {
    cli::cli_abort("model failed")
  })
  tmp <- withr::local_tempdir()
  out <- epinow(
    data = reported_cases, target_folder = tmp, return_output = TRUE,
    output = "timing", id = "region_a", logs = NULL, verbose = FALSE
  )
  expect_s3_class(out, "epinow")
  expect_match(out$error, "^region_a: model failed")
  expect_s3_class(out$trace, "rlang_trace")
  expect_s3_class(out$timing, "difftime")
  dated <- file.path(tmp, as.character(max(reported_cases$date)))
  expect_equal(readRDS(file.path(dated, "error.rds")), out$error)
  expect_true(file.exists(file.path(dated, "trace.rds")))
})

test_that("epinow logs warnings rather than raising them", {
  local_mocked_bindings(estimate_infections = function(...) {
    cli::cli_warn("divergent transitions")
    canned_estimate_infections()
  })
  old_threshold <- futile.logger::flog.threshold(name = "EpiNow2.epinow")
  withr::defer(
    futile.logger::flog.threshold(old_threshold, name = "EpiNow2.epinow")
  )
  get_log <- capture_log("EpiNow2.epinow")
  expect_no_warning(
    out <- epinow(
      data = reported_cases, id = "region_a", output = NULL,
      logs = NULL, verbose = TRUE
    )
  )
  expect_s3_class(out, "epinow")
  expect_true(any(grepl("WARN.*region_a: divergent transitions", get_log())))
  # verbose output raises the epinow logger to debug level
  expect_equal(
    futile.logger::flog.threshold(name = "EpiNow2.epinow"), "DEBUG"
  )
})

test_that("epinow writes log files when given a logs directory", {
  local_mocked_bindings(estimate_infections = function(...) {
    cli::cli_warn("divergent transitions")
    canned_estimate_infections()
  })
  logs <- withr::local_tempdir()
  withr::defer({
    for (logger in c("EpiNow2", "EpiNow2.epinow")) {
      futile.logger::flog.appender(
        futile.logger::appender.console(), name = logger
      )
    }
    setup_default_logging(logs = NULL)
  })
  suppressMessages(
    epinow(
      data = reported_cases, id = "region_a", output = NULL, logs = logs,
      verbose = FALSE
    )
  )
  expect_true(dir.exists(file.path(logs, "regional-epinow")))
  log_file <- file.path(
    logs, "epinow", paste0(max(reported_cases$date), ".log")
  )
  expect_true(file.exists(log_file))
  expect_true(
    any(grepl("region_a: divergent transitions", readLines(log_file)))
  )
})

test_that("epinow errors informatively for deprecated elements", {
  local_mocked_bindings(
    estimate_infections = function(...) canned_estimate_infections()
  )
  out <- epinow(
    data = reported_cases, output = NULL, logs = NULL, verbose = FALSE
  )
  deprecated <- c(
    "estimates", "estimated_reported_cases", "summary", "plots",
    "estimate_infections"
  )
  for (element in deprecated) {
    expect_error(out[[element]], class = "lifecycle_error_deprecated")
    expect_error(
      do.call(`$`, list(out, element)), class = "lifecycle_error_deprecated"
    )
  }
  expect_identical(out$observations, out[["observations"]])
  expect_null(out$error)
})

# Internal helpers ----------------------------------------------------------

test_that("update_horizon extends a non-zero horizon to the target date", {
  data <- data.frame(date = as.Date("2020-01-01") + 0:9)
  expect_equal(update_horizon(7, as.Date("2020-01-10"), data), 7)
  expect_equal(update_horizon(7, as.Date("2020-01-13"), data), 10)
  expect_equal(update_horizon(7, as.Date("2020-01-08"), data), 5)
  expect_equal(update_horizon(0, as.Date("2020-01-13"), data), 0)
})

test_that("save_input does nothing without a target folder", {
  expect_null(save_input(data.table::copy(reported_cases), NULL))
})

test_that("copy_results_to_latest copies and overwrites files", {
  tmp <- withr::local_tempdir()
  first <- file.path(tmp, "2020-01-01")
  second <- file.path(tmp, "2020-01-02")
  latest <- file.path(tmp, "latest")
  dir.create(first)
  dir.create(second)
  saveRDS("first", file.path(first, "summary.rds"))
  saveRDS("second", file.path(second, "summary.rds"))

  expect_null(copy_results_to_latest(NULL, latest))
  expect_false(dir.exists(latest))

  copy_results_to_latest(first, latest)
  expect_equal(readRDS(file.path(latest, "summary.rds")), "first")
  copy_results_to_latest(second, latest)
  expect_equal(readRDS(file.path(latest, "summary.rds")), "second")
})

test_that("copy_results_to_latest removes files from earlier runs", {
  skip("Known bug, see #1572")
  tmp <- withr::local_tempdir()
  first <- file.path(tmp, "2020-01-01")
  second <- file.path(tmp, "2020-01-02")
  latest <- file.path(tmp, "latest")
  dir.create(first)
  dir.create(second)
  saveRDS("failed", file.path(first, "error.rds"))
  saveRDS("second", file.path(second, "summary.rds"))
  copy_results_to_latest(first, latest)
  copy_results_to_latest(second, latest)
  expect_equal(list.files(latest), "summary.rds")
})

test_that("estimates_by_report_date returns reported case estimates", {
  fit <- canned_estimate_infections()
  out <- estimates_by_report_date(fit, CrIs = 0.9)
  expect_named(out, c("samples", "summarised"))
  expect_named(out$samples, c("date", "sample", "cases", "type"))
  # 30 observed days plus a 7 day forecast
  expect_equal(data.table::uniqueN(out$samples$date), 37)
  expect_equal(
    out$samples$cases,
    get_samples(fit)[variable == "reported_cases"]$value
  )
  expect_equal(nrow(out$summarised), 37)
  expect_true(all(out$summarised$type == "gp_rt"))
  expect_false(any(c("variable", "strat") %in% names(out$summarised)))
  expect_true(all(c("lower_90", "upper_90") %in% names(out$summarised)))

  no_samples <- estimates_by_report_date(fit, samples = FALSE)
  expect_named(no_samples, "summarised")
  expect_equal(no_samples$summarised, estimates_by_report_date(fit)$summarised)
})
