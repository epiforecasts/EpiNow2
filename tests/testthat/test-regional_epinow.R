
skip_on_cran()

# get example delays
futile.logger::flog.threshold("FATAL")

## Uses example case vector
cases <- EpiNow2::example_confirmed[1:30]
cases <- data.table::rbindlist(list(
  data.table::copy(cases)[, region := "testland"],
  cases[, region := "realland"]
))

df_non_zero <- function(df) {
  expect_true(nrow(df) > 0)
}

test_that("regional_epinow produces expected output when run with default settings", {
  out <- suppressWarnings(
    regional_epinow(
      data = cases,
      generation_time = generation_time_opts(example_generation_time),
      delays = delay_opts(example_reporting_delay),
      rt = rt_opts(rw = 10), gp = NULL,
      stan = stan_opts(
        samples = 100, warmup = 100,
        cores = 1, chains = 2,
        control = list(adapt_delta = 0.8)
      ),
      logs = NULL, verbose = FALSE
    )
  )
  expect_equal(names(out$regional), c("testland", "realland"))
  expect_equal(names(out$summary), c(
    "latest_date", "results", "summarised_results", "summary_plot",
    "summarised_measures", "reported_cases", "high_plots", "plots"
  ))
  expect_equal(names(out$regional$realland), c("estimates", "estimated_reported_cases", "summary", "plots", "timing"))
  expect_type(out$regional$realland$timing, "double")
  df_non_zero(out$regional$realland$estimates$samples)
  df_non_zero(out$regional$realland$estimates$summarised)
  df_non_zero(out$regional$realland$estimated_reported_cases$samples)
  df_non_zero(out$regional$realland$estimated_reported_cases$summarised)
  df_non_zero(out$regional$realland$summary)
  expect_equal(names(out$regional$realland$plots), c("summary", "infections", "reports", "R", "growth_rate"))
})

test_that("regional_epinow runs without error when given a very short timeout", {
  output <- capture.output(suppressMessages(
    out <- regional_epinow(
      data = cases,
      generation_time = generation_time_opts(example_generation_time),
      delays = delay_opts(example_reporting_delay),
      stan = stan_opts(
        samples = 1000, warmup = 100,
        cores = 1, chains = 2,
        control = list(adapt_delta = 0.8),
        max_execution_time = 1
      ), logs = NULL, verbose = FALSE
    )
  ))
  expect_true(all(vapply(out$regional, function(x) !is.null(x$error), TRUE)))
  output <- capture.output(suppressMessages(
    out <- regional_epinow(
      data = cases,
      generation_time = generation_time_opts(example_generation_time),
      delays = delay_opts(example_reporting_delay),
      stan = stan_opts(
        samples = 1000, warmup = 100,
        cores = 1, chains = 2,
        control = list(adapt_delta = 0.8),
        max_execution_time = 1, future = TRUE
      ), logs = NULL, verbose = FALSE
    )
  ))
  expect_true(all(vapply(out$regional, function(x) !is.null(x$error), TRUE)))
})


test_that("regional_epinow produces expected output when run with region specific settings", {
  gp <- opts_list(gp_opts(), cases)
  gp <- modifyList(gp, list(realland = NULL), keep.null = TRUE)
  rt <- opts_list(rt_opts(), cases, realland = rt_opts(rw = 7))
  out <- suppressWarnings(
    regional_epinow(
      data = cases,
      generation_time = generation_time_opts(example_generation_time),
      delays = delay_opts(example_reporting_delay),
      rt = rt, gp = gp,
      stan = stan_opts(
        samples = 100, warmup = 100,
        cores = 1, chains = 2,
        control = list(adapt_delta = 0.8)
      ),
      logs = NULL, verbose = FALSE
    )
  )
  expect_equal(names(out$regional), c("testland", "realland"))
  expect_equal(names(out$summary), c(
    "latest_date", "results", "summarised_results", "summary_plot",
    "summarised_measures", "reported_cases", "high_plots", "plots"
  ))
  expect_equal(names(out$regional$realland), c("estimates", "estimated_reported_cases", "summary", "plots", "timing"))
  expect_type(out$regional$realland$timing, "double")
  df_non_zero(out$regional$realland$estimates$samples)
  df_non_zero(out$regional$realland$estimates$summarised)
  df_non_zero(out$regional$realland$estimated_reported_cases$samples)
  df_non_zero(out$regional$realland$estimated_reported_cases$summarised)
  df_non_zero(out$regional$realland$summary)
  expect_equal(names(out$regional$realland$plots), c("summary", "infections", "reports", "R", "growth_rate"))
})

test_that("deprecated arguments are recognised", {
  options(warn = 2)
  expect_error(regional_epinow(data = cases),
                      "deprecated"
  )
})
