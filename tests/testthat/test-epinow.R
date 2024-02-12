skip_on_cran()

# set example reporting delay
reporting_delay <- dist_spec(
  mean = convert_to_logmean(2, 1), mean_sd = 0.1,
  sd = convert_to_logsd(2, 1), sd_sd = 0.1,
  max = 10
)

reported_cases <- EpiNow2::example_confirmed[1:30]

futile.logger::flog.threshold("FATAL")

df_non_zero <- function(df) {
  expect_true(nrow(df) > 0)
}
expected_out <- c("estimates", "estimated_reported_cases", "summary", "plots", "timing")

test_that("epinow produces expected output when run with default settings", {
  out <- suppressWarnings(epinow(
    reported_cases = reported_cases,
    generation_time = generation_time_opts(example_generation_time),
    delays = delay_opts(example_incubation_period + reporting_delay),
    stan = stan_opts(
      samples = 25, warmup = 25,
      cores = 1, chains = 2,
      control = list(adapt_delta = 0.8)
    ),
    logs = NULL, verbose = FALSE
  ))

  expect_equal(names(out), expected_out)
  df_non_zero(out$estimates$samples)
  df_non_zero(out$estimates$summarised)
  df_non_zero(out$estimated_reported_cases$samples)
  df_non_zero(out$estimated_reported_cases$summarised)
  df_non_zero(out$summary)
  expect_equal(names(out$plots), c("infections", "reports", "R", "growth_rate", "summary"))
})

test_that("epinow produces expected output when run with the
           cmdstanr backend", {
  skip_on_os("windows")
  output <- capture.output(suppressMessages(suppressWarnings(
    out <- epinow(
      reported_cases = reported_cases,
      generation_time = generation_time_opts(example_generation_time),
      delays = delay_opts(example_incubation_period + reporting_delay),
      stan = stan_opts(
        samples = 25, warmup = 25,
        cores = 1, chains = 2,
        control = list(adapt_delta = 0.8),
        backend = "cmdstanr"
      ),
      logs = NULL, verbose = FALSE
    )
  )))

  expect_equal(names(out), expected_out)
  df_non_zero(out$estimates$samples)
  df_non_zero(out$estimates$summarised)
  df_non_zero(out$estimated_reported_cases$samples)
  df_non_zero(out$estimated_reported_cases$summarised)
  df_non_zero(out$summary)
  expect_equal(names(out$plots), c("infections", "reports", "R", "growth_rate", "summary"))
})

test_that("epinow runs without error when saving to disk", {
  expect_null(suppressWarnings(epinow(
    reported_cases = reported_cases,
    generation_time = generation_time_opts(example_generation_time),
    delays = delay_opts(example_incubation_period + reporting_delay),
    stan = stan_opts(
      samples = 25, warmup = 25, cores = 1, chains = 2,
      control = list(adapt_delta = 0.8)
    ),
    target_folder = tempdir(check = TRUE),
    logs = NULL, verbose = FALSE
  )))
})

test_that("epinow can produce partial output as specified", {
  out <- suppressWarnings(epinow(
    reported_cases = reported_cases,
    generation_time = generation_time_opts(example_generation_time),
    delays = delay_opts(example_incubation_period + reporting_delay),
    stan = stan_opts(
      samples = 25, warmup = 25,
      cores = 1, chains = 2,
      control = list(adapt_delta = 0.8)
    ),
    output = NULL,
    logs = NULL, verbose = FALSE
  ))
  expect_equal(names(out), c("estimates", "estimated_reported_cases", "summary"))
  expect_null(out$estimates$samples)
  df_non_zero(out$estimates$summarised)
  expect_null(out$estimated_reported_cases$samples)
  df_non_zero(out$estimated_reported_cases$summarised)
  df_non_zero(out$summary)
})



test_that("epinow fails as expected when given a short timeout", {
  expect_error(suppressWarnings(x = epinow(
    reported_cases = reported_cases,
    generation_time = generation_time_opts(example_generation_time),
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


test_that("epinow fails if given NUTs arguments when using variational inference", {
  expect_error(suppressWarnings(epinow(
    reported_cases = reported_cases,
    generation_time = generation_time_opts(example_generation_time),
    delays = delay_opts(example_incubation_period + reporting_delay),
    stan = stan_opts(
      samples = 100, warmup = 100,
      cores = 1, chains = 2,
      method = "vb"
    ),
    logs = NULL, verbose = FALSE
  )))
})


test_that("epinow fails if given variational inference arguments when using NUTs", {
  expect_error(suppressWarnings(epinow(
    reported_cases = reported_cases,
    generation_time = generation_time_opts(example_generation_time),
    delays = delay_opts(example_incubation_period + reporting_delay),
    stan = stan_opts(method = "sampling", tol_rel_obj = 1),
    logs = NULL, verbose = FALSE
  )))
})
