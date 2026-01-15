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
  lifecycle::expect_deprecated(df_non_zero(out$estimates$samples))
  lifecycle::expect_deprecated(df_non_zero(out$estimates$summarised))
  lifecycle::expect_deprecated(
    df_non_zero(out$estimated_reported_cases$samples)
  )
  lifecycle::expect_deprecated(
    df_non_zero(out$estimated_reported_cases$summarised)
  )

  lifecycle::expect_deprecated(df_non_zero(out$summary))
  lifecycle::expect_deprecated(
    expect_equal(
      names(out$plots), c("summary", "infections", "reports", "R", "growth_rate")
    )
  )

  # Regression test: custom CrIs should be respected in output
  lifecycle::expect_deprecated(
    expect_equal(extract_CrIs(out$estimates$summarised), 95)
  )
  lifecycle::expect_deprecated(
    expect_equal(extract_CrIs(out$estimated_reported_cases$summarised), 95)
  )
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
  lifecycle::expect_deprecated(df_non_zero(out$estimates$samples))
  lifecycle::expect_deprecated(df_non_zero(out$estimates$summarised))
  lifecycle::expect_deprecated(
    df_non_zero(out$estimated_reported_cases$samples)
  )
  lifecycle::expect_deprecated(
    df_non_zero(out$estimated_reported_cases$summarised)
  )
  lifecycle::expect_deprecated(df_non_zero(out$summary))
  lifecycle::expect_deprecated(
    expect_equal(
      names(out$plots), c("summary", "infections", "reports", "R", "growth_rate")
    )
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
  expect_warning(df_non_zero(out$estimates$samples), "deprecated")
  expect_warning(df_non_zero(out$estimates$summarised), "deprecated")
  expect_warning(
    df_non_zero(out$estimated_reported_cases$samples), "deprecated"
  )
  expect_warning(
    df_non_zero(out$estimated_reported_cases$summarised), "deprecated"
  )
  expect_warning(df_non_zero(out$summary), "deprecated")
  expect_warning(
    expect_equal(
      names(out$plots), c("summary", "infections", "reports", "R", "growth_rate")
    ),
    "deprecated"
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
  expect_warning(df_non_zero(out$estimates$samples), "deprecated")
  expect_warning(df_non_zero(out$estimates$summarised), "deprecated")
  expect_warning(
    df_non_zero(out$estimated_reported_cases$samples), "deprecated"
  )
  expect_warning(
    df_non_zero(out$estimated_reported_cases$summarised), "deprecated"
  )
  expect_warning(df_non_zero(out$summary), "deprecated")
  expect_warning(
    expect_equal(
      names(out$plots), c("summary", "infections", "reports", "R", "growth_rate")
    ),
    "deprecated"
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
  expect_warning(df_non_zero(out$estimates$samples), "deprecated")
  expect_warning(df_non_zero(out$estimates$summarised), "deprecated")
  expect_warning(
    df_non_zero(out$estimated_reported_cases$summarised), "deprecated"
  )
  expect_warning(df_non_zero(out$summary), "deprecated")
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
