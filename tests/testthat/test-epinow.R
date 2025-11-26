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
expected_out <- c("estimates", "estimated_reported_cases", "summary", "plots", "timing")

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
  df_non_zero(out$estimates$samples)
  df_non_zero(out$estimates$summarised)
  df_non_zero(out$estimated_reported_cases$samples)
  df_non_zero(out$estimated_reported_cases$summarised)
  df_non_zero(out$summary)
  expect_equal(names(out$plots), c("summary", "infections", "reports", "R", "growth_rate"))

  # Regression test: custom CrIs should be respected in output
  expect_equal(extract_CrIs(out$estimates$summarised), 95)
  expect_equal(extract_CrIs(out$estimated_reported_cases$summarised), 95)
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
