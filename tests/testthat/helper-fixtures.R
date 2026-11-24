# Shared test fixtures -----------------------------------------------------
# Run regional_epinow() once and reuse output for all downstream tests.
# This avoids running MCMC multiple times while testing the full pipeline.

#' Get shared test fixtures
#'
#' Runs regional_epinow() once (lazily) and caches the result.
#' This runs regardless of integration test settings to provide fixtures
#' for downstream tests. Note: code here won't be captured by coverage
#' tools since it's outside test_that() blocks.
#'
#' @return List with regional_epinow output and extracted estimate_infections
#'   objects
get_test_fixtures <- local({
  fixtures <- NULL
  function() {
    if (is.null(fixtures)) {
      futile.logger::flog.threshold("FATAL")

      # Create test data with 2 regions
      cases <- EpiNow2::example_confirmed[1:30]
      cases <- data.table::rbindlist(list(
        data.table::copy(cases)[, region := "testland"],
        data.table::copy(cases)[, region := "realland"]
      ))

      # Run regional_epinow once with estimate_infections output
      suppressWarnings(suppressMessages({
        regional_out <- regional_epinow(
          data = cases,
          generation_time = gt_opts(example_generation_time),
          delays = delay_opts(example_incubation_period + example_reporting_delay),
          rt = rt_opts(prior = GP(init = LogNormal(mean = 2, sd = 0.2))),
          stan = stan_opts(
            samples = 25, warmup = 25,
            chains = 2, cores = 1,
            control = list(adapt_delta = 0.8)
          ),
          output = c(
            "regions", "summary", "samples", "plots", "timing",
            "estimate_infections"
          ),
          verbose = FALSE
        )
      }))

      fixtures <<- list(
        regional = regional_out,
        estimate_infections = regional_out$regional$testland,
        estimate_infections_alt = regional_out$regional$realland
      )
    }
    fixtures
  }
})
