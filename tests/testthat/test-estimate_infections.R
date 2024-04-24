# Setup for testing -------------------------------------------------------

futile.logger::flog.threshold("FATAL")

reported_cases <- EpiNow2::example_confirmed[1:30]

default_estimate_infections <- function(..., add_stan = list(), gt = TRUE,
                                        delay = TRUE) {
  futile.logger::flog.threshold("FATAL")

  def_stan <- list(
    chains = 2, warmup = 50, samples = 50,
    control = list(adapt_delta = 0.8)
  )
  def_stan <- modifyList(def_stan, add_stan)
  stan_args <- do.call(stan_opts, def_stan)

  suppressWarnings(estimate_infections(...,
    generation_time = fifelse(
      gt, generation_time_opts(example_generation_time), generation_time_opts()
    ),
    delays = ifelse(delay, list(delay_opts(example_reporting_delay)), list(delay_opts()))[[1]],
    stan = stan_args, verbose = FALSE
  ))
}

test_estimate_infections <- function(...) {
  out <- default_estimate_infections(...)
  expect_equal(names(out), c("samples", "summarised", "fit", "args", "observations"))
  expect_true(nrow(out$samples) > 0)
  expect_true(nrow(out$summarised) > 0)
  expect_true(nrow(out$observations) > 0)
  invisible(out)
}

# Test functionality ------------------------------------------------------

test_that("estimate_infections successfully returns estimates using default settings", {
  skip_on_cran()
  test_estimate_infections(reported_cases)
})

test_that("estimate_infections successfully returns estimates when passed NA values", {
  skip_on_cran()
  reported_cases_na <- data.table::copy(reported_cases)
  reported_cases_na[sample(1:30, 5), confirm := NA]
  test_estimate_infections(reported_cases_na)
})

test_that("estimate_infections successfully returns estimates when accumulating to weekly", {
  skip_on_cran()
  reported_cases_weekly <- data.table::copy(reported_cases)
  reported_cases_weekly[, confirm := frollsum(confirm, 7)]
  reported_cases_weekly <-
    reported_cases_weekly[seq(7, nrow(reported_cases_weekly), 7)]
  test_estimate_infections(reported_cases_weekly, obs = obs_opts(na = "accumulate"))
})

test_that("estimate_infections successfully returns estimates using no delays", {
  skip_on_cran()
  test_estimate_infections(reported_cases, delay = FALSE)
})
test_that("estimate_infections successfully returns estimates using the poisson observation model", {
  skip_on_cran()
  test_estimate_infections(reported_cases, obs = obs_opts(family = "poisson"))
})

test_that("estimate_infections successfully returns estimates using backcalculation", {
  skip_on_cran()
  test_estimate_infections(reported_cases, rt = NULL)
})

test_that("estimate_infections successfully returns estimates using a fixed Rt", {
  skip_on_cran()
  test_estimate_infections(reported_cases, gp = NULL)
})

test_that("estimate_infections successfully returns estimates using only mean shifted reported cases", {
  skip_on_cran()
  test_estimate_infections(reported_cases, gp = NULL, rt = NULL)
})

test_that("estimate_infections successfully returns estimates using a single breakpoint", {
  skip_on_cran()
  test_estimate_infections(data.table::copy(reported_cases)[, breakpoint := ifelse(date == "2020-03-10", 1, 0)],
    gp = NULL
  )
})


test_that("estimate_infections successfully returns estimates using a random walk", {
  skip_on_cran()
  test_estimate_infections(reported_cases, gp = NULL, rt = rt_opts(rw = 7))
})

test_that("estimate_infections works without setting a generation time", {
  skip_on_cran()
  df <- test_estimate_infections(reported_cases, gt = FALSE, delay = FALSE)
  ## check exp(r) == R
  growth_rate <- df$samples[variable == "growth_rate"][,
    list(date, sample, growth_rate = value)
  ]
  R <- df$samples[variable == "R"][,
    list(date, sample, R = value)
  ]
  combined <- merge(growth_rate, R, by = c("date", "sample"), all = FALSE)
  expect_equal(exp(combined$growth_rate), combined$R)
})

test_that("estimate_infections fails as expected when given a very short timeout", {
  skip_on_cran()
  expect_error(output <- capture.output(suppressMessages(
    out <- default_estimate_infections(
      reported_cases,
      add_stan = list(future = TRUE, max_execution_time = 1, samples = 2000)
  ))), "all chains failed")
  expect_error(output <- capture.output(suppressMessages(
    out <- default_estimate_infections(
      reported_cases,
      add_stan = list(future = FALSE, max_execution_time = 1, samples = 2000)
  ))), "timed out")
})


test_that("estimate_infections works as expected with failing chains", {
  skip_on_cran()
  test_estimate_infections(reported_cases,
    add_stan = list(
      chains = 4,
      stuck_chains = 2, future = TRUE,
      control = list(adapt_delta = 0.8)
    )
  )

  expect_error(default_estimate_infections(reported_cases,
    add_stan = list(chains = 4, stuck_chains = 1)
  ))
  expect_error(default_estimate_infections(reported_cases,
    add_stan = list(
      chains = 4,
      stuck_chains = 3,
      future = TRUE
    )
  ))
})

test_that("deprecated arguments are recognised", {
  expect_deprecated(
    estimate_infections(
      reported_cases = reported_cases,
      generation_time = generation_time_opts(Fixed(1)
    )
  )
})
