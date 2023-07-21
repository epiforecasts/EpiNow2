# Setup for testing -------------------------------------------------------

futile.logger::flog.threshold("FATAL")

reported_cases <- EpiNow2::example_confirmed[1:30]
generation_time <- get_generation_time(disease = "SARS-CoV-2", source = "ganyani", max_value = 10)
incubation_period <- get_incubation_period(disease = "SARS-CoV-2", source = "lauer", max_value = 10)
reporting_delay <- dist_spec(
  mean = convert_to_logmean(2, 1), mean_sd = 0.1,
  sd = convert_to_logsd(2, 1), sd_sd = 0.1, max = 10
)

default_estimate_infections <- function(..., add_stan = list(), delay = TRUE) {
  futile.logger::flog.threshold("FATAL")

  def_stan <- stan_opts(
    chains = 2, warmup = 50, samples = 50,
    control = list(adapt_delta = 0.8)
  )
  stan_args <- def_stan[setdiff(names(def_stan), names(add_stan))]
  stan_args <- c(stan_args, add_stan)

  suppressWarnings(estimate_infections(...,
    generation_time = generation_time_opts(generation_time),
    delays = ifelse(delay, list(delay_opts(reporting_delay)), list(delay_opts()))[[1]],
    stan = stan_args, verbose = FALSE
  ))
}

test_estimate_infections <- function(...) {
  out <- default_estimate_infections(...)
  expect_equal(names(out), c("samples", "summarised", "fit", "args", "observations"))
  expect_true(nrow(out$samples) > 0)
  expect_true(nrow(out$summarised) > 0)
  expect_true(nrow(out$observations) > 0)
}

# Test functionality ------------------------------------------------------

test_that("estimate_infections successfully returns estimates using default settings", {
  skip_on_cran()
  test_estimate_infections(reported_cases)
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

test_that("estimate_infections fails as expected when given a very short timeout", {
  skip_on_cran()
  expect_error(default_estimate_infections(
    verbose = FALSE, reported_cases,
    add_stan = list(future = TRUE, max_execution_time = 1))
  )
  expect_error(default_estimate_infections(
    verbose = FALSE, reported_cases,
    add_stan = list(future = FALSE, max_execution_time = 1))
  )
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
