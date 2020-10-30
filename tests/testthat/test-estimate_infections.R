context("estimate_infections")


# Setup for testing -------------------------------------------------------

futile.logger::flog.threshold("FATAL")

reported_cases <- EpiNow2::example_confirmed[1:30]
generation_time <- get_generation_time(disease = "SARS-CoV-2", source = "ganyani", max_value = 10)
incubation_period <- get_incubation_period(disease = "SARS-CoV-2", source = "lauer", max_value = 10)
reporting_delay <- list(mean = log(3), mean_sd = 0.1, sd = log(2), sd_sd = 0.1, max = 10)

default_estimate_infections <- function(..., add_stan = list()) {
  
  def_stan <- list(chains = 2, warmup = 50,
                   control = list(adapt_delta = 0.8), fit = NULL)
  stan_args <- def_stan[setdiff(names(def_stan), names(add_stan))]
  stan_args <- c(stan_args, add_stan)
  
  suppressWarnings(estimate_infections(...,
    generation_time = generation_time,
    delays = list(reporting_delay), samples = 50, 
    stan_args = stan_args))
}

test_estimate_infections <- function(...) {
    out <- default_estimate_infections(...)
    expect_equal(names(out), c("samples", "summarised", "observations"))
    expect_true(nrow(out$samples) > 0)
    expect_true(nrow(out$summarised) > 0)
    expect_true(nrow(out$observations) > 0)
}

# Test functionality ------------------------------------------------------

test_that("estimate_infections successfully returns estimates using default settings", {
  skip_on_cran()
  test_estimate_infections(reported_cases)
})

test_that("estimate_infections successfully returns estimates using default settings", {
  skip_on_cran()
  test_estimate_infections(reported_cases, stan_configuration = list(backend = "cmdstan"))
})

test_that("estimate_infections successfully returns estimates using backcalculation", {
  skip_on_cran()
  test_estimate_infections(reported_cases, rt_prior = list())
})

test_that("estimate_infections successfully returns estimates using default settings", {
  skip_on_cran()
  test_estimate_infections(reported_cases, gp = list())
})

test_that("estimate_infections successfully returns estimates using only mean shifted reported cases", {
  skip_on_cran()
  test_estimate_infections(reported_cases, gp = list(), rt_prior = list())
})
             
test_that("estimate_infections successfully returns estimates using a single breakpoint", {
  skip_on_cran()
  tmp <- default_estimate_infections(data.table::copy(reported_cases)[, breakpoint := ifelse(date == "2020-03-10", 1, 0)],
                           gp = list())
})


test_that("estimate_infections fails as expected when given a very short timeout", {
  skip_on_cran()
  expect_error(default_estimate_infections(reported_cases, future = TRUE, max_execution_time = 1))
  expect_error(default_estimate_infections(reported_cases, future = FALSE, max_execution_time = 1))
})


test_that("estimate_infections works as expected with failing chains", {
  skip_on_cran()
  test_estimate_infections(reported_cases, future = TRUE,
                           add_stan = list(chains = 4, warmup = 100,
                                           stuck_chains = 2,
                                           control = list(adapt_delta = 0.8)))
  
  expect_error(default_estimate_infections(reported_cases, generation_time = generation_time,
                                                    delays = list(reporting_delay), samples = 1,
                                                    stan_args = list(chains = 4, warmup = 1,
                                                                     stuck_chains = 1)))
  expect_error(default_estimate_infections(reported_cases,
                                           add_stan = list(chains = 4, warmup = 1,
                                                           stuck_chains = 3),
                                           future = TRUE))
  
  expect_error(default_estimate_infections(reported_cases,
                                           add_stan = list(chains = 4, warmup = 1,
                                                           stuck_chains = 3),
                                           stan_configuration = list(backend = "cmdstan"),
                                           future = TRUE))
})


# test_that("estimate_infections produces output as expected when using approximate approach", {
#   skip_on_cran()
#   out <- suppressWarnings(estimate_infections(reported_cases, generation_time = generation_time,
#                                               delays = list(reporting_delay),
#                                               samples = 1000,
#                                               stan_args = list(trials = 20,
#                                                                algorithm = "meanfield")))
#   expect_equal(names(out), c("samples", "summarised"))
#   expect_true(nrow(out$samples) > 0)
#   expect_true(nrow(out$summarised) > 0)
# })
