skip_on_cran()

#### Incidence data example ####

# make some example secondary incidence data
cases <- example_confirmed
cases <- as.data.table(cases)[, primary := confirm]

inc_cases <- copy(cases)
# Assume that only 40 percent of cases are reported
inc_cases[, scaling := 0.4]
# Parameters of the assumed log normal delay distribution
inc_cases[, meanlog := 1.8][, sdlog := 0.5]

# Simulate secondary cases
inc_cases <- convolve_and_scale(inc_cases, type = "incidence")
inc_cases[
  ,
  c("confirm", "scaling", "meanlog", "sdlog", "index", "scaled", "conv") :=
    NULL
]
#
# fit model to example data specifying a weak prior for fraction reported
# with a secondary case
inc <- estimate_secondary(inc_cases[1:60],
  obs = obs_opts(scale = list(mean = 0.2, sd = 0.2), week_effect = FALSE),
  verbose = FALSE
)

# extract posterior variables of interest
params <- c(
  "meanlog" = "delay_params[1]", "sdlog" = "delay_params[2]",
  "scaling" = "frac_obs[1]"
)

inc_posterior <- inc$posterior[variable %in% params]

# fit model to example data with a fixed delay
inc_fixed <- estimate_secondary(inc_cases[1:60],
  delays = delay_opts(Gamma(mean = 15, sd = 5, max = 30)),
  verbose = FALSE
)

#### Prevalence data example ####

# make some example prevalence data
prev_cases <- copy(cases)
# Assume that only 30 percent of cases are reported
prev_cases[, scaling := 0.3]
# Parameters of the assumed log normal delay distribution
prev_cases[, meanlog := 1.6][, sdlog := 0.8]

# Simulate secondary cases
prev_cases <- convolve_and_scale(prev_cases, type = "prevalence")

# fit model to example prevalence data
prev <- estimate_secondary(prev_cases[1:100],
  secondary = secondary_opts(type = "prevalence"),
  obs = obs_opts(
    week_effect = FALSE,
    scale = list(mean = 0.4, sd = 0.1)
  ),
  verbose = FALSE
)

# extract posterior parameters of interest
prev_posterior <- prev$posterior[variable %in% params]

# Test output
test_that("estimate_secondary can return values from simulated data and plot
           them", {
  expect_equal(names(inc), c("predictions", "posterior", "data", "fit"))
  expect_equal(
    names(inc$predictions),
    c(
      "date", "primary", "secondary", "mean", "se_mean", "sd",
      "lower_90", "lower_50", "lower_20", "median", "upper_20", "upper_50", "upper_90"
    )
  )
  expect_true(is.list(inc$data))
  # validation plot of observations vs estimates
  expect_error(plot(inc, primary = TRUE), NA)
})

test_that("estimate_secondary successfully returns estimates when passed NA values", {
  skip_on_cran()
  cases_na <- data.table::copy(inc_cases)
  cases_na[sample(1:60, 5), secondary := NA]
  inc_na <- estimate_secondary(cases_na[1:60],
    delays = delay_opts(
      LogNormal(meanlog = 1.8, sdlog = 0.5, max = 30)
    ),
    obs = obs_opts(scale = list(mean = 0.2, sd = 0.2), week_effect = FALSE),
    verbose = FALSE
  )
  prev_cases_na <- data.table::copy(prev_cases)
  prev_cases_na[sample(1:60, 5), secondary := NA]
  prev_na <- estimate_secondary(prev_cases_na[1:60],
    secondary = secondary_opts(type = "prevalence"),
    delays = delay_opts(
      LogNormal(mean = 1.8, sd = 0.5, max = 30)
    ),
    obs = obs_opts(scale = list(mean = 0.2, sd = 0.2), week_effect = FALSE),
    verbose = FALSE
  )
  expect_true(is.list(inc_na$data))
  expect_true(is.list(prev_na$data))
})

test_that("estimate_secondary successfully returns estimates when accumulating to weekly", {
  skip_on_cran()
  secondary_weekly <- inc_cases[, list(date, secondary)]
  secondary_weekly[, secondary := frollsum(secondary, 7)]
  secondary_weekly <- secondary_weekly[seq(7, nrow(secondary_weekly), by = 7)]
  cases_weekly <- merge(
    cases[, list(date, primary)], secondary_weekly, by = "date", all.x = TRUE
  )
  inc_weekly <- estimate_secondary(cases_weekly,
    delays = delay_opts(
      LogNormal(
        mean = 1.8, sd = 0.5, max = 30
      )
    ),
    obs = obs_opts(
      scale = list(mean = 0.4, sd = 0.05), week_effect = FALSE, na = "accumulate"
    ), verbose = FALSE
  )
  expect_true(is.list(inc_weekly$data))
})

test_that("estimate_secondary works when only estimating scaling", {
  inc <- estimate_secondary(inc_cases[1:60],
    obs = obs_opts(scale = list(mean = 0.2, sd = 0.2), week_effect = FALSE),
    delay = delay_opts(),
    verbose = FALSE
  )
  expect_equal(names(inc), c("predictions", "posterior", "data", "fit"))
})

test_that("estimate_secondary can recover simulated parameters", {
  expect_equal(
    inc_posterior[, mean], c(1.8, 0.5, 0.4),
    tolerance = 0.1
  )
  expect_equal(
    inc_posterior[, median], c(1.8, 0.5, 0.4),
    tolerance = 0.1
  )
  expect_equal(
    prev_posterior[, mean], c(1.6, 0.8, 0.3), tolerance = 0.2
  )
  expect_equal(
    prev_posterior[, median], c(1.6, 0.8, 0.3), tolerance = 0.2
  )
})

test_that("estimate_secondary can recover simulated parameters with the
           cmdstanr backend", {
  skip_on_os("windows")
  output <- capture.output(suppressMessages(suppressWarnings(
    inc_cmdstanr <- estimate_secondary(inc_cases[1:60],
      obs = obs_opts(scale = list(mean = 0.2, sd = 0.2), week_effect = FALSE),
      verbose = FALSE, stan = stan_opts(backend = "cmdstanr")
    )
  )))
  inc_posterior_cmdstanr <- inc_cmdstanr$posterior[variable %in% params]
  expect_equal(
    inc_posterior_cmdstanr[, mean], c(1.8, 0.5, 0.4),
    tolerance = 0.1
  )
  expect_equal(
    inc_posterior_cmdstanr[, median], c(1.8, 0.5, 0.4),
    tolerance = 0.1
  )
})

test_that("forecast_secondary can return values from simulated data and plot
           them", {
  inc_preds <- forecast_secondary(
    inc, inc_cases[seq(61, .N)][, value := primary]
  )
  expect_equal(names(inc_preds), c("samples", "forecast", "predictions"))
  # validation plot of observations vs estimates
  expect_error(plot(inc_preds, new_obs = inc_cases, from = "2020-05-01"), NA)
})

test_that("forecast_secondary works with fixed delays", {
  inc_preds <- forecast_secondary(
    inc_fixed, inc_cases[seq(61, .N)][, value := primary]
  )
  expect_equal(names(inc_preds), c("samples", "forecast", "predictions"))
  # validation plot of observations vs estimates
  expect_error(plot(inc_preds, new_obs = inc_cases, from = "2020-05-01"), NA)
})

test_that("forecast_secondary can return values from simulated data when using
           the cmdstanr backend", {
  skip_on_os("windows")
  capture.output(suppressMessages(suppressWarnings(
    inc_preds <- forecast_secondary(
      inc, inc_cases[seq(61, .N)][, value := primary], backend = "cmdstanr"
    )
  )))
  expect_equal(names(inc_preds), c("samples", "forecast", "predictions"))
})

test_that("estimate_secondary works with weigh_delay_priors = TRUE", {
  delays <- LogNormal(
    meanlog = Normal(2.5, 0.5),
    sdlog = Normal(0.47, 0.25),
    max = 30
  )
  inc_weigh <- estimate_secondary(
    inc_cases[1:60], delays = delay_opts(delays),
    obs = obs_opts(scale = list(mean = 0.2, sd = 0.2), week_effect = FALSE),
    weigh_delay_priors = TRUE, verbose = FALSE
  )
  expect_s3_class(inc_weigh, "estimate_secondary")
})

test_that("estimate_secondary works with filter_leading_zeros set", {
  modified_data <- inc_cases[1:10, secondary := 0]
  out <- estimate_secondary(
    modified_data,
    obs = obs_opts(scale = list(mean = 0.2, sd = 0.2),
    week_effect = FALSE),
    filter_leading_zeros = TRUE,
    verbose = FALSE
  )
  expect_s3_class(out, "estimate_secondary")
  expect_named(out, c("predictions", "posterior", "data", "fit"))
  expect_equal(out$predictions$primary, modified_data$primary[-(1:10)])
})

test_that("estimate_secondary works with zero_threshold set", {
  modified_data <- inc_cases[sample(1:30, 10), primary := 0]
  out <- estimate_secondary(
    modified_data,
    obs = obs_opts(scale = list(mean = 0.2, sd = 0.2),
                   week_effect = FALSE),
    zero_threshold = 10,
    verbose = FALSE
  )
  expect_s3_class(out, "estimate_secondary")
  expect_named(out, c("predictions", "posterior", "data", "fit"))
})
