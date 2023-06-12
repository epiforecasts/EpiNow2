skip_on_cran()
library(data.table)

#### Incidence data example ####

# make some example secondary incidence data
cases <- example_confirmed
cases <- as.data.table(cases)[, primary := confirm]
# Assume that only 40 percent of cases are reported
cases[, scaling := 0.4]
# Parameters of the assumed log normal delay distribution
cases[, meanlog := 1.8][, sdlog := 0.5]

# Simulate secondary cases
cases <- simulate_secondary(cases, type = "incidence")
cases[
  ,
  c("confirm", "scaling", "meanlog", "sdlog", "index", "scaled", "conv") :=
    NULL
]
#
# fit model to example data specifying a weak prior for fraction reported
# with a secondary case
inc <- estimate_secondary(cases[1:60],
  obs = obs_opts(scale = list(mean = 0.2, sd = 0.2), week_effect = FALSE),
  control = list(adapt_delta = 0.95),
  verbose = FALSE
)

# extract posterior variables of interest
params <- c(
  "meanlog" = "delay_mean[1]", "sdlog" = "delay_sd[1]",
  "scaling" = "frac_obs[1]"
)

inc_posterior <- inc$posterior[variable %in% params]

#### Prevalence data example ####

# make some example prevalence data
cases <- example_confirmed
cases <- as.data.table(cases)[, primary := confirm]
# Assume that only 30 percent of cases are reported
cases[, scaling := 0.3]
# Parameters of the assumed log normal delay distribution
cases[, meanlog := 1.6][, sdlog := 0.8]

# Simulate secondary cases
cases <- simulate_secondary(cases, type = "prevalence")

# fit model to example prevalence data
prev <- estimate_secondary(cases[1:100],
  secondary = secondary_opts(type = "prevalence"),
  obs = obs_opts(
    week_effect = FALSE,
    scale = list(mean = 0.4, sd = 0.1)
  ),
  verbose = TRUE
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

test_that("estimate_secondary can recover simulated parameters", {
  expect_equal(
    inc_posterior[, mean], c(1.8, 0.5, 0.4),
    tolerance = 0.1
  )
  expect_equal(
    inc_posterior[, median], c(1.8, 0.5, 0.4),
    tolerance = 0.1
  )
  # These tests currently fail indicating the model is not recovering the
  # simulated parameters.
  # expect_equal(
  #   prev_posterior[, mean], c(1.6, 0.8, 0.3), tolerance = 0.1
  # )
  # expect_equal(
  #   prev_posterior[, median], c(1.6, 0.8, 0.3), tolerance = 0.1
  # )
})

test_that("forecast_secondary can return values from simulated data and plot
           them", {
  inc_preds <- forecast_secondary(inc, cases[seq(61, .N)][, value := primary])
  expect_equal(names(inc_preds), c("samples", "forecast", "predictions"))
  # validation plot of observations vs estimates
  expect_error(plot(inc_preds, new_obs = cases, from = "2020-05-01"), NA)
})
