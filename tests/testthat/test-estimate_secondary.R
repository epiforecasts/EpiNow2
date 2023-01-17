 skip_on_cran()
 library(data.table)

 # make some example secondary incidence data
 cases <- example_confirmed
 cases <- as.data.table(cases)[, primary := confirm]
 # Assume that only 40 percent of cases are reported
 cases[, scaling := 0.4]
 # Parameters of the assumed log normal delay distribution
 cases[, meanlog := 1.8][, sdlog := 0.5]

 # apply a convolution of a log normal to a vector of observations
 weight_cmf <- function(x, ...) {
   set.seed(x[1])
   meanlog <- rnorm(1, 1.6, 0.2)
   sdlog <- rnorm(1, 0.8, 0.1)
   cmf <- cumsum(dlnorm(seq_along(x), meanlog, sdlog)) -
     cumsum(dlnorm(0:(length(x) - 1), meanlog, sdlog))
   cmf <- cmf / plnorm(length(x), meanlog, sdlog)
   conv <- sum(x * rev(cmf), na.rm = TRUE)
   conv <- round(conv, 0)
   return(conv)
 }
 # roll over observed cases to produce a convolution
 cases <- cases[, .(date, primary = confirm, secondary = confirm)]
 cases <- cases[, secondary := frollapply(secondary, 15, weight_cmf, align = "right")]
 cases <- cases[!is.na(secondary)]
 # add a day of the week effect and scale secondary observations at 40\% of primary
 cases <- cases[lubridate::wday(date) == 1, secondary := round(0.5 * secondary, 0)]
 cases <- cases[, secondary := round(secondary * rnorm(.N, 0.4, 0.025), 0)]
 cases <- cases[secondary < 0, secondary := 0]
 cases <- cases[, secondary := map_dbl(secondary, ~ rpois(1, .))]

 # fit model to example data assuming only a given fraction of primary observations
 # become secondary observations
 inc <- estimate_secondary(cases[1:60],
   obs = obs_opts(scale = list(mean = 0.2, sd = 0.2)),
   chains = 2, warmup = 250, iter = 750, cores = 2,
   refresh = 0
 )

test_that("estimate_secondary can return values from simulated data and plot
           them", {
  expect_equal(names(inc), c("predictions", "data", "fit"))
  expect_equal(
    names(inc$predictions),
    c("date", "primary", "secondary", "mean", "se_mean", "sd", "lower_90",
      "lower_50", "lower_20", "median", "upper_20", "upper_50", "upper_90"
    )
  )
  expect_true(is.list(inc$data))
  # validation plot of observations vs estimates
  expect_error(plot(inc, primary = TRUE), NA)
})

test_that("forecast_secondary can return values from simulated data and plot
           them", {
  inc_preds <- forecast_secondary(inc, cases[61:.N][, value := primary])
  expect_equal(names(inc_preds), c("samples", "forecast", "predictions"))
  # validation plot of observations vs estimates
  expect_error(plot(inc_preds, new_obs = cases, from = "2020-05-01"), NA)
})
