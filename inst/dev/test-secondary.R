
options(mc.cores = ifelse(interactive(), 4, 1))

library(data.table)
library(lubridate)
library(purrr)
library(rstan)

cases <- example_confirmed
cases <- as.data.table(cases)

# apply a convolution of a log normal to a vector of observations
weight_cmf <- function(x, ...) {
  set.seed(x[1])
  meanlog <- rnorm(1, 1.6, 0.05)
  sdlog <- rnorm(1, 0.8, 0.01)
  cmf <- cumsum(dlnorm(1:length(x), meanlog, sdlog)) -
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

# compile the model (for dev testing)
model <- stan_model("inst/stan/estimate_secondary.stan")

# fit model to example data assuming only a given fraction of primary observations
# become secondary observations
inc <- estimate_secondary(cases[1:60], model = model,
  delays = delay_opts(
    list(mean = 2.5, mean_sd = 0.5,
         sd = 0.5, sd_sd = 0.25, max = 15)),
  obs = obs_opts(scale = list(mean = 0.2, sd = 0.2),
                 family = "poisson")
)
plot(inc, primary = TRUE)

# allow parameters to vary over time
inc_vary <- estimate_secondary(
  cases[1:60], model = model,
  delays = delay_opts(
    list(mean = 2.5, mean_sd = 0.5,
         sd = 0.5, sd_sd = 0.25, max = 15),
    mean =  list(gp_opts(order = "0", basis_prop = 0.025,
                  ls_mean = 55, ls_sd = 2, alpha_sd = 0.01)),
    sd =  list(gp_opts(order = "0", basis_prop = 0.025,
                       ls_mean = 55, ls_sd = 2, alpha_sd = 0.01))),
  obs = obs_opts(scale = list(mean = 0.2, sd = 0.2), family = "poisson",
                 gp = gp_opts(order = "1", basis_prop = 0.025,
                              ls_mean = 44, ls_sd = 2, alpha_sd = 0.01)),
  control = list(adapt_delta = 0.95)
)

plot(inc_vary, primary = TRUE)