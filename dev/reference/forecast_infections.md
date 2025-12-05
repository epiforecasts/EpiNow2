# Forecast infections from a given fit and trajectory of the time-varying reproduction number

**\[stable\]** This function simulates infections using an existing fit
to observed cases but with a modified time-varying reproduction number.
This can be used to explore forecast models or past counterfactuals.
Simulations can be run in parallel using
[`future::plan()`](https://future.futureverse.org/reference/plan.html).

## Usage

``` r
forecast_infections(
  estimates,
  R = NULL,
  model = NULL,
  samples = NULL,
  batch_size = 10,
  backend = "rstan",
  verbose = interactive()
)
```

## Arguments

- estimates:

  The `estimates` element of an
  [`epinow()`](https://epiforecasts.io/EpiNow2/dev/reference/epinow.md)
  run that has been done with output = "fit", or the result of
  [`estimate_infections()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_infections.md)
  with `return_fit` set to TRUE.

- R:

  A numeric vector of reproduction numbers; these will overwrite the
  reproduction numbers contained in `estimates`, except elements set to
  NA. Alternatively accepts a `<data.frame>` containing at least `date`
  and `value` (integer) variables and optionally `sample`. More (or
  fewer) days than in the original fit can be simulated.

- model:

  A compiled stan model as returned by
  [`rstan::stan_model()`](https://rdrr.io/pkg/rstan/man/stan_model.html).

- samples:

  Numeric, number of posterior samples to simulate from. The default is
  to use all samples in the `estimates` input.

- batch_size:

  Numeric, defaults to 10. Size of batches in which to simulate. May
  decrease run times due to reduced IO costs but this is still being
  evaluated. If set to NULL then all simulations are done at once.

- backend:

  Character string indicating the backend to use for fitting stan
  models. Supported arguments are "rstan" (default) or "cmdstanr".

- verbose:

  Logical defaults to
  [`interactive()`](https://rdrr.io/r/base/interactive.html). If the
  `progressr` package is available, a progress bar will be shown.

## Value

A `<forecast_infections>` object containing simulated infections and
cases from the specified scenario. The structure is similar to
[`estimate_infections()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_infections.md)
output but contains `samples` rather than `fit`.

## See also

[`generation_time_opts()`](https://epiforecasts.io/EpiNow2/dev/reference/generation_time_opts.md)
[`delay_opts()`](https://epiforecasts.io/EpiNow2/dev/reference/delay_opts.md)
[`rt_opts()`](https://epiforecasts.io/EpiNow2/dev/reference/rt_opts.md)
[`estimate_infections()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_infections.md)
[`trunc_opts()`](https://epiforecasts.io/EpiNow2/dev/reference/trunc_opts.md)
[`stan_opts()`](https://epiforecasts.io/EpiNow2/dev/reference/stan_opts.md)
[`obs_opts()`](https://epiforecasts.io/EpiNow2/dev/reference/obs_opts.md)
[`gp_opts()`](https://epiforecasts.io/EpiNow2/dev/reference/gp_opts.md)

## Examples

``` r
# \donttest{
# set number of cores to use
old_opts <- options()
options(mc.cores = ifelse(interactive(), 4, 1))

# get example case counts
reported_cases <- example_confirmed[1:50]

# fit model to data to recover Rt estimates
est <- estimate_infections(reported_cases,
  generation_time = generation_time_opts(example_generation_time),
  delays = delay_opts(example_incubation_period + example_reporting_delay),
  rt = rt_opts(prior = LogNormal(mean = 2, sd = 0.1), rw = 7),
  obs = obs_opts(scale = Normal(mean = 0.1, sd = 0.01)),
  gp = NULL,
  forecast = forecast_opts(horizon = 0)
)

# update Rt trajectory and simulate new infections using it
R <- c(rep(NA_real_, 26), rep(0.5, 10), rep(0.8, 14))
sims <- forecast_infections(est, R)
plot(sims)


# with a data.frame input of samples
R_dt <- data.frame(
  date = seq(
    min(summary(est, type = "parameters", param = "R")$date),
    by = "day", length.out = length(R)
  ),
  value = R
)
sims <- forecast_infections(est, R_dt)
plot(sims)


#' # with a data.frame input of samples
R_samples <- get_samples(est)[variable == "R"]
R_samples <- R_samples[
  ,
  .(date, sample, value)
][sample <= 1000][date <= "2020-04-10"]
R_samples <- R_samples[date >= "2020-04-01", value := 1.1]
sims <- forecast_infections(est, R_samples)
plot(sims)


options(old_opts)
# }
```
