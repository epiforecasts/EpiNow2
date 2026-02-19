# Forecast Secondary Observations Given a Fit from estimate_secondary

**\[experimental\]** This function forecasts secondary observations
using the output of
[`estimate_secondary()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_secondary.md)
and either observed primary data or a forecast of primary observations.
See the examples of
[`estimate_secondary()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_secondary.md)
for one use case. It can also be combined with
[`estimate_infections()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_infections.md)
to produce a forecast for a secondary observation from a forecast of a
primary observation. See the examples of
[`estimate_secondary()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_secondary.md)
for example use cases on synthetic data. See
[here](https://gist.github.com/seabbs/4f09d7609df298db7a86c31612ff9d17)
for an example of forecasting Covid-19 deaths from Covid-19 cases.

## Usage

``` r
forecast_secondary(
  estimate,
  primary,
  primary_variable = "reported_cases",
  model = NULL,
  backend = "rstan",
  samples = NULL,
  all_dates = FALSE,
  CrIs = c(0.2, 0.5, 0.9)
)
```

## Arguments

- estimate:

  An object of class "estimate_secondary" as produced by
  [`estimate_secondary()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_secondary.md).

- primary:

  A `<data.frame>` containing at least `date` and `value` (integer)
  variables and optionally `sample`. Used as the primary observation
  used to forecast the secondary observations. Alternatively, this may
  be an object of class "estimate_infections" as produced by
  [`estimate_infections()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_infections.md).
  If `primary` is of class "estimate_infections" then the internal
  samples will be filtered to have a minimum date ahead of those
  observed in the `estimate` object.

- primary_variable:

  A character string indicating the primary variable, defaulting to
  "reported_cases". Only used when primary is of class
  `<estimate_infections>`.

- model:

  A compiled stan model as returned by
  [`rstan::stan_model()`](https://mc-stan.org/rstan/reference/stan_model.html).

- backend:

  Character string indicating the backend to use for fitting stan
  models. Supported arguments are "rstan" (default) or "cmdstanr".

- samples:

  Numeric, number of posterior samples to simulate from. The default is
  to use all samples in the `primary` input when present. If not present
  the default is to use 1000 samples.

- all_dates:

  Logical, defaults to FALSE. Should a forecast for all dates and not
  just those in the forecast horizon be returned.

- CrIs:

  Numeric vector of credible intervals to calculate.

## Value

A list containing: `predictions` (a `<data.frame>` ordered by date with
the primary, and secondary observations, and a summary of the forecast
secondary observations. For primary observations in the forecast horizon
when uncertainty is present the median is used), `samples` a
`<data.frame>` of forecast secondary observation posterior samples, and
`forecast` a summary of the forecast secondary observation posterior.

## See also

[`estimate_secondary()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_secondary.md)
