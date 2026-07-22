# Extract parameter samples from a Stan model

**\[deprecated\]** This function has been deprecated. Use
[`format_simulation_output()`](https://epiforecasts.io/EpiNow2/dev/reference/format_simulation_output.md)
for simulation outputs or
[`get_samples()`](https://epiforecasts.io/EpiNow2/dev/reference/get_samples.md)
for estimation outputs instead.

## Usage

``` r
extract_parameter_samples(
  stan_fit,
  data,
  reported_dates,
  imputed_dates,
  reported_inf_dates,
  drop_length_1 = FALSE,
  merge = FALSE
)
```

## Arguments

- stan_fit:

  A `<stanfit>` or `<CmdStanMCMC>` object as returned by
  [`fit_model()`](https://epiforecasts.io/EpiNow2/dev/reference/fit_model.md).

- data:

  A list of the data supplied to the simulation.

- reported_dates:

  A vector of dates to report estimates for.

- imputed_dates:

  A vector of dates to report imputed reports for.

- reported_inf_dates:

  A vector of dates to report infection estimates for.

- drop_length_1:

  Logical; drop dimensions of length 1 in arrays extracted from the stan
  fit. Used in simulations where there's only 1 realization.

- merge:

  if TRUE, merge samples into a single data.table using rbindlist. If
  FALSE returns a list of samples by parameter.

## Value

A list of `<data.frame>`'s each containing the posterior of a parameter
