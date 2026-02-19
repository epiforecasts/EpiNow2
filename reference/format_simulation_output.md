# Format Simulation Output from Stan

**\[stable\]** Formats simulation output from Stan models into
structured data.tables with dates. This is an internal function used by
[`simulate_infections()`](https://epiforecasts.io/EpiNow2/reference/simulate_infections.md)
and
[`forecast_infections()`](https://epiforecasts.io/EpiNow2/reference/forecast_infections.md)
to process simulation results.

This differs from
[`get_samples()`](https://epiforecasts.io/EpiNow2/reference/get_samples.md)
in that it's designed for simulation outputs which have different array
structures (especially with `drop_length_1 = TRUE`) and need different
date ranges for different parameters.

## Usage

``` r
format_simulation_output(
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
  [`fit_model()`](https://epiforecasts.io/EpiNow2/reference/fit_model.md).

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

A list of `<data.frame>`'s each containing the simulated trajectories of
each parameter, or a single merged data.table if merge = TRUE.
