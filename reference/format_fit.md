# Format Posterior Samples

**\[stable\]** Summaries posterior samples and adds additional custom
variables.

## Usage

``` r
format_fit(posterior_samples, horizon, shift, burn_in, start_date, CrIs)
```

## Arguments

- posterior_samples:

  A list of posterior samples as returned by
  [`format_simulation_output()`](https://epiforecasts.io/EpiNow2/reference/format_simulation_output.md).

- horizon:

  Numeric, forecast horizon.

- shift:

  Numeric, the shift to apply to estimates.

- burn_in:

  Deprecated; this functionality is no longer available.

- start_date:

  Deprecated; this functionality is no longer available.

- CrIs:

  Numeric vector of credible intervals to calculate.

## Value

A list of samples and summarised posterior parameter estimates.
