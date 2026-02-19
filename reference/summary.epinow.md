# Summary output from epinow

**\[stable\]** `summary` method for class "epinow". This method inherits
from
[`summary.estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/summary.estimate_infections.md)
and supports the same arguments.

## Usage

``` r
# S3 method for class 'epinow'
summary(
  object,
  output = NULL,
  type = c("snapshot", "parameters"),
  target_date = NULL,
  params = NULL,
  CrIs = c(0.2, 0.5, 0.9),
  ...
)
```

## Arguments

- object:

  An `<epinow>` object as produced by
  [`epinow()`](https://epiforecasts.io/EpiNow2/reference/epinow.md).

- output:

  **\[deprecated\]** Use the `type` argument instead. Previously
  supported "estimates", "forecast", and "estimated_reported_cases".

- type:

  A character vector of data types to return. Defaults to "snapshot" but
  also supports "parameters". "snapshot" returns a summary at a given
  date (by default the latest date informed by data). "parameters"
  returns summarised parameter estimates that can be further filtered
  using `params` to show just the parameters of interest and date.

  Note: `type = "samples"` is deprecated. Use
  [`get_samples()`](https://epiforecasts.io/EpiNow2/reference/get_samples.md)
  instead.

- target_date:

  Date, defaults to maximum found in the data if not specified.

- params:

  A character vector of parameters to filter for.

- CrIs:

  Numeric vector of credible intervals to calculate.

- ...:

  Pass additional summary arguments to
  [`summary.estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/summary.estimate_infections.md)

## Value

Returns a `<data.frame>` of summary output

## See also

[`summary.estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/summary.estimate_infections.md)
[`epinow()`](https://epiforecasts.io/EpiNow2/reference/epinow.md)
