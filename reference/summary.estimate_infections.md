# Summary output from estimate_infections

**\[stable\]** `summary` method for class "estimate_infections".

## Usage

``` r
# S3 method for class 'estimate_infections'
summary(
  object,
  type = c("snapshot", "parameters"),
  target_date = NULL,
  params = NULL,
  CrIs = c(0.2, 0.5, 0.9),
  ...
)
```

## Arguments

- object:

  A list of output as produced by "estimate_infections".

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

  Pass additional arguments to `report_summary` when
  `type = "snapshot"`.

## Value

Returns a `<data.frame>` of summary output

## See also

[`summary.epinow()`](https://epiforecasts.io/EpiNow2/reference/summary.epinow.md)
[`estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.md)
[`report_summary()`](https://epiforecasts.io/EpiNow2/reference/report_summary.md)
