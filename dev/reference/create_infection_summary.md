# Create summary output from infection estimation objects

**\[stable\]**

This function creates summary output from infection estimation objects
(either `estimate_infections` or `forecast_infections`). It is used
internally by
[`summary.estimate_infections()`](https://epiforecasts.io/EpiNow2/dev/reference/summary.estimate_infections.md)
and
[`summary.forecast_infections()`](https://epiforecasts.io/EpiNow2/dev/reference/summary.forecast_infections.md)
to provide a consistent summary interface.

## Usage

``` r
create_infection_summary(
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

  An infection estimation object (either from
  [`estimate_infections()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_infections.md)
  or
  [`forecast_infections()`](https://epiforecasts.io/EpiNow2/dev/reference/forecast_infections.md)).

- type:

  A character vector of data types to return. Defaults to "snapshot" but
  also supports "parameters". "snapshot" returns a summary at a given
  date (by default the latest date informed by data). "parameters"
  returns summarised parameter estimates that can be further filtered
  using `params` to show just the parameters of interest and date.

- target_date:

  Date, defaults to maximum found in the data if not specified.

- params:

  A character vector of parameters to filter for.

- CrIs:

  Numeric vector of credible intervals to calculate. Defaults to c(0.2,
  0.5, 0.9).

- ...:

  Additional arguments passed to
  [`report_summary()`](https://epiforecasts.io/EpiNow2/dev/reference/report_summary.md).

## Value

A `<data.frame>` of summary output, either a snapshot summary (via
[`report_summary()`](https://epiforecasts.io/EpiNow2/dev/reference/report_summary.md))
or parameter summaries (via
[`calc_summary_measures()`](https://epiforecasts.io/EpiNow2/dev/reference/calc_summary_measures.md)).

## See also

[`summary.estimate_infections()`](https://epiforecasts.io/EpiNow2/dev/reference/summary.estimate_infections.md)
[`summary.forecast_infections()`](https://epiforecasts.io/EpiNow2/dev/reference/summary.forecast_infections.md)
[`report_summary()`](https://epiforecasts.io/EpiNow2/dev/reference/report_summary.md)
[`calc_summary_measures()`](https://epiforecasts.io/EpiNow2/dev/reference/calc_summary_measures.md)
