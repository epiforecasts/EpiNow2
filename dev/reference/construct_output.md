# Construct Output

**\[stable\]** Combines the output produced internally by `epinow` into
a single list.

## Usage

``` r
construct_output(
  estimates,
  estimated_reported_cases,
  plots = NULL,
  summary = NULL,
  samples = TRUE,
  CrIs = c(0.2, 0.5, 0.9)
)
```

## Arguments

- estimates:

  List of data frames as output by `estimate_infections`

- estimated_reported_cases:

  A list of dataframes as produced by `estimates_by_report_date`.

- plots:

  A list of plots as produced by `report_plots`.

- summary:

  A list of summary output as produced by `report_summary`.

- samples:

  Logical, defaults to TRUE. Should samples be saved

- CrIs:

  Numeric vector of credible intervals to calculate.

## Value

A list of output as returned by `epinow`
