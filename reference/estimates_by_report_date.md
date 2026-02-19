# Estimate Cases by Report Date

**\[questioning\]** Either extracts or converts reported cases from an
input data table. For output from `estimate_infections` this is a simple
filtering step.

## Usage

``` r
estimates_by_report_date(
  estimates,
  CrIs = c(0.2, 0.5, 0.9),
  target_folder = NULL,
  samples = TRUE
)
```

## Arguments

- estimates:

  List of data frames as output by `estimate_infections`

- CrIs:

  Numeric vector of credible intervals to calculate.

- target_folder:

  Character string specifying where to save results (will create if not
  present).

- samples:

  Logical, defaults to TRUE. Should samples be saved

## Value

A list of samples and summarised estimates of estimated cases by date of
report.
