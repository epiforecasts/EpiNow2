# Plot method for estimate_infections

**\[maturing\]** `plot` method for class `<estimate_infections>`.

## Usage

``` r
# S3 method for class 'estimate_infections'
plot(x, type = "summary", CrIs = c(0.2, 0.5, 0.9), ...)
```

## Arguments

- x:

  A list of output as produced by `estimate_infections`

- type:

  A character vector indicating the name of the plot to return. Defaults
  to "summary" with supported options being "infections", "reports",
  "R", "growth_rate", "summary", "all". If "all" is supplied all plots
  are generated.

- CrIs:

  Numeric vector of credible intervals to calculate.

- ...:

  Pass additional arguments to report_plots

## Value

List of plots as produced by
[`report_plots()`](https://epiforecasts.io/EpiNow2/reference/report_plots.md)

## See also

[`report_plots()`](https://epiforecasts.io/EpiNow2/reference/report_plots.md)

## Examples

``` r
# get example output
out <- readRDS(system.file(
  package = "EpiNow2", "extdata", "example_estimate_infections.rds"
))

# plot with error bars instead of ribbons
plot(out, style = "linerange")
```
