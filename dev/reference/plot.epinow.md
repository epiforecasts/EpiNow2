# Plot method for epinow

**\[maturing\]** `plot` method for class `<epinow>`.

## Usage

``` r
# S3 method for class 'epinow'
plot(x, type = "summary", ...)
```

## Arguments

- x:

  A list of output as produced by
  [`epinow()`](https://epiforecasts.io/EpiNow2/dev/reference/epinow.md).

- type:

  A character vector indicating the name of the plot to return. Defaults
  to "summary" with supported options being "infections", "reports",
  "R", "growth_rate", "summary", "all". If "all" is supplied all plots
  are generated.

- ...:

  Pass additional arguments to report_plots

## Value

List of plots as produced by
[`report_plots()`](https://epiforecasts.io/EpiNow2/dev/reference/report_plots.md)

## See also

[`plot.estimate_infections()`](https://epiforecasts.io/EpiNow2/dev/reference/plot.estimate_infections.md)
[`report_plots()`](https://epiforecasts.io/EpiNow2/dev/reference/report_plots.md)
