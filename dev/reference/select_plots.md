# Internal helper function to select plots from those created by [`report_plots()`](https://epiforecasts.io/EpiNow2/dev/reference/report_plots.md)

Internal helper function to select plots from those created by
[`report_plots()`](https://epiforecasts.io/EpiNow2/dev/reference/report_plots.md)

## Usage

``` r
select_plots(
  plots,
  type = c("summary", "infections", "reports", "R", "growth_rate", "all")
)
```

## Arguments

- plots:

  A list of plots as produced by
  [`report_plots()`](https://epiforecasts.io/EpiNow2/dev/reference/report_plots.md).

- type:

  A character vector indicating the name of the plot to return. Defaults
  to "summary" with supported options being "infections", "reports",
  "R", "growth_rate", "summary", "all". If "all" is supplied all plots
  are generated.

## Value

Selected plots by type
