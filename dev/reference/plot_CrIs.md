# Plot EpiNow2 Credible Intervals

**\[stable\]** Adds credible intervals to a plot, either as ribbons or
error bars.

## Usage

``` r
plot_CrIs(plot, CrIs, alpha, linewidth, style = c("ribbon", "linerange"))
```

## Arguments

- plot:

  A `{ggplot2}` plot

- CrIs:

  Numeric list of credible intervals present in the data. As produced by
  [`extract_CrIs()`](https://epiforecasts.io/EpiNow2/dev/reference/extract_CrIs.md).

- alpha:

  Numeric, overall alpha of the target line range.

- linewidth:

  Numeric, line width of the default line range.

- style:

  Character string indicating the plot style. Options are "ribbon"
  (default) for shaded ribbon plots or "linerange" for error bars.

## Value

A `{ggplot2}` plot.
