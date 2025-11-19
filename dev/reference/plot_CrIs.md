# Plot EpiNow2 Credible Intervals

**\[stable\]** Adds lineranges for user specified credible intervals

## Usage

``` r
plot_CrIs(plot, CrIs, alpha, linewidth)
```

## Arguments

- plot:

  A `{ggplot2}` plot

- CrIs:

  Numeric list of credible intervals present in the data. As produced by
  [`extract_CrIs()`](https://epiforecasts.io/EpiNow2/dev/reference/extract_CrIs.md).

- alpha:

  Numeric, overall alpha of the target line range

- linewidth:

  Numeric, line width of the default line range.

## Value

A `{ggplot2}` plot.
