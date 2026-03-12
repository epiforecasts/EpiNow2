# Plot method for estimate_truncation

**\[experimental\]**
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) method for
class `<estimate_truncation>`. Returns a plot faceted over each dataset
used in fitting with the latest observations as columns, the data
observed at the time (and so truncated) as dots and the truncation
adjusted estimates as a ribbon.

## Usage

``` r
# S3 method for class 'estimate_truncation'
plot(x, ...)
```

## Arguments

- x:

  A list of output as produced by
  [`estimate_truncation()`](https://epiforecasts.io/EpiNow2/reference/estimate_truncation.md)

- ...:

  Pass additional arguments to plot function. Not currently in use.

## Value

`ggplot2` object

## See also

[`estimate_truncation()`](https://epiforecasts.io/EpiNow2/reference/estimate_truncation.md)
