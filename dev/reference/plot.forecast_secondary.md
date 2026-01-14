# Plot method for forecast_secondary objects

**\[stable\]** Plot method for forecast secondary observations.

## Usage

``` r
# S3 method for class 'forecast_secondary'
plot(x, primary = FALSE, from = NULL, to = NULL, new_obs = NULL, ...)
```

## Arguments

- x:

  A list of output as produced by `estimate_secondary`

- primary:

  Logical, defaults to `FALSE`. Should `primary` reports also be plot?

- from:

  Date object indicating when to plot from.

- to:

  Date object indicating when to plot up to.

- new_obs:

  A `<data.frame>` containing the columns `date` and `secondary` which
  replace the secondary observations stored in the `estimate_secondary`
  output.

- ...:

  Pass additional arguments to plot function. Not currently in use.
