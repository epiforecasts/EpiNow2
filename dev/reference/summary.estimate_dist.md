# Summarise results from estimate_dist

**\[experimental\]** Returns parameter summary statistics for the fitted
delay distribution model.

## Usage

``` r
# S3 method for class 'estimate_dist'
summary(object, CrIs = c(0.2, 0.5, 0.9), ...)
```

## Arguments

- object:

  A fitted model object from
  [`estimate_dist()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_dist.md)

- CrIs:

  Numeric vector of credible intervals to calculate.

- ...:

  Additional arguments (currently unused)

## Value

A `<data.table>` with summary statistics for the delay distribution
parameters.
