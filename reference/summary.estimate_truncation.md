# Summarise results from estimate_truncation

**\[stable\]** Returns parameter summary statistics for the fitted
truncation model.

## Usage

``` r
# S3 method for class 'estimate_truncation'
summary(object, CrIs = c(0.2, 0.5, 0.9), ...)
```

## Arguments

- object:

  A fitted model object from
  [`estimate_truncation()`](https://epiforecasts.io/EpiNow2/reference/estimate_truncation.md)

- CrIs:

  Numeric vector of credible intervals to calculate.

- ...:

  Additional arguments (currently unused)

## Value

A `<data.table>` with summary statistics for the truncation distribution
parameters.
