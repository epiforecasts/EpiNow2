# Allocate Empty Parameters to a List

**\[stable\]** Allocate missing parameters to be empty two dimensional
arrays. Used internally by
[`forecast_infections()`](https://epiforecasts.io/EpiNow2/dev/reference/forecast_infections.md).

## Usage

``` r
allocate_empty(data, params, n = 0)
```

## Arguments

- data:

  A list of parameters

- params:

  A character vector of parameters to allocate to empty if missing.

- n:

  Numeric, number of samples to assign an empty array

## Value

A list of parameters some allocated to be empty
