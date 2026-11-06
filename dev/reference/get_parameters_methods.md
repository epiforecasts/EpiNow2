# Extract parameters from EpiNow2 model fits

S3 methods for
[`distspec::get_parameters()`](https://epiforecasts.io/distspec/reference/get_parameters.html)
that extract the estimated delay distribution and scalar parameters from
fitted EpiNow2 model objects.

## Usage

``` r
get_parameters.epinowfit(x, ...)

get_parameters.estimate_dist(x, ...)
```

## Arguments

- x:

  A fitted EpiNow2 model object.

- ...:

  Not used.

## Value

A named list of parameters.
