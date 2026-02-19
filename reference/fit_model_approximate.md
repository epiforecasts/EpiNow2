# Fit a Stan Model using an approximate method

**\[maturing\]** Fits a stan model using variational inference.

## Usage

``` r
fit_model_approximate(args, future = FALSE, id = "stan")
```

## Arguments

- args:

  List of stan arguments.

- future:

  Logical, defaults to `FALSE`. Should `future` be used to run stan
  chains in parallel.

- id:

  A character string used to assign logging information on error. Used
  by
  [`regional_epinow()`](https://epiforecasts.io/EpiNow2/reference/regional_epinow.md)
  to assign errors to regions. Alter the default to run with error
  catching.

## Value

A stan model object
