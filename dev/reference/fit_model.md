# Fit a model using the chosen backend.

Internal function for dispatch to fitting with NUTS or VB.

## Usage

``` r
fit_model(args, id = "stan")
```

## Arguments

- args:

  List of stan arguments.

- id:

  A character string used to assign logging information on error. Used
  by
  [`regional_epinow()`](https://epiforecasts.io/EpiNow2/dev/reference/regional_epinow.md)
  to assign errors to regions. Alter the default to run with error
  catching.
