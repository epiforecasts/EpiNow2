# Extract scalar parameters from a fitted model

Internal helper to extract scalar parameters (e.g., `fraction_observed`)
from the params array based on `param_id_*` variables.

## Usage

``` r
extract_scalar_params(x, stan_data)
```

## Arguments

- x:

  A fitted model object with `$fit` and `$args` components.

- stan_data:

  The stan data list from `x$args`.

## Value

A named list of `dist_spec` objects representing the posterior
distributions of scalar parameters.
