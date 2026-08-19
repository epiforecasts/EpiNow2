# Extract delay distributions from a fitted model

Internal helper to extract delay distributions from the `delay_id_*`
variables in stan data.

## Usage

``` r
extract_delay_params(x, stan_data)
```

## Arguments

- x:

  A fitted model object with `$fit` and `$args` components.

- stan_data:

  The stan data list from `x$args`.

## Value

A named list of `dist_spec` objects representing the posterior
distributions of delay parameters.
