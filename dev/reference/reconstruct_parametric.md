# Reconstruct a parametric delay distribution

Helper function to reconstruct a single parametric delay component from
Stan data and posterior samples.

## Usage

``` r
reconstruct_parametric(stan_data, param_id, posterior)
```

## Arguments

- stan_data:

  List of Stan data containing delay specification

- param_id:

  Integer index into the parametric delay arrays

- posterior:

  Data frame with posterior mean and sd for delay_params, or NULL if not
  estimated

## Value

A `dist_spec` object representing the delay distribution
