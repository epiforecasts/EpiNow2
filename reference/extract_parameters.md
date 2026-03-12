# Extract samples from all parameters

Extract samples from all parameters

## Usage

``` r
extract_parameters(samples, args)
```

## Arguments

- samples:

  Extracted stan model (using
  [`rstan::extract()`](https://mc-stan.org/rstan/reference/stanfit-method-extract.html))

- args:

  Stan data list containing param_id\_\* and params_variable_lookup for
  parameter naming.

## Value

A `<data.table>` with columns: variable, sample, value, or NULL if
parameters don't exist in the samples
