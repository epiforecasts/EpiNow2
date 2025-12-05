# Extract samples from all parameters

Extract samples from all parameters

## Usage

``` r
extract_parameters(samples)
```

## Arguments

- samples:

  Extracted stan model (using
  [`rstan::extract()`](https://rdrr.io/pkg/rstan/man/stanfit-method-extract.html))

## Value

A `<data.table>` containing the parameter name, sample id and sample
value, or NULL if parameters don't exist in the samples
