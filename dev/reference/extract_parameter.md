# Extract Samples from a Parameter with a Single Dimension

Extract Samples from a Parameter with a Single Dimension

## Usage

``` r
extract_parameter(param, samples)
```

## Arguments

- param:

  Character string indicating the parameter to extract

- samples:

  Extracted stan model (using
  [`rstan::extract()`](https://rdrr.io/pkg/rstan/man/stanfit-method-extract.html))

## Value

A `<data.frame>` containing the parameter name, sample id and sample
value, or NULL if the parameter doesn't exist in the samples
