# Create a Normal distribution from posterior samples

Helper function to create a Normal distribution from a row of posterior
summary statistics, with consistent rounding.

## Usage

``` r
posterior_to_normal(posterior, idx)
```

## Arguments

- posterior:

  Data frame with `mean` and `sd` columns from Stan output

- idx:

  Integer index into the posterior data frame

## Value

A `Normal` distribution object
