# Quantiles of the lengthscale prior

Quantiles of a lengthscale prior truncated to be positive and below its
maximum.

## Usage

``` r
gp_ls_quantiles(ls, probs = c(0.05, 0.95))
```

## Arguments

- ls:

  A `<dist_spec>` giving the lengthscale prior.

- probs:

  Numeric vector of probabilities.

## Value

A numeric vector of quantiles.
