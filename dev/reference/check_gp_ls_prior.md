# Check the approximate GP settings can be chosen from a lengthscale prior

The settings can be chosen from priors with fixed parameters and an
analytic cumulative distribution function.

## Usage

``` r
check_gp_ls_prior(ls)
```

## Arguments

- ls:

  A `<dist_spec>` giving the lengthscale prior.

## Value

Invisibly, `ls`. Called for its side effect of erroring for unsupported
priors.
