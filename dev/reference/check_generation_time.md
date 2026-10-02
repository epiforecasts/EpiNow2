# Validate probability distribution for using as generation time

does all the checks
in[`check_stan_delay()`](https://epiforecasts.io/EpiNow2/dev/reference/check_stan_delay.md)
and additionally makes sure that if `dist` is nonparametric, its first
element is zero.

## Usage

``` r
check_generation_time(dist)
```

## Arguments

- dist:

  A `dist_spec` object.\`

## Value

Called for its side effects.

## Details

An estimated (Dirichlet-backed) nonparametric delay has no fixed PMF, so
any uncertainty is resolved to the prior mean before checking the first
element; that element is zero exactly when the prior puts no mass there.
