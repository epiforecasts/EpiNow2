# Validate probability distribution for using as generation time

**\[stable\]** does all the checks
in[`check_stan_delay()`](https://epiforecasts.io/EpiNow2/reference/check_stan_delay.md)
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
