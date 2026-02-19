# Choose a parallel or sequential apply function

Internal function that chooses an appropriate "apply"-type function
(either [`lapply()`](https://rdrr.io/r/base/lapply.html) or
[`future.apply::future_lapply()`](https://future.apply.futureverse.org/reference/future_lapply.html))

## Usage

``` r
lapply_func(..., backend = "rstan", future.opts = list())
```

## Arguments

- ...:

  Additional parameters to pass to underlying option functions,
  [`stan_sampling_opts()`](https://epiforecasts.io/EpiNow2/dev/reference/stan_sampling_opts.md)
  or
  [`stan_vb_opts()`](https://epiforecasts.io/EpiNow2/dev/reference/stan_vb_opts.md),
  depending on the method

- backend:

  Character string indicating the backend to use for fitting stan
  models. Supported arguments are "rstan" (default) or "cmdstanr".

## Value

A function that can be used to apply a function to a list
