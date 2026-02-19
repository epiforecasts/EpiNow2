# Stan Laplace algorithm Options

**\[experimental\]** Defines a list specifying the arguments passed to
[`cmdstanr::laplace()`](https://mc-stan.org/cmdstanr/reference/model-method-laplace.html).

## Usage

``` r
stan_laplace_opts(backend = "cmdstanr", trials = 10, ...)
```

## Arguments

- backend:

  Character string indicating the backend to use for fitting stan
  models. Supported arguments are "rstan" (default) or "cmdstanr".

- trials:

  Numeric, defaults to 10. Number of attempts to use rstan::vb()\]
  before failing.

- ...:

  Additional parameters to pass to
  [`cmdstanr::laplace()`](https://mc-stan.org/cmdstanr/reference/model-method-laplace.html).

## Value

A list of arguments to pass to
[`cmdstanr::laplace()`](https://mc-stan.org/cmdstanr/reference/model-method-laplace.html).

## Examples

``` r
stan_laplace_opts()
#> $trials
#> [1] 10
#> 
```
