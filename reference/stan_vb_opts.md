# Stan Variational Bayes Options

**\[stable\]** Defines a list specifying the arguments passed to
[`rstan::vb()`](https://mc-stan.org/rstan/reference/stanmodel-method-vb.html)
or
[`cmdstanr::variational()`](https://mc-stan.org/cmdstanr/reference/model-method-variational.html).
Custom settings can be supplied which override the defaults.

## Usage

``` r
stan_vb_opts(samples = 2000, trials = 10, iter = 10000, ...)
```

## Arguments

- samples:

  Numeric, default 2000. Overall number of approximate posterior
  samples.

- trials:

  Numeric, defaults to 10. Number of attempts to use rstan::vb()\]
  before failing.

- iter:

  Numeric, defaulting to 10000. Number of iterations to use in
  [`rstan::vb()`](https://mc-stan.org/rstan/reference/stanmodel-method-vb.html).

- ...:

  Additional parameters to pass to
  [`rstan::vb()`](https://mc-stan.org/rstan/reference/stanmodel-method-vb.html)
  or
  [`cmdstanr::variational()`](https://mc-stan.org/cmdstanr/reference/model-method-variational.html),
  depending on the chosen backend.

## Value

A list of arguments to pass to
[`rstan::vb()`](https://mc-stan.org/rstan/reference/stanmodel-method-vb.html)
or
[`cmdstanr::variational()`](https://mc-stan.org/cmdstanr/reference/model-method-variational.html),
depending on the chosen backend.

## Examples

``` r
stan_vb_opts(samples = 1000)
#> $trials
#> [1] 10
#> 
#> $iter
#> [1] 10000
#> 
#> $output_samples
#> [1] 1000
#> 
```
