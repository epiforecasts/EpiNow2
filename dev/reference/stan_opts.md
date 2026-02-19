# Stan Options

**\[stable\]** Defines a list specifying the arguments passed to
underlying stan backend functions via
[`stan_sampling_opts()`](https://epiforecasts.io/EpiNow2/dev/reference/stan_sampling_opts.md)
and
[`stan_vb_opts()`](https://epiforecasts.io/EpiNow2/dev/reference/stan_vb_opts.md).
Custom settings can be supplied which override the defaults.

## Usage

``` r
stan_opts(
  object = NULL,
  samples = 2000,
  method = c("sampling", "vb", "laplace", "pathfinder"),
  backend = c("rstan", "cmdstanr"),
  return_fit = TRUE,
  ...
)
```

## Arguments

- object:

  Stan model object. By default uses the compiled package default if
  using the "rstan" backend, and the default model obtained using
  [`epinow2_cmdstan_model()`](https://epiforecasts.io/EpiNow2/dev/reference/epinow2_cmdstan_model.md)
  if using the "cmdstanr" backend.

- samples:

  Numeric, defaults to 2000. Number of posterior samples.

- method:

  A character string, defaulting to sampling. Currently supports MCMC
  sampling ("sampling") or approximate posterior sampling via
  variational inference ("vb") and, as experimental features if the
  "cmdstanr" backend is used, approximate posterior sampling with the
  laplace algorithm ("laplace") or pathfinder ("pathfinder").

- backend:

  Character string indicating the backend to use for fitting stan
  models. Supported arguments are "rstan" (default) or "cmdstanr".

- return_fit:

  Logical, defaults to TRUE. Should the fit stan model be returned.

- ...:

  Additional parameters to pass to underlying option functions,
  [`stan_sampling_opts()`](https://epiforecasts.io/EpiNow2/dev/reference/stan_sampling_opts.md)
  or
  [`stan_vb_opts()`](https://epiforecasts.io/EpiNow2/dev/reference/stan_vb_opts.md),
  depending on the method

## Value

A `<stan_opts>` object of arguments to pass to the appropriate rstan
functions.

## See also

[`stan_sampling_opts()`](https://epiforecasts.io/EpiNow2/dev/reference/stan_sampling_opts.md)
[`stan_vb_opts()`](https://epiforecasts.io/EpiNow2/dev/reference/stan_vb_opts.md)

## Examples

``` r
# using default of [rstan::sampling()]
stan_opts(samples = 1000)
#> $backend
#> [1] "rstan"
#> 
#> $object
#> NULL
#> 
#> $method
#> [1] "sampling"
#> 
#> $chains
#> [1] 4
#> 
#> $save_warmup
#> [1] FALSE
#> 
#> $seed
#> [1] 33039985
#> 
#> $future
#> [1] FALSE
#> 
#> $max_execution_time
#> [1] Inf
#> 
#> $cores
#> [1] 1
#> 
#> $warmup
#> [1] 250
#> 
#> $control
#> $control$adapt_delta
#> [1] 0.9
#> 
#> $control$max_treedepth
#> [1] 12
#> 
#> 
#> $iter
#> [1] 500
#> 
#> $return_fit
#> [1] TRUE
#> 
#> attr(,"class")
#> [1] "stan_opts" "list"     

# using vb
stan_opts(method = "vb")
#> $backend
#> [1] "rstan"
#> 
#> $object
#> NULL
#> 
#> $method
#> [1] "vb"
#> 
#> $trials
#> [1] 10
#> 
#> $iter
#> [1] 10000
#> 
#> $output_samples
#> [1] 2000
#> 
#> $return_fit
#> [1] TRUE
#> 
#> attr(,"class")
#> [1] "stan_opts" "list"     
```
