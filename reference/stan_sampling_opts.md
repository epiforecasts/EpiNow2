# Stan Sampling Options

**\[stable\]** Defines a list specifying the arguments passed to either
[`rstan::sampling()`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html)
or
[`cmdstanr::sample()`](https://mc-stan.org/cmdstanr/reference/model-method-sample.html).
Custom settings can be supplied which override the defaults.

## Usage

``` r
stan_sampling_opts(
  cores = getOption("mc.cores", 1L),
  warmup = 250,
  samples = 2000,
  chains = 4,
  control = list(),
  save_warmup = FALSE,
  seed = as.integer(runif(1, 1, 1e+08)),
  future = FALSE,
  max_execution_time = Inf,
  backend = c("rstan", "cmdstanr"),
  ...
)
```

## Arguments

- cores:

  Number of cores to use when executing the chains in parallel, which
  defaults to 1 but it is recommended to set the mc.cores option to be
  as many processors as the hardware and RAM allow (up to the number of
  chains).

- warmup:

  Numeric, defaults to 250. Number of warmup samples per chain.

- samples:

  Numeric, default 2000. Overall number of posterior samples. When using
  multiple chains iterations per chain is samples / chains.

- chains:

  Numeric, defaults to 4. Number of MCMC chains to use.

- control:

  List, defaults to empty. control parameters to pass to underlying
  `rstan` function. By default `adapt_delta = 0.9` and
  `max_treedepth = 12` though these settings can be overwritten.

- save_warmup:

  Logical, defaults to FALSE. Should warmup progress be saved.

- seed:

  Numeric, defaults uniform random number between 1 and 1e8. Seed of
  sampling process.

- future:

  Logical, defaults to `FALSE`. Should stan chains be run in parallel
  using `future`. This allows users to have chains fail gracefully (i.e
  when combined with `max_execution_time`). Should be combined with a
  call to
  [`future::plan()`](https://future.futureverse.org/reference/plan.html).

- max_execution_time:

  Numeric, defaults to Inf (seconds). If set wil kill off processing of
  each chain if not finished within the specified timeout. When more
  than 2 chains finish successfully estimates will still be returned. If
  less than 2 chains return within the allowed time then estimation will
  fail with an informative error.

- backend:

  Character string indicating the backend to use for fitting stan
  models. Supported arguments are "rstan" (default) or "cmdstanr".

- ...:

  Additional parameters to pass to
  [`rstan::sampling()`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html)
  or
  [`cmdstanr::sample()`](https://mc-stan.org/cmdstanr/reference/model-method-sample.html).

## Value

A list of arguments to pass to
[`rstan::sampling()`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html)
or
[`cmdstanr::sample()`](https://mc-stan.org/cmdstanr/reference/model-method-sample.html).

## Examples

``` r
stan_sampling_opts(samples = 2000)
#> $chains
#> [1] 4
#> 
#> $save_warmup
#> [1] FALSE
#> 
#> $seed
#> [1] 15882020
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
#> [1] 750
#> 
```
