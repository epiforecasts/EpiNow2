# Return a stan model object for the appropriate backend

Return a stan model object for the appropriate backend

## Usage

``` r
epinow2_stan_model(
  backend = c("rstan", "cmdstanr"),
  model = c("estimate_infections", "simulate_infections", "estimate_secondary",
    "simulate_secondary", "estimate_truncation", "dist_fit")
)
```

## Arguments

- backend:

  Character string indicating the backend to use for fitting stan
  models. Supported arguments are "rstan" (default) or "cmdstanr".

- model:

  A character string indicating the model to use. One of
  "estimate_infections" (default), "simulate_infections",
  "estimate_secondary", "simulate_secondary", "estimate_truncation" or
  "dist_fit".

## Value

A stan model object (either `rstan::stanmodel` or
[`cmdstanr::CmdStanModel`](https://mc-stan.org/cmdstanr/reference/CmdStanModel.html),
depending on the backend)
