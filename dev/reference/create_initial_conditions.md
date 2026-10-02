# Create Initial Conditions Generating Function

Uses the output of
[`create_stan_data()`](https://epiforecasts.io/EpiNow2/dev/reference/create_stan_data.md)
to create a function which can be used to sample from the prior
distributions (or as close as possible) for parameters. Used in order to
initialise each stan chain within a range of plausible values.

## Usage

``` r
create_initial_conditions(stan_data, params)
```

## Arguments

- stan_data:

  A list of data as produced by
  [`create_stan_data()`](https://epiforecasts.io/EpiNow2/dev/reference/create_stan_data.md).

- params:

  A list of `<EpiNow2.params>` as created by
  [`make_param()`](https://epiforecasts.io/EpiNow2/dev/reference/make_param.md)

## Value

An initial condition generating function

## Details

`R_mean` is seeded from the initial-Rt prior carried in `stan_data` by
[`make_init_priors()`](https://epiforecasts.io/EpiNow2/dev/reference/make_init_priors.md),
so chains start near the configured reproduction number; this is a
stopgap until derived-prior parameters are initialised through the
shared path (#1481). The distribution code follows
[`pack_init_prior()`](https://epiforecasts.io/EpiNow2/dev/reference/pack_init_prior.md)
(0: lognormal, 1: gamma, 2: normal).
