# Create Initial Conditions Generating Function

**\[stable\]** Uses the output of
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
