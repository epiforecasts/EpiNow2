# Update estimate_secondary default priors

**\[stable\]** This functions allows the user to more easily specify
data driven or model based priors for
[`estimate_secondary()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_secondary.md)
from example from previous model fits using a `<data.frame>` to
overwrite other default settings. Note that default settings are still
required.

## Usage

``` r
update_secondary_args(data, priors, verbose = TRUE)
```

## Arguments

- data:

  A list of data and arguments as returned by
  [`create_stan_data()`](https://epiforecasts.io/EpiNow2/dev/reference/create_stan_data.md).

- priors:

  A `<data.frame>` of named priors to be used in model fitting rather
  than the defaults supplied from other arguments. This is typically
  useful if wanting to inform a estimate from the posterior of another
  model fit. Priors that are currently use to update the defaults are
  the scaling fraction ("fraction_observed"), and delay parameters
  ("delay_params"). The `<data.frame>` should have the following
  variables: `variable`, `mean`, and `sd`.

- verbose:

  Logical, defaults to `FALSE`. Should verbose progress messages be
  returned.

## Value

A list as produced by
[`create_stan_data()`](https://epiforecasts.io/EpiNow2/dev/reference/create_stan_data.md).

## Examples

``` r
priors <- data.frame(variable = "fraction_observed", mean = 3, sd = 1)
data <- list(obs_scale_mean = 4, obs_scale_sd = 3)
update_secondary_args(data, priors)
#> Replacing specified priors with those passed through the `prior` dataframe.
#> $obs_scale_mean
#> [1] 3
#> 
#> $obs_scale_sd
#> [1] 1
#> 
```
