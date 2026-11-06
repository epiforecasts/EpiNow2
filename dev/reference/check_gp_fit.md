# Check the approximate GP of a fitted model

Extracts the posterior lengthscale from a fitted model and passes it to
[`check_gp_lengthscale()`](https://epiforecasts.io/EpiNow2/dev/reference/check_gp_lengthscale.md).

## Usage

``` r
check_gp_fit(fit, stan_data)
```

## Arguments

- fit:

  A fitted Stan model.

- stan_data:

  A list of Stan data as returned by
  [`create_gp_data()`](https://epiforecasts.io/EpiNow2/dev/reference/create_gp_data.md).

## Value

Invisibly, the result of
[`check_gp_lengthscale()`](https://epiforecasts.io/EpiNow2/dev/reference/check_gp_lengthscale.md).
