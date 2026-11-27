# Check the posterior lengthscale against the approximate GP settings

Implements the lengthscale diagnostic of Riutort-Mayol et al. (2023,
Section 4.5)
[doi:10.1007/s11222-022-10167-2](https://doi.org/10.1007/s11222-022-10167-2)
. The approximation is assumed accurate when the posterior median
lengthscale lies within the range returned by
[`gp_ls_range()`](https://epiforecasts.io/EpiNow2/dev/reference/gp_ls_range.md),
allowing a tolerance of 0.01 on the rescaled scale at the lower end.

## Usage

``` r
check_gp_lengthscale(rho, stan_data)
```

## Arguments

- rho:

  Numeric vector of posterior lengthscale samples (in days).

- stan_data:

  A list of Stan data as returned by
  [`create_gp_data()`](https://epiforecasts.io/EpiNow2/dev/reference/create_gp_data.md).

## Value

Invisibly, the range from
[`gp_ls_range()`](https://epiforecasts.io/EpiNow2/dev/reference/gp_ls_range.md).
Called for its side effect of warning when the lengthscale is outside
this range.
