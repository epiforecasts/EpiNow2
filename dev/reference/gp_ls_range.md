# Range of lengthscales the approximate GP represents accurately

Range of lengthscales the approximate GP represents accurately

## Usage

``` r
gp_ls_range(stan_data)
```

## Arguments

- stan_data:

  A list of Stan data as returned by
  [`create_gp_data()`](https://epiforecasts.io/EpiNow2/dev/reference/create_gp_data.md).

## Value

A numeric vector giving the shortest and longest lengthscale (in days)
that the boundary factor and number of basis functions can approximate
accurately. The longest is `Inf` for the periodic kernel, which has no
boundary.
