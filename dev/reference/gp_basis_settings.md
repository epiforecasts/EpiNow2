# Choose the boundary factor and number of basis functions

Settings not given in
[`gp_opts()`](https://epiforecasts.io/EpiNow2/dev/reference/gp_opts.md)
are chosen from the lengthscale prior using
[`gp_approx_constants()`](https://epiforecasts.io/EpiNow2/dev/reference/gp_approx_constants.md).
The boundary factor is set by the upper (95%) prior quantile, as longer
lengthscales need a wider boundary. The number of basis functions is
then set by the lower (5%) prior quantile, as shorter lengthscales need
more basis functions. The periodic kernel has no boundary, so only the
number of basis functions is chosen.

## Usage

``` r
gp_basis_settings(gp, n, gp_type)
```

## Arguments

- gp:

  A `<gp_opts>` object.

- n:

  Length of the Gaussian process.

- gp_type:

  Integer kernel type as used in the Stan model (0: squared exponential,
  1: periodic, 2: Matern).

## Value

A list with the boundary factor `L` and number of basis functions `M`.
