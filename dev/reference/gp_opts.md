# Approximate Gaussian Process Settings

Defines a list specifying the structure of the approximate Gaussian
process. Custom settings can be supplied which override the defaults.

## Usage

``` r
gp_opts(
  basis_prop = NULL,
  boundary_scale = NULL,
  ls = LogNormal(mean = 21, sd = 7, max = 60),
  alpha = Normal(mean = 0, sd = 0.01),
  kernel = c("matern", "se", "ou", "periodic"),
  matern_order = 3/2,
  w0 = 1
)
```

## Arguments

- basis_prop:

  Numeric, the proportion of time points to use as basis functions.
  Defaults to `NULL`, in which case the number of basis functions is
  chosen from the lengthscale prior (see Details). Decreasing this value
  results in a decrease in accuracy but a faster compute time (with
  increasing it having the opposite effect).

- boundary_scale:

  Numeric, the boundary factor of the approximate Gaussian process
  relative to the half-range of the time points. Defaults to `NULL`, in
  which case it is chosen from the lengthscale prior (see Details). Not
  used by the periodic kernel.

- ls:

  A `<dist_spec>` giving the prior distribution of the lengthscale
  parameter of the Gaussian process kernel on the scale of days.
  Defaults to a Lognormal distribution with mean 21 days, sd 7 days and
  maximum 60 days: `LogNormal(mean = 21, sd = 7, max = 60)` (a lower
  limit of 0 will be enforced automatically to ensure positivity)

- alpha:

  A `<dist_spec>` giving the prior distribution of the magnitude
  parameter of the Gaussian process kernel. Should be approximately the
  expected standard deviation of the Gaussian process (logged Rt in case
  of the renewal model, logged infections in case of the nonmechanistic
  model). Defaults to a half-normal distribution with mean 0 and sd
  0.01: `Normal(mean = 0, sd = 0.01)` (a lower limit of 0 will be
  enforced automatically to ensure positivity)

- kernel:

  Character string, the type of kernel required. Currently supporting
  the Matern kernel ("matern"), squared exponential kernel ("se"),
  Ornstein-Uhlenbeck kernel ("ou"), and the periodic kernel
  ("periodic").

- matern_order:

  Numeric, defaults to 3/2. Order of Matérn Kernel to use. Common
  choices are 1/2, 3/2, and 5/2. If `kernel` is set to "ou",
  `matern_order` will be automatically set to 1/2. Only used if the
  kernel is set to "matern".

- w0:

  Numeric, defaults to 1.0. Fundamental frequency for periodic kernel.
  They are only used if `kernel` is set to "periodic".

## Value

A `<gp_opts>` object of settings defining the Gaussian process

## Details

The Gaussian process is approximated using the Hilbert space method of
Riutort-Mayol et al. (2023)
[doi:10.1007/s11222-022-10167-2](https://doi.org/10.1007/s11222-022-10167-2)
. Its accuracy depends on the boundary factor and number of basis
functions relative to the lengthscale. Longer lengthscales need a wider
boundary, and shorter lengthscales need more basis functions. By default
both are chosen from the 5% and 95% quantiles of the lengthscale prior
using the relationships in Section 4.3.1 of that paper, so that
lengthscales in this range are approximated accurately. The choice of
these quantiles is a convention that covers most of the prior without
relying on its extreme tails. The paper starts from a single initial
guess at the lengthscale rather than a prior range. The
Ornstein-Uhlenbeck kernel (Matern with `matern_order = 1/2`) is not
covered by the paper. Its number of basis functions uses a relationship
derived for it (see
[`gp_approx_constants()`](https://epiforecasts.io/EpiNow2/dev/reference/gp_approx_constants.md)).
Its boundary uses the Matern 3/2 relationship, which was not re-derived.
The periodic kernel has no boundary, and its number of basis functions
is chosen from the 5% quantile using Appendix B. After fitting, a
warning is given if the posterior median lengthscale lies outside the
range that the approximation represents accurately.

## Examples

``` r
# default settings
gp_opts()
#> $basis_prop
#> NULL
#> 
#> $boundary_scale
#> NULL
#> 
#> $ls
#> - lognormal distribution (max: 60):
#>   meanlog:
#>     3
#>   sdlog:
#>     0.32
#> 
#> $alpha
#> - normal distribution:
#>   mean:
#>     0
#>   sd:
#>     0.01
#> 
#> $kernel
#> [1] "matern"
#> 
#> $matern_order
#> [1] 1.5
#> 
#> $w0
#> [1] 1
#> 
#> attr(,"class")
#> [1] "gp_opts" "list"   

# add a custom length scale
gp_opts(ls = LogNormal(mean = 4, sd = 1, max = 20))
#> $basis_prop
#> NULL
#> 
#> $boundary_scale
#> NULL
#> 
#> $ls
#> - lognormal distribution (max: 20):
#>   meanlog:
#>     1.4
#>   sdlog:
#>     0.25
#> 
#> $alpha
#> - normal distribution:
#>   mean:
#>     0
#>   sd:
#>     0.01
#> 
#> $kernel
#> [1] "matern"
#> 
#> $matern_order
#> [1] 1.5
#> 
#> $w0
#> [1] 1
#> 
#> attr(,"class")
#> [1] "gp_opts" "list"   

# use periodic kernel
gp_opts(kernel = "periodic")
#> $basis_prop
#> NULL
#> 
#> $boundary_scale
#> NULL
#> 
#> $ls
#> - lognormal distribution (max: 60):
#>   meanlog:
#>     3
#>   sdlog:
#>     0.32
#> 
#> $alpha
#> - normal distribution:
#>   mean:
#>     0
#>   sd:
#>     0.01
#> 
#> $kernel
#> [1] "periodic"
#> 
#> $matern_order
#> [1] 1.5
#> 
#> $w0
#> [1] 1
#> 
#> attr(,"class")
#> [1] "gp_opts" "list"   

# set the approximation manually
gp_opts(basis_prop = 0.2, boundary_scale = 1.5)
#> $basis_prop
#> [1] 0.2
#> 
#> $boundary_scale
#> [1] 1.5
#> 
#> $ls
#> - lognormal distribution (max: 60):
#>   meanlog:
#>     3
#>   sdlog:
#>     0.32
#> 
#> $alpha
#> - normal distribution:
#>   mean:
#>     0
#>   sd:
#>     0.01
#> 
#> $kernel
#> [1] "matern"
#> 
#> $matern_order
#> [1] 1.5
#> 
#> $w0
#> [1] 1
#> 
#> attr(,"class")
#> [1] "gp_opts" "list"   
```
