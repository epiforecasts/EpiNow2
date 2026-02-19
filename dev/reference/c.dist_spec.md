# Combines multiple delay distributions for further processing

**\[experimental\]** This combines the parameters so that they can be
fed as multiple delay distributions to
[`epinow()`](https://epiforecasts.io/EpiNow2/dev/reference/epinow.md) or
[`estimate_infections()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_infections.md).

Note that distributions that already are combinations of other
distributions cannot be combined with other combinations of
distributions.

## Usage

``` r
# S3 method for class 'dist_spec'
c(...)
```

## Arguments

- ...:

  The delay distributions to combine

## Value

Combined delay distributions (with class `<dist_spec>`)

## Examples

``` r
# A fixed lognormal distribution with mean 5 and sd 1.
dist1 <- LogNormal(
  meanlog = 1.6, sdlog = 1, max = 20
)
dist1 + dist1
#> Composite distribution:
#> - lognormal distribution (max: 20):
#>   meanlog:
#>     1.6
#>   sdlog:
#>     1
#> - lognormal distribution (max: 20):
#>   meanlog:
#>     1.6
#>   sdlog:
#>     1

# An uncertain gamma distribution with shape and rate normally distributed
# as Normal(3, 0.5) and Normal(2, 0.5) respectively
dist2 <- Gamma(
  shape = Normal(3, 0.5),
  rate = Normal(2, 0.5),
  max = 20
)
c(dist1, dist2)
#> Composite distribution:
#> - lognormal distribution (max: 20):
#>   meanlog:
#>     1.6
#>   sdlog:
#>     1
#> - gamma distribution (max: 20):
#>   shape:
#>     - normal distribution:
#>       mean:
#>         3
#>       sd:
#>         0.5
#>   rate:
#>     - normal distribution:
#>       mean:
#>         2
#>       sd:
#>         0.5
```
