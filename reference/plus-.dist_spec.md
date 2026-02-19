# Creates a delay distribution as the sum of two other delay distributions.

**\[experimental\]**

## Usage

``` r
# S3 method for class 'dist_spec'
e1 + e2
```

## Arguments

- e1:

  The first delay distribution (of type \<dist_spec\>) to combine.

- e2:

  The second delay distribution (of type \<dist_spec\>) to combine.

## Value

A delay distribution representing the sum of the two delays

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
dist1 + dist2
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
