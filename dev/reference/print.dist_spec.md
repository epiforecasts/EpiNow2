# Prints the parameters of one or more delay distributions

**\[experimental\]** This displays the parameters of the uncertain and
probability mass functions of fixed delay distributions combined in the
passed \<dist_spec\>.

## Usage

``` r
# S3 method for class 'dist_spec'
print(x, ...)
```

## Arguments

- x:

  The `<dist_spec>` to use

- ...:

  Not used

## Value

invisible

## Examples

``` r
#' # A fixed lognormal distribution with mean 5 and sd 1.
dist1 <- LogNormal(mean = 1.5, sd = 0.5, max = 20)
print(dist1)
#> - lognormal distribution (max: 19):
#>   meanlog:
#>     0.35
#>   sdlog:
#>     0.32

# An uncertain gamma distribution with shape and rate normally distributed
# as Normal(3, 0.5) and Normal(2, 0.5) respectively
dist2 <- Gamma(
  shape = Normal(3, 0.5), rate = Normal(2, 0.5), max = 20
)
print(dist2)
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
