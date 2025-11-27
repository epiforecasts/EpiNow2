# Collapse nonparametric distributions in a \<dist_spec\>

**\[experimental\]** This convolves any consecutive nonparametric
distributions contained in the \<dist_spec\>.

## Usage

``` r
# S3 method for class 'dist_spec'
collapse(x, ...)
```

## Arguments

- x:

  A `<dist_spec>`

- ...:

  ignored

## Value

A `<dist_spec>` where consecutive nonparametric distributions have been
convolved

## Examples

``` r
# A fixed gamma distribution with mean 5 and sd 1.
dist1 <- Gamma(mean = 5, sd = 1, max = 20)

# An uncertain lognormal distribution with meanlog and sdlog normally
# distributed as Normal(3, 0.5) and Normal(2, 0.5) respectively
dist2 <- LogNormal(
  meanlog = Normal(3, 0.5),
  sdlog = Normal(2, 0.5),
  max = 20
)

# The maxf the sum of two distributions
collapse(discretise(dist1 + dist2, strict = FALSE))
#> Composite distribution:
#> - nonparametric distribution
#>   PMF: [8e-11 2.3e-05 0.0056 0.078 0.26 0.34 0.22 0.076 0.016 0.0022 0.00022 1.7e-05 1.1e-06 5.5e-08 2.4e-09 9.2e-11 3.2e-12 9.7e-14 2.8e-15]
#> - lognormal distribution (max: 20):
#>   meanlog:
#>     - normal distribution:
#>       mean:
#>         3
#>       sd:
#>         0.5
#>   sdlog:
#>     - normal distribution:
#>       mean:
#>         2
#>       sd:
#>         0.5
```
