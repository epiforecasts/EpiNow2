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
#>   PMF: [7.5e-12 5.5e-06 0.0026 0.061 0.27 0.37 0.22 0.067 0.013 0.0016 0.00015 1.1e-05 6.6e-07 3.2e-08 1.4e-09 5.2e-11 1.7e-12 5e-14 8.9e-16 7.8e-16]
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
