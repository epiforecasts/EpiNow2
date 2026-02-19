# Discretise a \<dist_spec\>

**\[experimental\]**

## Usage

``` r
# S3 method for class 'dist_spec'
discretise(x, strict = TRUE, remove_trailing_zeros = TRUE, ...)

discretize(x, ...)
```

## Arguments

- x:

  A `<dist_spec>`

- strict:

  Logical; If `TRUE` (default) an error will be thrown if a distribution
  cannot be discretised (e.g., because no finite maximum has been
  specified or parameters are uncertain). If `FALSE` then any
  distribution that cannot be discretised will be returned as is.

- remove_trailing_zeros:

  Logical; If `TRUE` (default), trailing zeroes in the resulting PMF
  will be removed. If `FALSE`, trailing zeroes will be retained.

- ...:

  ignored

## Value

A `<dist_spec>` where all distributions with constant parameters are
nonparametric.

## Methodological details

The probability mass function is computed using the `{primarycensored}`
package, which provides double censored PMF calculations. This correctly
represents the probability mass function of a double censored
distribution arising from the difference of two censored events.

The probability mass function of the discretised probability
distribution is a vector where the first entry corresponds to the
integral over the (0,1\] interval of the corresponding continuous
distribution (probability of integer 0), the second entry corresponds to
the (0,2\] interval (probability mass of integer 1), the third entry
corresponds to the (1, 3\] interval (probability mass of integer 2),
etc.

## References

Charniga, K., et al. "Best practices for estimating and reporting
epidemiological delay distributions of infectious diseases using public
health surveillance and healthcare data", *arXiv e-prints*, 2024.
[doi:10.48550/arXiv.2405.08841](https://doi.org/10.48550/arXiv.2405.08841)
Park, S. W., et al., "Estimating epidemiological delay distributions for
infectious diseases", *medRxiv*, 2024.
[doi:10.1101/2024.01.12.24301247](https://doi.org/10.1101/2024.01.12.24301247)
Abbott S., et al., "primarycensored: Primary Event Censored
Distributions", 2025.
[doi:10.5281/zenodo.13632839](https://doi.org/10.5281/zenodo.13632839)

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
discretise(dist1 + dist2, strict = FALSE)
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
