# Discretised probability mass function

**\[questioning\]** This function returns the probability mass function
of a discretised and truncated distribution defined by distribution
type, maximum value and model parameters.

## Usage

``` r
discrete_pmf(
  distribution = c("exp", "gamma", "lognormal", "normal", "fixed"),
  params,
  max_value,
  cdf_cutoff,
  width
)
```

## Arguments

- distribution:

  A character string representing the distribution to be used (one of
  "exp", "gamma", "lognormal", "normal" or "fixed")

- params:

  A list of parameters values (by name) required for each model. For the
  exponential model this is a rate parameter and for the gamma model
  this is alpha and beta.

- max_value:

  Numeric, the maximum value to allow. Samples outside of this range are
  resampled.

- cdf_cutoff:

  Numeric; the desired CDF cutoff. Any part of the cumulative
  distribution function beyond 1 minus the value of this argument is
  removed. Default: `0`, i.e. use the full distribution.

- width:

  Numeric, the width of each discrete bin.

## Value

A vector representing a probability distribution.

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
