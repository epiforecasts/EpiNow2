# Distribution Skeleton

**\[deprecated\]** This function acts as a skeleton for a truncated
distribution defined by model type, maximum value and model parameters.

## Usage

``` r
dist_skel(
  n,
  dist = FALSE,
  cum = TRUE,
  model,
  discrete = FALSE,
  params,
  max_value = 120
)
```

## Arguments

- n:

  Numeric vector, number of samples to take (or days for the probability
  density).

- dist:

  Logical, defaults to `FALSE`. Should the probability density be
  returned rather than a number of samples.

- cum:

  Logical, defaults to `TRUE`. If `dist = TRUE` should the returned
  distribution be cumulative.

- model:

  Character string, defining the model to be used. Supported options are
  exponential ("exp"), gamma ("gamma"), and log normal ("lognormal")

- discrete:

  Logical, defaults to `FALSE`. Should the probability distribution be
  discretised. In this case each entry of the probability mass function
  corresponds to the 2-length interval ending at the entry except for
  the first interval that covers (0, 1). That is, the probability mass
  function is a vector where the first entry corresponds to the integral
  over the (0,1\] interval of the continuous distribution, the second
  entry corresponds to the (0,2\] interval, the third entry corresponds
  to the (1, 3\] interval etc.

- params:

  A list of parameters values (by name) required for each model. For the
  exponential model this is a rate parameter and for the gamma model
  this is alpha and beta.

- max_value:

  Numeric, the maximum value to allow. Defaults to 120. Samples outside
  of this range are resampled.

## Value

A vector of samples or a probability distribution.
