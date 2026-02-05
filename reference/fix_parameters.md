# Fix the parameters of a `<dist_spec>`

**\[experimental\]** If the given `<dist_spec>` has any uncertainty, it
is removed and the corresponding distribution converted into a fixed
one.

## Usage

``` r
# S3 method for class 'dist_spec'
fix_parameters(x, strategy = c("mean", "sample"), ...)
```

## Arguments

- x:

  A `<dist_spec>`

- strategy:

  Character; either "mean" (use the mean estimates of the mean and
  standard deviation) or "sample" (randomly sample mean and standard
  deviation from uncertainty given in the `<dist_spec>`

- ...:

  ignored

## Value

A `<dist_spec>` object without uncertainty

## Examples

``` r
# An uncertain gamma distribution with shape and rate normally distributed
# as Normal(3, 0.5) and Normal(2, 0.5) respectively
dist <- Gamma(
  shape = Normal(3, 0.5),
  rate = Normal(2, 0.5),
  max = 20
)

fix_parameters(dist)
#> - gamma distribution (max: 20):
#>   shape:
#>     3
#>   rate:
#>     2
```
