# Returns the maximum of one or more delay distribution

**\[experimental\]** This works out the maximum of all the (parametric /
nonparametric) delay distributions combined in the passed \<dist_spec\>
(ignoring any uncertainty in parameters)

## Usage

``` r
# S3 method for class 'dist_spec'
max(x, ...)
```

## Arguments

- x:

  The \<dist_spec\> to use

- ...:

  Not used

## Value

A vector of means.

## Examples

``` r
# A fixed gamma distribution with mean 5 and sd 1.
dist1 <- Gamma(mean = 5, sd = 1, max = 20)
max(dist1)
#> [1] 20

# An uncertain lognormal distribution with meanlog and sdlog normally
# distributed as Normal(3, 0.5) and Normal(2, 0.5) respectively
dist2 <- LogNormal(
  meanlog = Normal(3, 0.5),
  sdlog = Normal(2, 0.5),
  max = 20
)
max(dist2)
#> [1] 20

# The max the sum of two distributions
max(dist1 + dist2)
#> [1] 20 20
```
