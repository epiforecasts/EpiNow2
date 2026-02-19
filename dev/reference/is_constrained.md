# Check if a \<dist_spec\> is constrained, i.e. has a finite maximum or nonzero CDF cutoff.

**\[experimental\]**

## Usage

``` r
# S3 method for class 'dist_spec'
is_constrained(x, ...)
```

## Arguments

- x:

  A `<dist_spec>`

- ...:

  ignored

## Value

Logical; TRUE if `x` is constrained

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

# both distributions are constrained and therefore so is the sum
is_constrained(dist1 + dist2)
#> [1] TRUE
```
