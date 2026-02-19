# Returns the standard deviation of one or more delay distribution

**\[experimental\]** This works out the standard deviation of all the
(parametric / nonparametric) delay distributions combined in the passed
\<dist_spec\>. If any of the parameters are themselves uncertain then
`NA` is returned.

## Usage

``` r
# S3 method for class 'dist_spec'
sd(x, ...)
```

## Arguments

- x:

  The \<dist_spec\> to use

## Value

A vector of standard deviations.

## Examples

``` r
if (FALSE) { # \dontrun{
# A fixed lognormal distribution with sd 5 and sd 1.
dist1 <- LogNormal(mean = 5, sd = 1, max = 20)
sd(dist1)

# A gamma distribution with mean 3 and sd 2
dist2 <- Gamma(mean = 3, sd = 2)
sd(dist2)

# The sd of the sum of two distributions
sd(dist1 + dist2)
} # }
```
