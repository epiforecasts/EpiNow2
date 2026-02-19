# Extract a single element of a composite `<dist_spec>`

**\[experimental\]**

## Usage

``` r
extract_single_dist(x, i)
```

## Arguments

- x:

  A composite `dist_spec` object

- i:

  The index to extract

## Value

A single `dist_spec` object

## Examples

``` r
dist1 <- LogNormal(mean = 1.6, sd = 0.5, max = 20)

# An uncertain gamma distribution with shape and rate normally distributed
# as Normal(3, 0.5) and Normal(2, 0.5) respectively
dist2 <- Gamma(
  shape = Normal(3, 0.5),
  rate = Normal(2, 0.5),
  max = 20
)

# Multiple distributions
if (FALSE) { # \dontrun{
dist <- dist1 + dist2
extract_single_dist(dist, 2)
} # }
```
