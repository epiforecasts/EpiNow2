# Get the distribution of a `<dist_spec>`

**\[experimental\]**

## Usage

``` r
get_distribution(x, id = NULL)
```

## Arguments

- x:

  A `<dist_spec>`.

- id:

  Integer; the id of the distribution to use (if x is a composite
  distribution). If `x` is a single distribution this is ignored and can
  be left at its default value of `NULL`.

## Value

A character string naming the distribution (or "nonparametric")

## Examples

``` r
dist <- Gamma(shape = 3, rate = 2, max = 10)
get_distribution(dist)
#> [1] "gamma"
```
