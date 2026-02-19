# Get the probability mass function of a nonparametric distribution

**\[experimental\]**

## Usage

``` r
get_pmf(x, id = NULL)
```

## Arguments

- x:

  A `<dist_spec>`.

- id:

  Integer; the id of the distribution to use (if x is a composite
  distribution). If `x` is a single distribution this is ignored and can
  be left at its default value of `NULL`.

## Value

The pmf of the distribution

## Examples

``` r
dist <- discretise(Gamma(shape = 3, rate = 2, max = 10))
get_pmf(dist)
#>  [1] 1.090089e-01 4.559816e-01 3.019114e-01 1.007542e-01 2.544795e-02
#>  [6] 5.544248e-03 1.102455e-03 2.060432e-04 3.682103e-05 6.360829e-06
```
