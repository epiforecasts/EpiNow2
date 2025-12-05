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
#>  [1] 1.616618e-01 3.809484e-01 3.073539e-01 1.121747e-01 2.959971e-02
#>  [6] 6.615857e-03 1.337717e-03 2.529703e-04 4.560307e-05 7.931045e-06
#> [11] 1.341353e-06
```
