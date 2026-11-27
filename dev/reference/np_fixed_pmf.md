# Fixed PMF placeholder for a nonparametric delay

Fixed PMF placeholder for a nonparametric delay

## Usage

``` r
np_fixed_pmf(x)
```

## Arguments

- x:

  A nonparametric `dist_spec`.

## Value

The delay's PMF as a numeric vector.

## Details

An estimated (Dirichlet-backed) delay has no fixed PMF, so its prior
mean is used as a placeholder of the right length; Stan overwrites those
entries with the estimated simplex.
