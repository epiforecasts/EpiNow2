# Check that PMF tail is not sparse

Checks if the tail of a PMF vector has more than `span` consecutive
values smaller than `tol` and throws a warning if so.

## Usage

``` r
check_sparse_pmf_tail(pmf, span = 5, tol = 1e-06)
```

## Arguments

- pmf:

  A probability mass function vector

- span:

  The number of consecutive indices in the tail to check

- tol:

  The value which to consider the tail as sparse

## Value

Called for its side effects.
