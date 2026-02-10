# Reconstruct a nonparametric delay distribution

Helper function to reconstruct a single nonparametric delay component
from Stan data. Nonparametric delays are stored as probability mass
functions.

## Usage

``` r
reconstruct_nonparametric(stan_data, np_id)
```

## Arguments

- stan_data:

  List of Stan data containing delay specification

- np_id:

  Integer index into the nonparametric delay PMF arrays

## Value

A `dist_spec` object representing the nonparametric delay
