# Reconstruct a nonparametric delay distribution

Reconstruct a nonparametric delay from Stan data.

## Usage

``` r
reconstruct_nonparametric(stan_data, np_id, np_posterior = NULL)
```

## Arguments

- stan_data:

  List of Stan data containing delay specification

- np_id:

  Integer index into the nonparametric delay PMF arrays

- np_posterior:

  Matrix of posterior draws for `delay_np_est_raw` (draws x parameters),
  or NULL

## Value

A `dist_spec` object representing the nonparametric delay

## Details

For estimated delays, returns `NonParametric(pmf = Dirichlet(...))`,
using either the prior alpha (no fit available) or a moment-matched
Dirichlet whose mean equals the posterior mean of the simplex and whose
concentration matches the average per-bin posterior variance. For fixed
delays, returns the `NonParametric` PMF as supplied.
