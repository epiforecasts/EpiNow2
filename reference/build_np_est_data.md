# Build Stan data for estimated nonparametric delays

Takes the nonparametric delays and the PMF group boundaries and returns
the ragged-array fields that Stan needs to map a normalised Dirichlet
draw back into `delay_np_pmf`.

## Usage

``` r
build_np_est_data(np_delays, np_pmf_groups)
```

## Arguments

- np_delays:

  A list of nonparametric `dist_spec` objects in their original order.
  Each estimated entry must carry an `$alpha` numeric vector aligned
  with its PMF.

- np_pmf_groups:

  Integer vector of 1-indexed PMF group boundaries (output of
  [`create_stan_delays()`](https://epiforecasts.io/EpiNow2/reference/create_stan_delays.md)).

## Value

A named list with `n_np_est`, `np_est_which`, `np_est_alpha`,
`np_est_pos`, `np_est_groups`, and `np_est_length`. Empty arrays are
returned when no delays are estimated.

## Details

Structural zeros (entries where the prior alpha is zero, e.g. the
`t = 0` generation-time bin) are dropped from the estimated parameter
vector so Stan never sees a `Gamma(0, 1)` rate. Their positions are
still held by the fixed `delay_np_pmf` entries and are left untouched at
sampling time.
