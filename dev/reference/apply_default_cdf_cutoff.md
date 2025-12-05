# Apply default CDF cutoff to a \<dist_spec\> if it is unconstrained

Apply default CDF cutoff to a \<dist_spec\> if it is unconstrained

## Usage

``` r
apply_default_cdf_cutoff(dist, default_cdf_cutoff, cdf_cutoff_set)
```

## Arguments

- dist:

  A \<dist_spec\>

- default_cdf_cutoff:

  Numeric; default CDF cutoff to be used if an unconstrained
  distribution is passed as `dist`. If `dist` is already constrained by
  having a maximum or CDF cutoff this is ignored. Note that this can
  only be done for \<dist_spec\> objects with fixed parameters.

- cdf_cutoff_set:

  Logical; whether the default CDF cutoff has been set by the user; if
  yes and `dist` is constrained a warning is issued

## Value

A \<dist_spec\> with the default CDF cutoff set if previously not
constrained
