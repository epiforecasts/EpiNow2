# Applies a threshold to all nonparametric distributions in a \<dist_spec\>

**\[deprecated\]** This function is deprecated. Use
[`bound_dist()`](https://epiforecasts.io/EpiNow2/dev/reference/bound_dist.md)
instead.

## Usage

``` r
apply_tolerance(x, tolerance)
```

## Arguments

- x:

  A `<dist_spec>`

- tolerance:

  Numeric; the desired tolerance level. Any part of the cumulative
  distribution function beyond 1 minus this tolerance level is removed.

## Value

A `<dist_spec>` where probability masses below the threshold level have
been removed
