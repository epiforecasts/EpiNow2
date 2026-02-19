# Define bounds of a `<dist_spec>`

**\[experimental\]** This sets attributes for further processing

## Usage

``` r
bound_dist(x, max = Inf, cdf_cutoff = 0)
```

## Arguments

- x:

  A `<dist_spec>`.

- max:

  Numeric, maximum value of the distribution. The distribution will be
  truncated at this value. Default: `Inf`, i.e. no maximum.

- cdf_cutoff:

  Numeric; the desired CDF cutoff. Any part of the cumulative
  distribution function beyond 1 minus the value of this argument is
  removed. Default: `0`, i.e. use the full distribution.

## Value

a `<dist_spec>` with relevant attributes set that define its bounds
