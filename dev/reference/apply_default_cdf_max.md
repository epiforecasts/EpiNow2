# Apply default CDF level to a \<dist_spec\> if it is unconstrained

Apply default CDF level to a \<dist_spec\> if it is unconstrained

## Usage

``` r
apply_default_cdf_max(dist, default_cdf_max, cdf_max_set)
```

## Arguments

- dist:

  A \<dist_spec\>

- default_cdf_max:

  Numeric; default CDF level to keep the distribution up to if an
  unconstrained distribution is passed as `dist`. If `dist` is already
  constrained by having a maximum or CDF level this is ignored. Note
  that this can only be done for \<dist_spec\> objects with fixed
  parameters. Defaults to `getOption("EpiNow2.cdf_max", 0.999)`, so a
  session-wide default can be set with, e.g.,
  `options(EpiNow2.cdf_max = 0.995)`.

- cdf_max_set:

  Logical; whether the default CDF level has been set by the user; if
  yes and `dist` is constrained a warning is issued

## Value

A \<dist_spec\> with the default CDF level set if previously not
constrained
