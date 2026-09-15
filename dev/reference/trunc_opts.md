# Truncation Distribution Options

Returns a truncation distribution formatted for usage by downstream
functions. See
[`estimate_truncation()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_truncation.md)
for an approach to estimate these distributions.

## Usage

``` r
trunc_opts(
  dist = Fixed(0),
  default_cdf_max = getOption("EpiNow2.cdf_max", 0.999),
  weight_prior = FALSE,
  default_cdf_cutoff = lifecycle::deprecated()
)
```

## Arguments

- dist:

  A delay distribution or series of delay distributions reflecting the
  truncation. It can be specified using the probability distributions
  interface in `distspec` (See
  [`?distspec::Distributions`](https://epiforecasts.io/distspec/reference/Distributions.html))
  or estimated using
  [`estimate_truncation()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_truncation.md),
  which returns a `dist` object, suited for use here out-of-box. Default
  is a fixed distribution with maximum 0, i.e. no truncation.

- default_cdf_max:

  Numeric; default CDF level to keep the distribution up to if an
  unconstrained distribution is passed as `dist`. If `dist` is already
  constrained by having a maximum or CDF level this is ignored. Note
  that this can only be done for \<dist_spec\> objects with fixed
  parameters. Defaults to `getOption("EpiNow2.cdf_max", 0.999)`, so a
  session-wide default can be set with, e.g.,
  `options(EpiNow2.cdf_max = 0.995)`.

- weight_prior:

  Logical; if TRUE, the truncation prior will be weighted by the number
  of observation data points, in doing so approximately placing an
  independent prior at each time step and usually preventing the
  posteriors from shifting. If FALSE (default), no weight will be
  applied, i.e. the truncation distribution will be treated as a single
  parameter.

- default_cdf_cutoff:

  **\[deprecated\]** Use `default_cdf_max` instead.

## Value

A `<trunc_opts>` object summarising the input truncation distribution.

## See also

[`distspec::convert_to_logmean()`](https://epiforecasts.io/distspec/reference/convert_to_logmean.html)
[`distspec::convert_to_logsd()`](https://epiforecasts.io/distspec/reference/convert_to_logsd.html)
[`bootstrapped_dist_fit()`](https://epiforecasts.io/EpiNow2/dev/reference/bootstrapped_dist_fit.md)
[distspec::Distributions](https://epiforecasts.io/distspec/reference/Distributions.html)

## Examples

``` r
# no truncation
trunc_opts()
#> - fixed value:
#>   0

# truncation dist
trunc_opts(dist = LogNormal(mean = 3, sd = 2, max = 10))
#> - lognormal distribution (max: 10):
#>   meanlog:
#>     0.91
#>   sdlog:
#>     0.61
```
