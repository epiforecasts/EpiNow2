# Estimate a Delay Distribution

**\[deprecated\]** Estimate a log normal delay distribution from a
vector of integer delays.

**This function is deprecated.** Please use
[`estimate_dist()`](https://epiforecasts.io/EpiNow2/reference/estimate_dist.md)
instead, which provides better handling of censoring and truncation.

## Usage

``` r
estimate_delay(delays, ...)
```

## Arguments

- delays:

  Integer vector of delays

- ...:

  Arguments to pass to internal methods.

## Value

A `<dist_spec>` summarising the bootstrapped distribution

## See also

[`estimate_dist()`](https://epiforecasts.io/EpiNow2/reference/estimate_dist.md)
for the recommended replacement

## Examples

``` r
# \donttest{
delays <- rlnorm(500, log(5), 1)
# Old way (deprecated):
# estimate_delay(delays, samples = 1000, bootstraps = 10)

# New way: see ?estimate_dist and
# vignette("estimate_dist_workflow") for date-based usage
# }
```
