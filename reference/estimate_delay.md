# Estimate a Delay Distribution

**\[maturing\]** Estimate a log normal delay distribution from a vector
of integer delays. Currently this function is a simple wrapper for
[`bootstrapped_dist_fit()`](https://epiforecasts.io/EpiNow2/reference/bootstrapped_dist_fit.md).

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

[`bootstrapped_dist_fit()`](https://epiforecasts.io/EpiNow2/reference/bootstrapped_dist_fit.md)

## Examples

``` r
# \donttest{
# bootstraps and samples have been reduced for this example
delays <- rlnorm(500, log(5), 1)
estimate_delay(delays, samples = 500, bootstraps = 2)
#> WARN [2026-02-09 09:40:58] dist_fit (chain: 1): Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess - 
#> WARN [2026-02-09 09:40:58] dist_fit (chain: 2): Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess - 
#> WARN [2026-02-09 09:40:58] dist_fit (chain: 1): Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess - 
#> WARN [2026-02-09 09:40:58] dist_fit (chain: 2): Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess - 
#> WARN [2026-02-09 09:40:59] dist_fit (chain: 1): Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess - 
#> WARN [2026-02-09 09:40:59] dist_fit (chain: 2): Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess - 
#> WARN [2026-02-09 09:40:59] dist_fit (chain: 1): Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess - 
#> WARN [2026-02-09 09:40:59] dist_fit (chain: 2): Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess - 
#> - lognormal distribution (max: 99):
#>   meanlog:
#>     - normal distribution:
#>       mean:
#>         1.5
#>       sd:
#>         0.088
#>   sdlog:
#>     - normal distribution:
#>       mean:
#>         1.1
#>       sd:
#>         0.056
# }
```
