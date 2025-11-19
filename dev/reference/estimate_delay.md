# Estimate a Delay Distribution

**\[maturing\]** Estimate a log normal delay distribution from a vector
of integer delays. Currently this function is a simple wrapper for
[`bootstrapped_dist_fit()`](https://epiforecasts.io/EpiNow2/dev/reference/bootstrapped_dist_fit.md).

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

[`bootstrapped_dist_fit()`](https://epiforecasts.io/EpiNow2/dev/reference/bootstrapped_dist_fit.md)

## Examples

``` r
# \donttest{
delays <- rlnorm(500, log(5), 1)
estimate_delay(delays, samples = 1000, bootstraps = 10)
#> WARN [2025-11-19 13:34:41] dist_fit (chain: 1): Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess - 
#> WARN [2025-11-19 13:34:41] dist_fit (chain: 1): Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess - 
#> WARN [2025-11-19 13:34:43] dist_fit (chain: 1): Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess - 
#> WARN [2025-11-19 13:34:43] dist_fit (chain: 1): Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess - 
#> WARN [2025-11-19 13:34:44] dist_fit (chain: 1): The largest R-hat is 1.07, indicating chains have not mixed.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#r-hat - 
#> WARN [2025-11-19 13:34:44] dist_fit (chain: 1): Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess - 
#> WARN [2025-11-19 13:34:44] dist_fit (chain: 1): Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess - 
#> WARN [2025-11-19 13:34:45] dist_fit (chain: 1): The largest R-hat is 1.11, indicating chains have not mixed.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#r-hat - 
#> WARN [2025-11-19 13:34:45] dist_fit (chain: 1): Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess - 
#> WARN [2025-11-19 13:34:45] dist_fit (chain: 1): Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess - 
#> WARN [2025-11-19 13:34:47] dist_fit (chain: 1): Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess - 
#> WARN [2025-11-19 13:34:47] dist_fit (chain: 1): Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess - 
#> WARN [2025-11-19 13:34:48] dist_fit (chain: 1): Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess - 
#> WARN [2025-11-19 13:34:48] dist_fit (chain: 1): Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess - 
#> WARN [2025-11-19 13:34:50] dist_fit (chain: 1): Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess - 
#> WARN [2025-11-19 13:34:50] dist_fit (chain: 1): Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess - 
#> WARN [2025-11-19 13:34:51] dist_fit (chain: 1): Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess - 
#> WARN [2025-11-19 13:34:51] dist_fit (chain: 1): Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess - 
#> WARN [2025-11-19 13:34:52] dist_fit (chain: 1): The largest R-hat is 1.05, indicating chains have not mixed.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#r-hat - 
#> WARN [2025-11-19 13:34:52] dist_fit (chain: 1): Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess - 
#> WARN [2025-11-19 13:34:52] dist_fit (chain: 1): Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess - 
#> WARN [2025-11-19 13:34:54] dist_fit (chain: 1): The largest R-hat is 1.05, indicating chains have not mixed.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#r-hat - 
#> WARN [2025-11-19 13:34:54] dist_fit (chain: 1): Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess - 
#> WARN [2025-11-19 13:34:54] dist_fit (chain: 1): Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess - 
#> - lognormal distribution (max: 68):
#>   meanlog:
#>     - normal distribution:
#>       mean:
#>         1.6
#>       sd:
#>         0.1
#>   sdlog:
#>     - normal distribution:
#>       mean:
#>         1.1
#>       sd:
#>         0.084
# }
```
