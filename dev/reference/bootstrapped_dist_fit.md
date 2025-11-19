# Fit a Subsampled Bootstrap to Integer Values and Summarise Distribution Parameters

**\[stable\]** Fits an integer adjusted distribution to a subsampled
bootstrap of data and then integrates the posterior samples into a
single set of summary statistics. Can be used to generate a robust
reporting delay that accounts for the fact the underlying delay likely
varies over time or that the size of the available reporting delay
sample may not be representative of the current case load.

## Usage

``` r
bootstrapped_dist_fit(
  values,
  dist = "lognormal",
  samples = 2000,
  bootstraps = 10,
  bootstrap_samples = 250,
  max_value,
  verbose = FALSE
)
```

## Arguments

- values:

  Integer vector of values.

- dist:

  Character string, which distribution to fit. Defaults to lognormal
  (`"lognormal"`) but gamma (`"gamma"`) is also supported.

- samples:

  Numeric, number of samples to take overall from the bootstrapped
  posteriors.

- bootstraps:

  Numeric, defaults to 1. The number of bootstrap samples (with
  replacement) of the delay distribution to take. If `samples` is less
  than `bootstraps`, `samples` takes the value of `bootstraps`.

- bootstrap_samples:

  Numeric, defaults to 250. The number of samples to take in each
  bootstrap if the sample size of the supplied delay distribution is
  less than its value.

- max_value:

  Numeric, defaults to the maximum value in the observed data. Maximum
  delay to allow (added to output but does impact fitting).

- verbose:

  Logical, defaults to `FALSE`. Should progress messages be printed.

## Value

A `<dist_spec>` object summarising the bootstrapped distribution

## Examples

``` r
# \donttest{
# lognormal
delays <- rlnorm(500, log(5), 1)
out <- bootstrapped_dist_fit(delays,
  samples = 1000, bootstraps = 10,
  dist = "lognormal"
)
#> WARN [2025-11-19 13:33:20] dist_fit (chain: 1): The largest R-hat is 1.13, indicating chains have not mixed.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#r-hat - 
#> WARN [2025-11-19 13:33:20] dist_fit (chain: 1): Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess - 
#> WARN [2025-11-19 13:33:20] dist_fit (chain: 1): Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess - 
#> WARN [2025-11-19 13:33:22] dist_fit (chain: 1): Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess - 
#> WARN [2025-11-19 13:33:22] dist_fit (chain: 1): Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess - 
#> WARN [2025-11-19 13:33:23] dist_fit (chain: 1): The largest R-hat is 1.05, indicating chains have not mixed.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#r-hat - 
#> WARN [2025-11-19 13:33:23] dist_fit (chain: 1): Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess - 
#> WARN [2025-11-19 13:33:23] dist_fit (chain: 1): Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess - 
#> WARN [2025-11-19 13:33:25] dist_fit (chain: 1): Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess - 
#> WARN [2025-11-19 13:33:25] dist_fit (chain: 1): Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess - 
#> WARN [2025-11-19 13:33:26] dist_fit (chain: 1): The largest R-hat is 1.08, indicating chains have not mixed.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#r-hat - 
#> WARN [2025-11-19 13:33:26] dist_fit (chain: 1): Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess - 
#> WARN [2025-11-19 13:33:26] dist_fit (chain: 1): Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess - 
#> WARN [2025-11-19 13:33:27] dist_fit (chain: 1): Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess - 
#> WARN [2025-11-19 13:33:27] dist_fit (chain: 1): Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess - 
#> WARN [2025-11-19 13:33:29] dist_fit (chain: 1): Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess - 
#> WARN [2025-11-19 13:33:29] dist_fit (chain: 1): Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess - 
#> WARN [2025-11-19 13:33:30] dist_fit (chain: 1): Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess - 
#> WARN [2025-11-19 13:33:30] dist_fit (chain: 1): Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess - 
#> WARN [2025-11-19 13:33:31] dist_fit (chain: 1): Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess - 
#> WARN [2025-11-19 13:33:31] dist_fit (chain: 1): Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess - 
#> WARN [2025-11-19 13:33:33] dist_fit (chain: 1): Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess - 
#> WARN [2025-11-19 13:33:33] dist_fit (chain: 1): Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess - 
out
#> - lognormal distribution (max: 78):
#>   meanlog:
#>     - normal distribution:
#>       mean:
#>         1.5
#>       sd:
#>         0.099
#>   sdlog:
#>     - normal distribution:
#>       mean:
#>         1.2
#>       sd:
#>         0.076
# }
```
