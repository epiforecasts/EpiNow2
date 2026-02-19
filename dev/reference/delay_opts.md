# Delay Distribution Options

**\[stable\]** Returns delay distributions formatted for usage by
downstream functions.

## Usage

``` r
delay_opts(dist = Fixed(0), default_cdf_cutoff = 0.001, weight_prior = TRUE)
```

## Arguments

- dist:

  A delay distribution or series of delay distributions. Default is a
  fixed distribution with all mass at 0, i.e. no delay.

- default_cdf_cutoff:

  Numeric; default CDF cutoff to be used if an unconstrained
  distribution is passed as `dist`. If `dist` is already constrained by
  having a maximum or CDF cutoff this is ignored. Note that this can
  only be done for \<dist_spec\> objects with fixed parameters.

- weight_prior:

  Logical; if TRUE (default), any priors given in `dist` will be
  weighted by the number of observation data points, in doing so
  approximately placing an independent prior at each time step and
  usually preventing the posteriors from shifting. If FALSE, no weight
  will be applied, i.e. any parameters in `dist` will be treated as a
  single parameters.

## Value

A `<delay_opts>` object summarising the input delay distributions.

## See also

[`convert_to_logmean()`](https://epiforecasts.io/EpiNow2/dev/reference/convert_to_logmean.md)
[`convert_to_logsd()`](https://epiforecasts.io/EpiNow2/dev/reference/convert_to_logsd.md)
[`bootstrapped_dist_fit()`](https://epiforecasts.io/EpiNow2/dev/reference/bootstrapped_dist_fit.md)
[`Distributions`](https://epiforecasts.io/EpiNow2/dev/reference/Distributions.md)

## Examples

``` r
# no delays
delay_opts()
#> - fixed value:
#>   0

# A single delay that has uncertainty
delay <- LogNormal(
  meanlog = Normal(1, 0.2),
  sdlog = Normal(0.5, 0.1),
  max = 14
)
delay_opts(delay)
#> - lognormal distribution (max: 14):
#>   meanlog:
#>     - normal distribution:
#>       mean:
#>         1
#>       sd:
#>         0.2
#>   sdlog:
#>     - normal distribution:
#>       mean:
#>         0.5
#>       sd:
#>         0.1

# A single delay without uncertainty
delay <- LogNormal(meanlog = 1, sdlog = 0.5, max = 14)
delay_opts(delay)
#> - lognormal distribution (max: 14):
#>   meanlog:
#>     1
#>   sdlog:
#>     0.5

# Multiple delays (in this case twice the same)
delay_opts(delay + delay)
#> Composite distribution:
#> - lognormal distribution (max: 14):
#>   meanlog:
#>     1
#>   sdlog:
#>     0.5
#> - lognormal distribution (max: 14):
#>   meanlog:
#>     1
#>   sdlog:
#>     0.5
```
