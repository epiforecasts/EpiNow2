# Generation Time Distribution Options

**\[stable\]** Returns generation time parameters in a format for lower
level model use.

## Usage

``` r
gt_opts(dist = Fixed(1), default_cdf_cutoff = 0.001, weight_prior = TRUE)

generation_time_opts(
  dist = Fixed(1),
  default_cdf_cutoff = 0.001,
  weight_prior = TRUE
)
```

## Arguments

- dist:

  A delay distribution or series of delay distributions . If no
  distribution is given a fixed generation time of 1 will be assumed. If
  passing a nonparametric distribution the first element should be zero
  (see *Details* section)

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

A `<generation_time_opts>` object summarising the input delay
distributions.

## Details

Because the discretised renewal equation used in the package does not
support zero generation times, any distribution specified here will be
left-truncated at one, i.e. the first element of the nonparametric or
discretised probability distribution used for the generation time is set
to zero and the resulting distribution renormalised.

## See also

[`convert_to_logmean()`](https://epiforecasts.io/EpiNow2/reference/convert_to_logmean.md)
[`convert_to_logsd()`](https://epiforecasts.io/EpiNow2/reference/convert_to_logsd.md)
[`bootstrapped_dist_fit()`](https://epiforecasts.io/EpiNow2/reference/bootstrapped_dist_fit.md)
[`Gamma()`](https://epiforecasts.io/EpiNow2/reference/Distributions.md)
[`LogNormal()`](https://epiforecasts.io/EpiNow2/reference/Distributions.md)
[`Fixed()`](https://epiforecasts.io/EpiNow2/reference/Distributions.md)

## Examples

``` r
# default settings with a fixed generation time of 1
generation_time_opts()
#> Warning: ! No generation time distribution given. Using a fixed generation time of 1
#>   day, i.e. the reproduction number is the same as the daily growth rate.
#> ℹ If this was intended then this warning can be silenced by setting `dist =
#>   Fixed(1)`'.
#> - fixed value:
#>   1

# A fixed gamma distributed generation time
generation_time_opts(Gamma(mean = 3, sd = 2, max = 14))
#> - gamma distribution (max: 14):
#>   shape:
#>     2.2
#>   rate:
#>     0.75

# An uncertain gamma distributed generation time
generation_time_opts(
  Gamma(
    shape = Normal(mean = 3, sd = 1),
    rate = Normal(mean = 2, sd = 0.5),
    max = 14
  )
)
#> - gamma distribution (max: 14):
#>   shape:
#>     - normal distribution:
#>       mean:
#>         3
#>       sd:
#>         1
#>   rate:
#>     - normal distribution:
#>       mean:
#>         2
#>       sd:
#>         0.5

# An example generation time
gt_opts(example_generation_time)
#> - gamma distribution (max: 14):
#>   shape:
#>     - normal distribution:
#>       mean:
#>         1.4
#>       sd:
#>         0.48
#>   rate:
#>     - normal distribution:
#>       mean:
#>         0.38
#>       sd:
#>         0.25
```
