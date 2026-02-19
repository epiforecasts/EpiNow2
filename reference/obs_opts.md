# Observation Model Options

**\[stable\]** Defines a list specifying the structure of the
observation model. Custom settings can be supplied which override the
defaults.

## Usage

``` r
obs_opts(
  family = c("negbin", "poisson"),
  dispersion = Normal(mean = 0, sd = 0.25),
  weight = 1,
  week_effect = TRUE,
  week_length = 7,
  scale = Fixed(1),
  na = c("missing", "accumulate"),
  likelihood = TRUE,
  return_likelihood = FALSE,
  phi
)
```

## Arguments

- family:

  Character string defining the observation model. Options are Negative
  binomial ("negbin"), the default, and Poisson.

- dispersion:

  A `<dist_spec>` specifying a prior on the dispersion parameter of the
  reporting process, used only if `familiy` is "negbin". Internally
  parameterised such that this parameter is one over the square root of
  the `phi` parameter for overdispersion of the [negative binomial
  distribution](https://mc-stan.org/docs/functions-reference/unbounded_discrete_distributions.html#neg-binom-2-log).
  Defaults to a half-normal distribution with mean of 0 and standard
  deviation of 0.25: `Normal(mean = 0, sd = 0.25)`. A lower limit of
  zero will be enforced automatically.

- weight:

  Numeric, defaults to 1. Weight to give the observed data in the log
  density.

- week_effect:

  Logical defaulting to `TRUE`. Should a day of the week effect be used
  in the observation model.

- week_length:

  Numeric assumed length of the week in days, defaulting to 7 days. This
  can be modified if data aggregated over a period other than a week or
  if data has a non-weekly periodicity.

- scale:

  A `<dist_spec>` specifying a prior on the scaling factor to be applied
  to map latent infections (convolved to date of report). Defaults to a
  fixed value of 1, i.e. no scaling: `Fixed(1)`. A lower limit of zero
  will be enforced automatically. If setting to a prior distribution and
  no overreporting is expected, it might be sensible to set a maximum of
  1 via the `max` option when declaring the distribution.

- na:

  Deprecated; use the
  [`fill_missing()`](https://epiforecasts.io/EpiNow2/reference/fill_missing.md)
  function instead

- likelihood:

  Logical, defaults to `TRUE`. Should the likelihood be included in the
  model.

- return_likelihood:

  Logical, defaults to `FALSE`. Should the likelihood be returned by the
  model.

- phi:

  deprecated; use `dispersion` instead

## Value

An `<obs_opts>` object of observation model settings.

## Examples

``` r
# default settings
obs_opts()
#> $family
#> [1] "negbin"
#> 
#> $dispersion
#> - normal distribution:
#>   mean:
#>     0
#>   sd:
#>     0.25
#> 
#> $weight
#> [1] 1
#> 
#> $week_effect
#> [1] TRUE
#> 
#> $week_length
#> [1] 7
#> 
#> $scale
#> - fixed value:
#>   1
#> 
#> $accumulate
#> [1] 0
#> 
#> $likelihood
#> [1] TRUE
#> 
#> $return_likelihood
#> [1] FALSE
#> 
#> $na_as_missing_default_used
#> [1] TRUE
#> 
#> attr(,"class")
#> [1] "obs_opts" "list"    

# Turn off day of the week effect
obs_opts(week_effect = TRUE)
#> $family
#> [1] "negbin"
#> 
#> $dispersion
#> - normal distribution:
#>   mean:
#>     0
#>   sd:
#>     0.25
#> 
#> $weight
#> [1] 1
#> 
#> $week_effect
#> [1] TRUE
#> 
#> $week_length
#> [1] 7
#> 
#> $scale
#> - fixed value:
#>   1
#> 
#> $accumulate
#> [1] 0
#> 
#> $likelihood
#> [1] TRUE
#> 
#> $return_likelihood
#> [1] FALSE
#> 
#> $na_as_missing_default_used
#> [1] TRUE
#> 
#> attr(,"class")
#> [1] "obs_opts" "list"    

# Scale reported data
obs_opts(scale = Normal(mean = 0.2, sd = 0.02))
#> $family
#> [1] "negbin"
#> 
#> $dispersion
#> - normal distribution:
#>   mean:
#>     0
#>   sd:
#>     0.25
#> 
#> $weight
#> [1] 1
#> 
#> $week_effect
#> [1] TRUE
#> 
#> $week_length
#> [1] 7
#> 
#> $scale
#> - normal distribution:
#>   mean:
#>     0.2
#>   sd:
#>     0.02
#> 
#> $accumulate
#> [1] 0
#> 
#> $likelihood
#> [1] TRUE
#> 
#> $return_likelihood
#> [1] FALSE
#> 
#> $na_as_missing_default_used
#> [1] TRUE
#> 
#> attr(,"class")
#> [1] "obs_opts" "list"    
```
