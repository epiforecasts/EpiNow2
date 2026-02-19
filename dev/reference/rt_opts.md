# Time-Varying Reproduction Number Options

**\[stable\]** Defines a list specifying the optional arguments for the
time-varying reproduction number. Custom settings can be supplied which
override the defaults.

## Usage

``` r
rt_opts(
  prior = LogNormal(mean = 1, sd = 1),
  use_rt = TRUE,
  rw = 0,
  use_breakpoints = TRUE,
  future = "latest",
  gp_on = c("R_t-1", "R0"),
  pop = Fixed(0),
  pop_period = c("forecast", "all"),
  pop_floor = 1,
  growth_method = c("infections", "infectiousness")
)
```

## Arguments

- prior:

  A `<dist_spec>` giving the prior of the initial reproduction number.
  Ignored if `use_rt` is `FALSE`. Defaults to a LogNormal distribution
  with mean of 1 and standard deviation of 1:
  `LogNormal(mean = 1, sd = 1)`. A lower limit of 0 will be enforced
  automatically.

- use_rt:

  Logical, defaults to `TRUE`. Should Rt be used to generate infections
  and hence reported cases.

- rw:

  Numeric step size of the random walk, defaults to 0. To specify a
  weekly random walk set `rw = 7`. For more custom break point settings
  consider passing in a `breakpoints` variable as outlined in the next
  section.

- use_breakpoints:

  Logical, defaults to `TRUE`. Should break points be used if present as
  a `breakpoint` variable in the input data. Break points should be
  defined as 1 if present and otherwise 0. By default breakpoints are
  fit jointly with a global non-parametric effect and so represent a
  conservative estimate of break point changes (alter this by setting
  `gp = NULL`).

- future:

  A character string or integer. This argument indicates how to set
  future Rt values. Supported options are to project using the Rt model
  ("project"), to use the latest estimate based on partial data
  ("latest"), to use the latest estimate based on data that is over 50%
  complete ("estimate"). If an integer is supplied then the Rt estimate
  from this many days into the future (or past if negative) past will be
  used forwards in time.

- gp_on:

  Character string, defaulting to "R_t-1". Indicates how the Gaussian
  process, if in use, should be applied to Rt. Currently supported
  options are applying the Gaussian process to the last estimated Rt
  (i.e Rt = Rt-1 \* GP), and applying the Gaussian process to a global
  mean (i.e Rt = R0 \* GP). Both should produced comparable results when
  data is not sparse but the method relying on a global mean will revert
  to this for real time estimates, which may not be desirable.

- pop:

  A `<dist_spec>` giving the initial susceptible population size. Used
  to adjust Rt estimates based on the proportion of the population that
  is susceptible. Defaults to `Fixed(0)` which means no population
  adjustment is done. See also `pop_floor` for the numerical stability
  floor used when population adjustment is enabled. When `pop` is
  specified, returned Rt estimates are adjusted for susceptible
  depletion (accounting for population immunity), and unadjusted Rt
  estimates are also provided in a separate output variable
  `R_unadjusted`. Adjusted Rt represents the effective reproduction
  number given the current susceptible population, whilst unadjusted Rt
  represents the reproduction number that would occur in a fully
  susceptible population.

- pop_period:

  Character string, defaulting to "forecast". Controls when susceptible
  population adjustment is applied. "forecast" only applies the
  adjustment to forecasts whilst "all" applies it to both data and
  forecasts.

- pop_floor:

  Numeric. Minimum susceptible population used as a floor when adjusting
  for population depletion. This prevents numerical instability
  (division by zero) when the susceptible population approaches zero.
  Defaults to 1.0. Can be interpreted as representing a minimal ongoing
  import level. Note that if pop_floor \> 0, cumulative infections can
  exceed the population size, though this effect is negligible when
  pop_floor is very small compared to the population size.

- growth_method:

  Method used to compute growth rates from Rt. Options are "infections"
  (default) and "infectiousness". The option "infections" uses the
  classical approach, i.e. computing the log derivative on the number of
  new infections. The option "infectiousness" uses an alternative
  approach by Parag et al., which computes the log derivative of the
  infectiousness (i.e. the convolution of past infections with the
  generation time) and shifts it by the mean generation time. This can
  provide better stability and temporal matching with Rt. Note that, due
  to the temporal shift the "infectiousness" method results in undefined
  (NaN) growth rates for the most recent time points (equal to the mean
  generation time).

## Value

An `<rt_opts>` object with settings defining the time-varying
reproduction number.

## References

Parag, K. V., Thompson, R. N. & Donnelly, C. A. Are epidemic growth
rates more informative than reproduction numbers? Journal of the Royal
Statistical Society: Series A (Statistics in Society) 185, S5–S15
(2022).

## Examples

``` r
# default settings
rt_opts()
#> $use_rt
#> [1] TRUE
#> 
#> $rw
#> [1] 0
#> 
#> $use_breakpoints
#> [1] TRUE
#> 
#> $future
#> [1] "latest"
#> 
#> $gp_on
#> [1] "R_t-1"
#> 
#> $pop_period
#> [1] "forecast"
#> 
#> $pop_floor
#> [1] 1
#> 
#> $growth_method
#> [1] "infections"
#> 
#> $pop
#> - fixed value:
#>   0
#> 
#> $prior
#> - lognormal distribution:
#>   meanlog:
#>     -0.35
#>   sdlog:
#>     0.83
#> 
#> attr(,"class")
#> [1] "rt_opts" "list"   

# add a custom length scale
rt_opts(prior = LogNormal(mean = 2, sd = 1))
#> $use_rt
#> [1] TRUE
#> 
#> $rw
#> [1] 0
#> 
#> $use_breakpoints
#> [1] TRUE
#> 
#> $future
#> [1] "latest"
#> 
#> $gp_on
#> [1] "R_t-1"
#> 
#> $pop_period
#> [1] "forecast"
#> 
#> $pop_floor
#> [1] 1
#> 
#> $growth_method
#> [1] "infections"
#> 
#> $pop
#> - fixed value:
#>   0
#> 
#> $prior
#> - lognormal distribution:
#>   meanlog:
#>     0.58
#>   sdlog:
#>     0.47
#> 
#> attr(,"class")
#> [1] "rt_opts" "list"   

# add a weekly random walk
rt_opts(rw = 7)
#> $use_rt
#> [1] TRUE
#> 
#> $rw
#> [1] 7
#> 
#> $use_breakpoints
#> [1] TRUE
#> 
#> $future
#> [1] "latest"
#> 
#> $gp_on
#> [1] "R_t-1"
#> 
#> $pop_period
#> [1] "forecast"
#> 
#> $pop_floor
#> [1] 1
#> 
#> $growth_method
#> [1] "infections"
#> 
#> $pop
#> - fixed value:
#>   0
#> 
#> $prior
#> - lognormal distribution:
#>   meanlog:
#>     -0.35
#>   sdlog:
#>     0.83
#> 
#> attr(,"class")
#> [1] "rt_opts" "list"   
```
