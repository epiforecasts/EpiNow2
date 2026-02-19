# Back Calculation Options

**\[stable\]** Defines a list specifying the optional arguments for the
back calculation of cases. Only used if `rt = NULL`.

## Usage

``` r
backcalc_opts(
  prior = c("reports", "none", "infections"),
  prior_window = 14,
  rt_window = 1
)
```

## Arguments

- prior:

  A character string defaulting to "reports". Defines the prior to use
  when deconvolving. Currently implemented options are to use smoothed
  mean delay shifted reported cases ("reports"), to use the estimated
  infections from the previous time step seeded for the first time step
  using mean shifted reported cases ("infections"), or no prior
  ("none"). Using no prior will result in poor real time performance. No
  prior and using infections are only supported when a Gaussian process
  is present . If observed data is not reliable then it a sensible first
  step is to explore increasing the `prior_window` wit a sensible second
  step being to no longer use reported cases as a prior (i.e set
  `prior = "none"`).

- prior_window:

  Integer, defaults to 14 days. The mean centred smoothing window to
  apply to mean shifted reports (used as a prior during back
  calculation). 7 days is minimum recommended settings as this smooths
  day of the week effects but depending on the quality of the data and
  the amount of information users wish to use as a prior (higher values
  equalling a less informative prior).

- rt_window:

  Integer, defaults to 1. The size of the centred rolling average to use
  when estimating Rt. This must be odd so that the central estimate is
  included.

## Value

A `<backcalc_opts>` object of back calculation settings.

## Examples

``` r
# default settings
backcalc_opts()
#> $prior
#> [1] "reports"
#> 
#> $prior_window
#> [1] 14
#> 
#> $rt_window
#> [1] 1
#> 
#> attr(,"class")
#> [1] "backcalc_opts" "list"         
```
