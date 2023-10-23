
# EpiNow2: Estimate real-time case counts and time-varying epidemiological parameters

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#maturing)
[![R-CMD-check](https://github.com/epiforecasts/EpiNow2/workflows/R-CMD-check/badge.svg)](https://github.com/epiforecasts/EpiNow2/actions)
[![codecov](https://codecov.io/gh/epiforecasts/EpiNow2/branch/main/graph/badge.svg?token=FZWwEMdpq6)](https://app.codecov.io/gh/epiforecasts/EpiNow2)
[![metacran
downloads](http://cranlogs.r-pkg.org/badges/grand-total/EpiNow2?color=ff69b4)](https://cran.r-project.org/package=EpiNow2)

[![MIT
license](https://img.shields.io/badge/License-MIT-blue.svg)](https://github.com/epiforecasts/EpiNow2/blob/main/LICENSE.md/)
[![GitHub
contributors](https://img.shields.io/github/contributors/epiforecasts/EpiNow2)](https://github.com/epiforecasts/EpiNow2/graphs/contributors)
[![universe](https://epiforecasts.r-universe.dev/badges/EpiNow2)](http://epiforecasts.r-universe.dev/ui/#package:EpiNow2)
[![GitHub
commits](https://img.shields.io/github/commits-since/epiforecasts/EpiNow2/v1.4.0.svg?color=orange)](https://GitHub.com/epiforecasts/EpiNow2/commit/main/)
[![DOI](https://zenodo.org/badge/272995211.svg)](https://zenodo.org/badge/latestdoi/272995211)

This package estimates the time-varying reproduction number, growth
rate, and doubling time using a range of open-source tools ([Abbott et
al.](https://doi.org/10.12688/wellcomeopenres.16006.1)), and current
best practices ([Gostic et
al.](https://doi.org/10.1371/journal.pcbi.1008409)). It aims to help
users avoid some of the limitations of naive implementations in a
framework that is informed by community feedback and is actively
supported.

It estimates the time-varying reproduction number on cases by date of
infection (using a similar approach to that implemented in
[`{EpiEstim}`](https://github.com/mrc-ide/EpiEstim)). Imputed infections
are then mapped to observed data (for example cases by date of report)
via a series of uncertain delay distributions (in the examples in the
package documentation these are an incubation period and a reporting
delay) and a reporting model that can include weekly periodicity.

Uncertainty is propagated from all inputs into the final parameter
estimates, helping to mitigate spurious findings. This is handled
internally. The time-varying reproduction estimates and the uncertain
generation time also give time-varying estimates of the rate of growth.

The default model uses a non-stationary Gaussian process to estimate the
time-varying reproduction number and then infer infections. Other
options include:

- A stationary Gaussian process (faster to estimate but currently gives
  reduced performance for real time estimates).
- User specified breakpoints.
- A fixed reproduction number.
- As piecewise constant by combining a fixed reproduction number with
  breakpoints.
- As a random walk (by combining a fixed reproduction number with
  regularly spaced breakpoints (i.e weekly)).
- Inferring infections using deconvolution/back-calculation and then
  calculating the time-varying reproduction number.
- Adjustment for the remaining susceptible population beyond the
  forecast horizon.

These options generally reduce runtimes at the cost of the granularity
of estimates or at the cost of real-time performance.

The documentation for
[`estimate_infections`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.html)
provides examples of the implementation of the different options
available.

Forecasting is also supported for the time-varying reproduction number,
infections and reported cases using the same generative process approach
as used for estimation.

A simple example of using the package to estimate a national Rt for
Covid-19 can be found
[here](https://gist.github.com/seabbs/163d0f195892cde685c70473e1f5e867).

`EpiNow2` also supports adjustment for truncated data via
`estimate_truncation()` (though users may be interested in more
flexibility and if so should check out the
[`epinowcast`](https://package.epinowcast.org/) package), and for
estimating dependent observations (i.e deaths based on hospital
admissions) using `estimate_secondary()`.

## Installation

Install the released version of the package:

``` r
install.packages("EpiNow2")
```

Install the development version of the package with:

``` r
install.packages("EpiNow2", repos = "https://epiforecasts.r-universe.dev")
```

Alternatively, install the development version of the package with
[pak](https://pak.r-lib.org/) as follows (few users should need to do
this):

``` r
# check whether {pak} is installed
if (!require("pak")) {
  install.packages("pak")
}
pak::pkg_install("epiforecasts/EpiNow2")
```

If using `pak` fails, try:

``` r
# check whether {remotes} is installed
if (!require("remotes")) {
  install.packages("remotes")
}
remotes::install_github("epiforecasts/EpiNow2")
```

Windows users will need a working installation of Rtools in order to
build the package from source. See
[here](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started#checking-the-c-toolchain)
for a guide to installing Rtools for use with Stan (which is the
statistical modelling platform used for the underlying model). For
simple deployment/development a prebuilt docker image is also available
(see documentation
[here](https://github.com/epiforecasts/EpiNow2/wiki/Docker)).

## Quick start

`{EpiNow2}` is designed to be used with a single function call or to be
used in an ad-hoc fashion via individual function calls. The core
functions of `{EpiNow2}` are the two single-call functions
[`epinow()`](https://epiforecasts.io/EpiNow2/reference/epinow.html),
[`regional_epinow()`](https://epiforecasts.io/EpiNow2/reference/regional_epinow.html),
plus functions
[`estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.html),
[`estimate_secondary()`](https://epiforecasts.io/EpiNow2/reference/estimate_secondary.html)
and
[`estimate_truncation()`](https://epiforecasts.io/EpiNow2/reference/estimate_truncation.html).
In the following section we give an overview of the simple use case for
`epinow` and `regional_epinow`.
[`estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.html)
can be used on its own to infer the underlying infection case curve from
reported cases and estimate Rt. Estimating the underlying infection case
curve via back-calculation (and then calculating Rt) is substantially
less computationally demanding than generating using default settings
but may result in less reliable estimates of Rt. For more details on
using each function see the [function
documentation](https://epiforecasts.io/EpiNow2/reference/index.html).

The first step to using the package is to load it as follows.

``` r
library(EpiNow2)
```

### Reporting delays, incubation period and generation time

Distributions can either be fitted using package functionality or
determined elsewhere and then defined with uncertainty for use in
`{EpiNow2}`. When data is supplied a subsampled bootstrapped lognormal
will be fit (to account for uncertainty in the observed data without
being biased by changes in incidence). An arbitrary number of delay
distributions are supported with the most common use case likely to be a
incubation period followed by a reporting delay.

For example if data on the delay between onset and infection was
available we could fit a distribution to it with appropriate uncertainty
as follows (note this is a synthetic example),

``` r
reporting_delay <- estimate_delay(
  rlnorm(1000, log(2), 1),
  max_value = 14, bootstraps = 1
)
```

If data was not available we could instead make an informed estimate of
the likely delay. We choose a lognormal distribution with mean 2,
standard deviation 1 (both with some uncertainty) and a maximum of 10.
*This is just an example and unlikely to apply in any particular use
case*.

``` r
reporting_delay <- dist_spec(
  mean = convert_to_logmean(2, 1),
  mean_sd = 0.1,
  sd = convert_to_logsd(2, 1),
  sd_sd = 0.1,
  max = 10,
  dist = "lognormal"
)
#> Warning: The meaning of the 'max' argument has changed compared to previous versions. It now indicates the maximum of a distribution rather than the length of the probability mass function (including 0) that it represented previously. To replicate previous behaviour reduce max by 1.
#> This warning is displayed once every 8 hours.
reporting_delay
#> 
#>   Uncertain lognormal distribution with (untruncated) logmean 0.58 (SD 0.1) and logSD 0.47 (SD 0.1)
```

We use example literature estimates for the incubation period and
generation time of Covid-19 (see
[here](https://github.com/epiforecasts/EpiNow2/tree/main/data-raw) for
the code that generates these estimates). *These distributions are
unlikely to be applicable for your use case. We strongly recommend
investigating what might be the best distributions to use in any given
use case.*

``` r
example_generation_time
#> 
#>   Uncertain gamma distribution with (untruncated) mean 3.6 (SD 0.71) and SD 3.1 (SD 0.77)
example_incubation_period
#> 
#>   Uncertain lognormal distribution with (untruncated) logmean 1.6 (SD 0.064) and logSD 0.42 (SD 0.069)
```

### [epinow()](https://epiforecasts.io/EpiNow2/reference/epinow.html)

This function represents the core functionality of the package and
includes results reporting, plotting and optional saving. It requires a
data frame of cases by date of report and the distributions defined
above.

Load example case data from `{EpiNow2}`.

``` r
reported_cases <- example_confirmed[1:60]
head(reported_cases)
#>          date confirm
#> 1: 2020-02-22      14
#> 2: 2020-02-23      62
#> 3: 2020-02-24      53
#> 4: 2020-02-25      97
#> 5: 2020-02-26      93
#> 6: 2020-02-27      78
```

Estimate cases by date of infection, the time-varying reproduction
number, the rate of growth and forecast these estimates into the future
by 7 days. Summarise the posterior and return a summary table and plots
for reporting purposes. If a `target_folder` is supplied results can be
internally saved (with the option to also turn off explicit returning of
results). Here we use the default model parameterisation that
prioritises real-time performance over run-time or other considerations.
For other formulations see the documentation for
`estimate_infections()`.

``` r
estimates <- epinow(
  reported_cases = reported_cases,
  generation_time = generation_time_opts(example_generation_time),
  delays = delay_opts(example_incubation_period + reporting_delay),
  rt = rt_opts(prior = list(mean = 2, sd = 0.2)),
  stan = stan_opts(cores = 4, control = list(adapt_delta = 0.99)),
  verbose = interactive()
)
names(estimates)
#> [1] "estimates"                "estimated_reported_cases"
#> [3] "summary"                  "plots"                   
#> [5] "timing"
```

Both summary measures and posterior samples are returned for all
parameters in an easily explored format which can be accessed using
`summary`. The default is to return a summary table of estimates for key
parameters at the latest date partially supported by data.

``` r
knitr::kable(summary(estimates))
```

| measure                               | estimate                |
|:--------------------------------------|:------------------------|
| New confirmed cases by infection date | 2289 (1115 – 4367)      |
| Expected change in daily cases        | Likely decreasing       |
| Effective reproduction no.            | 0.89 (0.62 – 1.2)       |
| Rate of growth                        | -0.025 (-0.096 – 0.036) |
| Doubling/halving time (days)          | -27 (19 – -7.2)         |

Summarised parameter estimates can also easily be returned, either
filtered for a single parameter or for all parameters.

``` r
head(summary(estimates, type = "parameters", params = "R"))
#>          date variable strat     type   median     mean         sd lower_90
#> 1: 2020-02-22        R  <NA> estimate 2.212959 2.220541 0.14576748 1.988091
#> 2: 2020-02-23        R  <NA> estimate 2.182545 2.186632 0.12162401 1.992521
#> 3: 2020-02-24        R  <NA> estimate 2.150369 2.150836 0.10169055 1.986534
#> 4: 2020-02-25        R  <NA> estimate 2.113105 2.113283 0.08594870 1.974178
#> 5: 2020-02-26        R  <NA> estimate 2.073614 2.074148 0.07420031 1.955402
#> 6: 2020-02-27        R  <NA> estimate 2.034058 2.033642 0.06600593 1.928757
#>    lower_50 lower_20 upper_20 upper_50 upper_90
#> 1: 2.121108 2.179966 2.251215 2.319625 2.463337
#> 2: 2.103825 2.154804 2.213847 2.267385 2.395556
#> 3: 2.081757 2.123637 2.174697 2.216957 2.325451
#> 4: 2.055113 2.091263 2.133835 2.167701 2.262721
#> 5: 2.024078 2.053914 2.092803 2.120727 2.202303
#> 6: 1.988182 2.016474 2.051127 2.074294 2.145812
```

Reported cases are returned in a separate data frame in order to
streamline the reporting of forecasts and for model evaluation.

``` r
head(summary(estimates, output = "estimated_reported_cases"))
#>          date  type median     mean       sd lower_90 lower_50 lower_20
#> 1: 2020-02-22 gp_rt     66  68.2645 19.68574    40.00       54       61
#> 2: 2020-02-23 gp_rt     77  78.8550 21.40584    48.00       64       71
#> 3: 2020-02-24 gp_rt     76  77.8660 21.55264    47.00       63       71
#> 4: 2020-02-25 gp_rt     73  75.0555 20.84360    46.00       60       67
#> 5: 2020-02-26 gp_rt     78  80.2720 22.22163    48.95       65       73
#> 6: 2020-02-27 gp_rt    111 112.9485 29.56885    70.95       93      103
#>    upper_20 upper_50 upper_90
#> 1:       71       80   103.00
#> 2:       82       91   116.05
#> 3:       81       91   117.00
#> 4:       78       88   112.00
#> 5:       83       93   120.00
#> 6:      118      131   165.00
```

A range of plots are returned (with the single summary plot shown
below). These plots can also be generated using the following `plot`
method.

``` r
plot(estimates)
```

![](man/figures/unnamed-chunk-15-1.png)<!-- -->

### [regional_epinow()](https://epiforecasts.io/EpiNow2/reference/regional_epinow.html)

The `regional_epinow()` function runs the `epinow()` function across
multiple regions in an efficient manner.

Define cases in multiple regions delineated by the region variable.

``` r
reported_cases <- data.table::rbindlist(list(
  data.table::copy(reported_cases)[, region := "testland"],
  reported_cases[, region := "realland"]
))
head(reported_cases)
#>          date confirm   region
#> 1: 2020-02-22      14 testland
#> 2: 2020-02-23      62 testland
#> 3: 2020-02-24      53 testland
#> 4: 2020-02-25      97 testland
#> 5: 2020-02-26      93 testland
#> 6: 2020-02-27      78 testland
```

Calling `regional_epinow()` runs the `epinow()` on each region in turn
(or in parallel depending on the settings used). Here we switch to using
a weekly random walk rather than the full Gaussian process model giving
us piecewise constant estimates by week.

``` r
estimates <- regional_epinow(
  reported_cases = reported_cases,
  generation_time = generation_time_opts(example_generation_time),
  delays = delay_opts(example_incubation_period + reporting_delay),
  rt = rt_opts(prior = list(mean = 2, sd = 0.2), rw = 7),
  gp = NULL,
  stan = stan_opts(cores = 4, warmup = 250, samples = 1000)
)
#> INFO [2023-10-23 16:31:10] Producing following optional outputs: regions, summary, samples, plots, latest
#> INFO [2023-10-23 16:31:10] Reporting estimates using data up to: 2020-04-21
#> INFO [2023-10-23 16:31:11] No target directory specified so returning output
#> INFO [2023-10-23 16:31:11] Producing estimates for: testland, realland
#> INFO [2023-10-23 16:31:11] Regions excluded: none
#> INFO [2023-10-23 16:31:59] Completed estimates for: testland
#> INFO [2023-10-23 16:32:51] Completed estimates for: realland
#> INFO [2023-10-23 16:32:51] Completed regional estimates
#> INFO [2023-10-23 16:32:51] Regions with estimates: 2
#> INFO [2023-10-23 16:32:51] Regions with runtime errors: 0
#> INFO [2023-10-23 16:32:51] Producing summary
#> INFO [2023-10-23 16:32:51] No summary directory specified so returning summary output
#> INFO [2023-10-23 16:32:51] No target directory specified so returning timings
```

Results from each region are stored in a `regional` list with across
region summary measures and plots stored in a `summary` list. All
results can be set to be internally saved by setting the `target_folder`
and `summary_dir` arguments. Each region can be estimated in parallel
using the `{future}` package (when in most scenarios `cores` should be
set to 1). For routine use each MCMC chain can also be run in parallel
(with `future` = TRUE) with a time out (`max_execution_time`) allowing
for partial results to be returned if a subset of chains is running
longer than expected. See the documentation for the `{future}` package
for details on nested futures.

Summary measures that are returned include a table formatted for
reporting (along with raw results for further processing). Futures
updated will extend the S3 methods used above to smooth access to this
output.

``` r
knitr::kable(estimates$summary$summarised_results$table)
```

| Region   | New confirmed cases by infection date | Expected change in daily cases | Effective reproduction no. | Rate of growth          | Doubling/halving time (days) |
|:---------|:--------------------------------------|:-------------------------------|:---------------------------|:------------------------|:-----------------------------|
| realland | 2149 (1092 – 4175)                    | Likely decreasing              | 0.86 (0.61 – 1.2)          | -0.032 (-0.097 – 0.031) | -21 (22 – -7.2)              |
| testland | 2128 (1116 – 4273)                    | Likely decreasing              | 0.85 (0.62 – 1.2)          | -0.033 (-0.095 – 0.033) | -21 (21 – -7.3)              |

A range of plots are again returned (with the single summary plot shown
below).

``` r
estimates$summary$summary_plot
```

![](man/figures/unnamed-chunk-19-1.png)<!-- -->

### Reporting templates

Rmarkdown templates are provided in the package (`templates`) for
semi-automated reporting of estimates. If using these templates to
report your results please highlight our
[limitations](https://doi.org/10.12688/wellcomeopenres.16006.1) as these
are key to understanding the results from `{EpiNow2}` .

## Contributing

File an issue [here](https://github.com/epiforecasts/EpiNow2/issues) if
you have identified an issue with the package. Please note that due to
operational constraints priority will be given to users informing
government policy or offering methodological insights. We welcome all
contributions, in particular those that improve the approach or the
robustness of the code base. We also welcome additions and extensions to
the underlying model either in the form of options or improvements.
