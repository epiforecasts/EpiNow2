
# EpiNow2: Estimate real-time case counts and time-varying epidemiological parameters


[![R-CMD-check](https://github.com/epiforecasts/EpiNow2/workflows/R-CMD-check/badge.svg)](https://github.com/epiforecasts/EpiNow2/actions) [![Codecov test coverage](https://codecov.io/gh/epiforecasts/EpiNow2/branch/master/graph/badge.svg)](https://codecov.io/gh/epiforecasts/EpiNow2?branch=master) [![HitCount](http://hits.dwyl.com/epiforecasts/EpiNow2.svg)](http://hits.dwyl.com/epiforecasts/EpiNow2) [![metacran downloads](http://cranlogs.r-pkg.org/badges/grand-total/EpiNow2?color=ff69b4)](https://cran.r-project.org/package=EpiNow2)

[![MIT license](https://img.shields.io/badge/License-MIT-blue.svg)](https://lbesson.mit-license.org/)  ![GitHub contributors](https://img.shields.io/github/contributors/epiforecasts/EpiNow2)  [![PRs Welcome](https://img.shields.io/badge/PRs-welcome-yellow.svg)](http://makeapullrequest.com) [![GitHub commits](https://img.shields.io/github/commits-since/epiforecasts/EpiNow2/v1.2.0.svg?color=orange)](https://GitHub.com/epiforecasts/EpiNow2/commit/) [![DOI](https://zenodo.org/badge/272995211.svg)](https://zenodo.org/badge/latestdoi/272995211) 

This package estimates the time-varying reproduction number, rate of
spread, and doubling time using a range of open-source tools ([Abbott et
al.](https://doi.org/10.12688/wellcomeopenres.16006.1)), and current
best practices ([Gostic et
al.](https://doi.org/10.1101/2020.06.18.20134858)). It aims to help
users avoid some of the limitations of naive implementations in a
framework that is informed by community feedback and is under active
development.

It estimates the time-varying reproduction number on cases by date of
infection (using a similar approach to that implemented in the
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
time-varying reproduction number. Other options include:

  - A stationary Gaussian process (faster to estimate but currently
    gives reduced performance for real time estimates).
  - User specified breakpoints.
  - A fixed reproduction number.
  - As piecewise constant by combining a fixed reproduction number with
    breakpoints.
  - As a random walk (by combining a fixed reproduction number with
    regularly spaced breakpoints (i.e weekly)).

The documentation for
[`estimate_infections`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.html)
provides examples of the different options available.

Forecasting is also supported for the time-varying reproduction number,
infections and reported cases. The time-varying reproduction number can
be forecast forwards in time using an integration with the
[`{EpiSoon}`](https://epiforecasts.io/EpiSoon/) package, and converted
to a case forecast using the renewal equation. Alternatively, the
time-varying reproduction number and cases can be forecast using a
Gaussian process.

As a standalone tool, non-parametric back-calculation is also supported.
This uses a novel formulation based on a smoothed mean delay shift of
reported cases combined with a Gaussian process to determine the most
likely outbreak trajectory. Again see the documentation for
[`estimate_infections`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.html)
for an example.

## Installation

Install the stable version (*Note: The CRAN release of `EpiNow2 1.1.0`
has a substantially different interface to the one described below. It
is suggested to install the development version of the package or to
review the README in `EpiNow2 1.1.0`*) of the package:

``` r
install.packages("EpiNow2")
```

Install the stable development version of the package using
[`{drat}`](https://epiforecasts.io/drat/):

``` r
install.packages("drat")
drat:::add("epiforecasts")
install.packages("EpiNow2")
```

Install the unstable development version of the package with:

``` r
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
[`epinow`](https://epiforecasts.io/EpiNow2/reference/epinow.html),
[`regional_epinow`](https://epiforecasts.io/EpiNow2/reference/regional_epinow.html),
plus functions
[`estimate_infections`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.html),
and
[`forecast_infections`](https://epiforecasts.io/EpiNow2/reference/forecast_infections.html).
In the following section we give an overview of the simple use case for
`epinow` and `regional_epinow`.
[`estimate_infections`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.html)
can be use on its own to infer the underlying infection case curve from
reported cases with Rt optionally returned (on by default). Estimating
the underlying infection case curve alone is substantially less
computationally demanding than also estimating Rt. For more details on
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

``` r
reporting_delay <- bootstrapped_dist_fit(rlnorm(100, log(4), 1), max_value = 30)
```

Here we define the incubation period and generation time based on
literature estimates for Covid-19 (see
[here](https://github.com/epiforecasts/EpiNow/tree/master/data-raw) for
the code that generates these estimates).

``` r
generation_time <- get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
incubation_period <- get_incubation_period(disease = "SARS-CoV-2", source = "lauer")
```

### [epinow](https://epiforecasts.io/EpiNow2/reference/epinow.html)

This function represents the core functionality of the package and
includes results reporting, plotting and optional saving. It requires a
data frame of cases by date of report and the distributions defined
above. An additional forecasting module is supported via `EpiSoon` and
companion packages (see documentation for an example).

Load example case data from `{EpiNow2}`.

``` r
reported_cases <- example_confirmed[1:40]
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
results). *Note that for real use cases more samples and a longer warm
up may be needed*. If compute is limited approximate estimates can be
derived using `method = approximate` which makes use of variational
inference and is likely to be several orders of magnitude faster than
the default method (though this approach is experimental).

``` r
estimates <- epinow(reported_cases = reported_cases, 
                    generation_time = generation_time,
                    delays = list(incubation_period, reporting_delay),
                    logs = NULL)
#> WARN [2020-10-15 18:25:32] epinow: There were 2 divergent transitions after warmup. See
#> http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#> to find out why this is a problem and how to eliminate them. - 
#> WARN [2020-10-15 18:25:32] epinow: Examine the pairs() plot to diagnose sampling problems
#>  - 
#> WARN [2020-10-15 18:25:32] epinow: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> http://mc-stan.org/misc/warnings.html#bulk-ess -
names(estimates)
#> [1] "estimates"                "estimated_reported_cases"
#> [3] "summary"                  "plots"
```

Both summary measures and posterior samples are returned for all
parameters in an easily explored format.

``` r
head(estimates$estimates$samples)
#>      variable  parameter time       date sample     value strat     type
#> 1: infections infections    1 2020-02-12      1  1.920879  <NA> estimate
#> 2: infections infections    2 2020-02-13      1 10.088548  <NA> estimate
#> 3: infections infections    3 2020-02-14      1 17.178889  <NA> estimate
#> 4: infections infections    4 2020-02-15      1 27.634226  <NA> estimate
#> 5: infections infections    5 2020-02-16      1 44.621471  <NA> estimate
#> 6: infections infections    6 2020-02-17      1 44.704228  <NA> estimate
head(estimates$estimates$summarised)
#>          date variable strat     type   median     mean         sd lower_90
#> 1: 2020-02-22        R  <NA> estimate 2.030470 2.041023 0.21881114 1.702665
#> 2: 2020-02-23        R  <NA> estimate 2.013471 2.017060 0.16547736 1.749692
#> 3: 2020-02-24        R  <NA> estimate 1.991144 1.993677 0.12344306 1.796140
#> 4: 2020-02-25        R  <NA> estimate 1.969293 1.970475 0.09440217 1.820491
#> 5: 2020-02-26        R  <NA> estimate 1.945436 1.947055 0.07988297 1.817648
#> 6: 2020-02-27        R  <NA> estimate 1.924229 1.923030 0.07781099 1.793330
#>    lower_50 lower_20 upper_20 upper_50 upper_90
#> 1: 1.896576 1.981515 2.081383 2.177189 2.387305
#> 2: 1.904355 1.972517 2.056468 2.125750 2.284865
#> 3: 1.910323 1.963649 2.022345 2.077005 2.204855
#> 4: 1.906575 1.945704 1.993524 2.030962 2.128385
#> 5: 1.891581 1.924831 1.965225 2.002314 2.080576
#> 6: 1.873226 1.903116 1.942468 1.972435 2.052961
```

Reported cases are returned separately in order to ease reporting of
forecasts and model evaluation.

``` r
head(estimates$estimated_reported_cases$samples)
#>          date sample cases  type
#> 1: 2020-02-22      1    24 gp_rt
#> 2: 2020-02-23      1    53 gp_rt
#> 3: 2020-02-24      1    85 gp_rt
#> 4: 2020-02-25      1   125 gp_rt
#> 5: 2020-02-26      1   101 gp_rt
#> 6: 2020-02-27      1    75 gp_rt
head(estimates$estimated_reported_cases$summarised)
#>          date  type median    mean       sd lower_90 lower_50 lower_20 upper_20
#> 1: 2020-02-22 gp_rt     28  30.027 12.16743    13.00       21       25     32.0
#> 2: 2020-02-23 gp_rt     49  51.730 20.07706    24.00       37       45     54.0
#> 3: 2020-02-24 gp_rt     63  67.506 26.52160    31.00       49       57     70.0
#> 4: 2020-02-25 gp_rt     80  83.832 33.72490    38.00       61       72     87.4
#> 5: 2020-02-26 gp_rt     88  92.290 34.62347    44.95       67       79     97.0
#> 6: 2020-02-27 gp_rt    124 131.439 52.05080    62.00       95      114    137.0
#>    upper_50 upper_90
#> 1:    37.00    52.05
#> 2:    63.00    87.05
#> 3:    82.00   119.00
#> 4:   102.00   143.05
#> 5:   113.00   155.00
#> 6:   160.25   224.15
```

A summary table is returned for rapidly understanding the results and
for reporting purposes.

``` r
knitr::kable(estimates$summary[, -c("numeric_estimate")])
```

| measure                               | estimate             |
| :------------------------------------ | :------------------- |
| New confirmed cases by infection date | 2786 (624 – 10271)   |
| Expected change in daily cases        | Likely decreasing    |
| Effective reproduction no.            | 0.7 (0.3 – 1.3)      |
| Rate of growth                        | \-0.08 (-0.2 – 0.09) |
| Doubling/halving time (days)          | \-9.1 (8 – -3.4)     |

A range of plots are returned (with the single summary plot shown
below).

``` r
estimates$plots$summary
```

![](man/figures/unnamed-chunk-13-1.png)<!-- -->

### [regional\_epinow](https://epiforecasts.io/EpiNow2/reference/regional_epinow.html)

The `regional_epinow` function runs the `epinow` function across
multiple regions in an efficient manner.

Define cases in multiple regions delineated by the region variable.

``` r
reported_cases <- data.table::rbindlist(list(
   data.table::copy(reported_cases)[, region := "testland"],
   reported_cases[, region := "realland"]))
head(reported_cases)
#>          date confirm   region
#> 1: 2020-02-22      14 testland
#> 2: 2020-02-23      62 testland
#> 3: 2020-02-24      53 testland
#> 4: 2020-02-25      97 testland
#> 5: 2020-02-26      93 testland
#> 6: 2020-02-27      78 testland
```

Calling `regional_epinow` runs the `epinow` on each region in turn (or
in parallel depending on the settings used).

``` r
estimates <- regional_epinow(reported_cases = reported_cases, 
                             generation_time = generation_time,
                             delays = list(incubation_period, reporting_delay))
#> INFO [2020-10-15 18:25:36] Producing following optional outputs: regions, summary, samples, plots, latest
#> INFO [2020-10-15 18:25:36] Reporting estimates using data up to: 2020-04-01
#> INFO [2020-10-15 18:25:36] No target directory specified so returning output
#> INFO [2020-10-15 18:25:36] Producing estimates for: testland, realland
#> INFO [2020-10-15 18:25:36] Regions excluded: none
#> INFO [2020-10-15 18:25:36] Showing progress using progressr. Modify this behaviour using progressr::handlers.
#> INFO [2020-10-15 18:33:04] Completed estimates for: testland
#> INFO [2020-10-15 18:40:48] Completed estimates for: realland
#> INFO [2020-10-15 18:40:48] Completed regional estimates
#> INFO [2020-10-15 18:40:48] Regions with estimates: 2
#> INFO [2020-10-15 18:40:48] Regions with runtime errors: 0
#> INFO [2020-10-15 18:40:48] Producing summary
#> INFO [2020-10-15 18:40:48] No summary directory specified so returning summary output
#> INFO [2020-10-15 18:40:48] No target directory specified so returning timings
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
reporting (along with raw results for further processing).

``` r
knitr::kable(estimates$summary$summarised_results$table)
```

| Region   | New confirmed cases by infection date | Expected change in daily cases | Effective reproduction no. | Rate of growth        | Doubling/halving time (days) |
| :------- | :------------------------------------ | :----------------------------- | :------------------------- | :-------------------- | :--------------------------- |
| realland | 2915 (609 – 10156)                    | Likely decreasing              | 0.7 (0.3 – 1.3)            | \-0.07 (-0.21 – 0.08) | \-9.6 (8.7 – -3.3)           |
| testland | 2856 (664 – 9765)                     | Likely decreasing              | 0.8 (0.3 – 1.3)            | \-0.07 (-0.2 – 0.07)  | \-9.7 (9.6 – -3.4)           |

A range of plots are again returned (with the single summary plot shown
below).

``` r
estimates$summary$summary_plot
```

![](man/figures/unnamed-chunk-17-1.png)<!-- -->

### Reporting templates

Rmarkdown templates are provided in the package (`templates`) for
semi-automated reporting of estimates. These are currently undocumented
but an example integration can be seen
[here](https://github.com/epiforecasts/covid/blob/master/_posts/national/united-kingdom/united-kingdom.Rmd).
If using these templates to report your results please highlight our
[limitations](https://doi.org/10.12688/wellcomeopenres.16006.1) as these
are key to understanding the results from `{EpiNow2}` .

## Interactive figures

`{EpiNow2}` is integrated with the `{RtD3}` package which provides
interactive visualisations of Rt estimates. See the package
[documentation](https://epiforecasts.io/RtD3) for details.

## Contributing

File an issue [here](https://github.com/epiforecasts/EpiNow2/issues) if
you have identified an issue with the package. Please note that due to
operational constraints priority will be given to users informing
government policy or offering methodological insights. We welcome all
contributions, in particular those that improve the approach or the
robustness of the code base.
