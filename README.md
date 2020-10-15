
# EpiNow2: Estimate real-time case counts and time-varying epidemiological parameters

[![R-CMD-check](https://github.com/epiforecasts/EpiNow2/workflows/R-CMD-check/badge.svg)](https://github.com/epiforecasts/EpiNow2/actions)
[![Codecov test
coverage](https://codecov.io/gh/epiforecasts/EpiNow2/branch/master/graph/badge.svg)](https://codecov.io/gh/epiforecasts/EpiNow2?branch=master)
[![DOI](https://zenodo.org/badge/272995211.svg)](https://zenodo.org/badge/latestdoi/272995211)
[![metacran
downloads](http://cranlogs.r-pkg.org/badges/grand-total/EpiNow2?color=ff69b4)](https://cran.r-project.org/package=EpiNow2)

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
  - A fixed reproduction number is supported.
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
reporting_delay
#> $mean
#> [1] 1.251371
#> 
#> $mean_sd
#> [1] 0.1185135
#> 
#> $sd
#> [1] 1.014331
#> 
#> $sd_sd
#> [1] 0.1037697
#> 
#> $max
#> [1] 30
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
                    delays = list(incubation_period, reporting_delay))
#> WARN [2020-10-15 10:40:43] epinow: There were 3 divergent transitions after warmup. See
#> http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#> to find out why this is a problem and how to eliminate them. - 
#> WARN [2020-10-15 10:40:43] epinow: Examine the pairs() plot to diagnose sampling problems
#>  - 
#> WARN [2020-10-15 10:40:43] epinow: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
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
#> 1: infections infections    1 2020-02-11      1  1.908085  <NA> estimate
#> 2: infections infections    2 2020-02-12      1 13.782552  <NA> estimate
#> 3: infections infections    3 2020-02-13      1 18.445236  <NA> estimate
#> 4: infections infections    4 2020-02-14      1 38.429895  <NA> estimate
#> 5: infections infections    5 2020-02-15      1 57.777451  <NA> estimate
#> 6: infections infections    6 2020-02-16      1 57.766847  <NA> estimate
head(estimates$estimates$summarised)
#>          date variable strat     type   median     mean         sd lower_90
#> 1: 2020-02-22        R  <NA> estimate 1.819596 1.818002 0.20807045 1.469926
#> 2: 2020-02-23        R  <NA> estimate 1.828401 1.823357 0.16152609 1.556325
#> 3: 2020-02-24        R  <NA> estimate 1.831592 1.828929 0.12358350 1.628580
#> 4: 2020-02-25        R  <NA> estimate 1.833499 1.833810 0.09670256 1.678820
#> 5: 2020-02-26        R  <NA> estimate 1.834191 1.837076 0.08317796 1.701095
#> 6: 2020-02-27        R  <NA> estimate 1.837871 1.837834 0.08172036 1.702117
#>    lower_50 lower_20 upper_20 upper_50 upper_90
#> 1: 1.682912 1.771823 1.868168 1.947194 2.151804
#> 2: 1.714617 1.788518 1.864912 1.923085 2.092039
#> 3: 1.750022 1.799856 1.859175 1.907990 2.031455
#> 4: 1.769320 1.808499 1.856926 1.895937 1.996280
#> 5: 1.783717 1.816331 1.855630 1.888686 1.976484
#> 6: 1.788127 1.818421 1.854726 1.885785 1.972246
```

Reported cases are returned separately in order to ease reporting of
forecasts and model evaluation.

``` r
head(estimates$estimated_reported_cases$samples)
#>          date sample cases  type
#> 1: 2020-02-22      1    37 gp_rt
#> 2: 2020-02-23      1   207 gp_rt
#> 3: 2020-02-24      1    88 gp_rt
#> 4: 2020-02-25      1    98 gp_rt
#> 5: 2020-02-26      1   155 gp_rt
#> 6: 2020-02-27      1    89 gp_rt
head(estimates$estimated_reported_cases$summarised)
#>          date  type median    mean       sd lower_90 lower_50 lower_20 upper_20
#> 1: 2020-02-22 gp_rt     38  40.048 16.24764    17.00       28       34     43.0
#> 2: 2020-02-23 gp_rt     64  67.095 27.29159    30.00       47       57     71.4
#> 3: 2020-02-24 gp_rt     80  84.463 32.39525    37.95       62       74     89.0
#> 4: 2020-02-25 gp_rt     93  98.626 40.51379    43.95       71       83    102.0
#> 5: 2020-02-26 gp_rt    101 105.618 40.80126    49.00       76       92    111.0
#> 6: 2020-02-27 gp_rt    136 145.193 57.44045    69.00      106      124    150.0
#>    upper_50 upper_90
#> 1:    50.00    70.05
#> 2:    82.25   116.10
#> 3:   104.00   143.00
#> 4:   118.00   171.05
#> 5:   129.00   180.00
#> 6:   175.00   252.05
```

A summary table is returned for rapidly understanding the results and
for reporting purposes.

``` r
knitr::kable(estimates$summary[, -c("numeric_estimate")])
```

| measure                               | estimate              |
| :------------------------------------ | :-------------------- |
| New confirmed cases by infection date | 2952 (500 – 10895)    |
| Expected change in daily cases        | Unsure                |
| Effective reproduction no.            | 0.8 (0.3 – 1.4)       |
| Rate of growth                        | \-0.07 (-0.22 – 0.09) |
| Doubling/halving time (days)          | \-10.3 (7.5 – -3.2)   |

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
#> INFO [2020-10-15 10:40:47] Producing following optional outputs: regions, summary, samples, plots, latest
#> INFO [2020-10-15 10:40:47] Reporting estimates using data up to: 2020-04-01
#> INFO [2020-10-15 10:40:47] No target directory specified so returning output
#> INFO [2020-10-15 10:40:47] Producing estimates for: testland, realland
#> INFO [2020-10-15 10:40:47] Regions excluded: none
#> INFO [2020-10-15 10:40:47] Showing progress using progressr. Modify this behaviour using progressr::handlers.
#> INFO [2020-10-15 10:48:33] Completed estimates for: testland
#> INFO [2020-10-15 10:54:08] Completed estimates for: realland
#> INFO [2020-10-15 10:54:08] Completed regional estimates
#> INFO [2020-10-15 10:54:08] Regions with estimates: 2
#> INFO [2020-10-15 10:54:08] Regions with runtime errors: 0
#> INFO [2020-10-15 10:54:08] Producing summary
#> INFO [2020-10-15 10:54:08] No summary directory specified so returning summary output
#> INFO [2020-10-15 10:54:08] No target directory specified so returning timings
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
| realland | 2820 (551 – 11814)                    | Unsure                         | 0.7 (0.3 – 1.4)            | \-0.08 (-0.22 – 0.12) | \-9.2 (6 – -3.2)             |
| testland | 3216 (549 – 12859)                    | Unsure                         | 0.8 (0.3 – 1.5)            | \-0.07 (-0.21 – 0.13) | \-10.5 (5.4 – -3.3)          |

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
