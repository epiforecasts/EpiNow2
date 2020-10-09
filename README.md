
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
  - A Fixed reproduction number is supported.
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

Install the stable version of the package:

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
statistical modeling platform used for the underlying model). For simple
deployment/development a prebuilt docker image is also available (see
documentation
[here](https://github.com/epiforecasts/EpiNow2/wiki/Docker)).

## Quick start

`{EpiNow2}` is designed to be used with a single function call or to be
used in an ad-hoc fashion via individual function calls. In the
following section we give an overview of the simple use case. For more
on using each function see the [function
documentation](https://epiforecasts.io/EpiNow2/reference/index.html).
The core functions are:
[`epinow`](https://epiforecasts.io/EpiNow2/reference/epinow.html),
[`regional_epinow`](https://epiforecasts.io/EpiNow2/reference/epinow.html),
[`estimate_infections`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.html),
and
[`forecast_infections`](https://epiforecasts.io/EpiNow2/reference/forecast_infections.html).
[`estimate_infections`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.html)
can be use on its own to infer the underlying infection case curve from
reported cases with Rt optionally returned (on by default). Estimating
the underlying infection case curve alone is substantially less
computationally demanding than also estimating Rt. The first step to
using the package is to load it as follows.

``` r
library(EpiNow2)
```

The next (optional) step is to define where logs should be stored (here
summary logs are written to the console and detailed logs are stored in
`info.logs`).

``` r
# Sets up a default info logger - this is the level of logging enabled by default
setup_logging("INFO")
#> Setting up logging for the EpiNow2 logger
#> Logging threshold set at: INFO
#> Writing logs to the console
# Sets up a logger for epinow linked logging messages which writes to info.log
# See ?setup_logging and the documentation of ?futile.logger for further details.
setup_logging("INFO", file = "info.log", name = "EpiNow2.epinow")
#> Setting up logging for the EpiNow2.epinow logger
#> Logging threshold set at: INFO
#> Writing logs to: info.log
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
#> [1] 1.350997
#> 
#> $mean_sd
#> [1] 0.159539
#> 
#> $sd
#> [1] 1.100183
#> 
#> $sd_sd
#> [1] 0.1105245
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
                    delays = list(incubation_period, reporting_delay),
                    stan_args = list(cores = 4))
names(estimates)
#> [1] "estimates"                "estimated_reported_cases"
#> [3] "summary"                  "plots"
```

Both summary measures and posterior samples are returned for all
parameters in an easily explored format.

``` r
head(estimates$estimates$samples)
#>      variable  parameter time       date sample     value strat     type
#> 1: infections infections    1 2020-02-10      1  1.927029  <NA> estimate
#> 2: infections infections    2 2020-02-11      1 12.423668  <NA> estimate
#> 3: infections infections    3 2020-02-12      1 19.186168  <NA> estimate
#> 4: infections infections    4 2020-02-13      1 24.401000  <NA> estimate
#> 5: infections infections    5 2020-02-14      1 44.337476  <NA> estimate
#> 6: infections infections    6 2020-02-15      1 55.686685  <NA> estimate
head(estimates$estimates$summarised)
#>          date variable strat     type   bottom      top    lower    upper
#> 1: 2020-02-22        R  <NA> estimate 1.307502 1.962194 1.530423 1.773185
#> 2: 2020-02-23        R  <NA> estimate 1.405941 1.928722 1.568768 1.768754
#> 3: 2020-02-24        R  <NA> estimate 1.475787 1.875317 1.619527 1.777184
#> 4: 2020-02-25        R  <NA> estimate 1.552148 1.858453 1.658480 1.785947
#> 5: 2020-02-26        R  <NA> estimate 1.592603 1.857001 1.673682 1.782719
#> 6: 2020-02-27        R  <NA> estimate 1.622465 1.885298 1.701551 1.803220
#>    central_lower central_upper   median     mean         sd
#> 1:      1.589120      1.672091 1.638275 1.644146 0.20190887
#> 2:      1.618665      1.691963 1.663261 1.663283 0.15915962
#> 3:      1.652050      1.709661 1.685909 1.684769 0.12275307
#> 4:      1.698117      1.741081 1.710975 1.707469 0.09560944
#> 5:      1.721932      1.758229 1.732138 1.730060 0.08141700
#> 6:      1.723187      1.758318 1.750165 1.751091 0.08082206
```

Reported cases are returned separately in order to ease reporting of
forecasts and model evaluation.

``` r
head(estimates$estimated_reported_cases$samples)
#>          date sample cases  type
#> 1: 2020-02-22      1    48 gp_rt
#> 2: 2020-02-23      1    63 gp_rt
#> 3: 2020-02-24      1    66 gp_rt
#> 4: 2020-02-25      1   154 gp_rt
#> 5: 2020-02-26      1   173 gp_rt
#> 6: 2020-02-27      1   198 gp_rt
head(estimates$estimated_reported_cases$summarised)
#>          date  type bottom top lower upper central_lower central_upper median
#> 1: 2020-02-22 gp_rt     14  84    27    54            44            53   49.0
#> 2: 2020-02-23 gp_rt     27 142    45    86            55            70   78.0
#> 3: 2020-02-24 gp_rt     38 174    61   110            74            91   97.0
#> 4: 2020-02-25 gp_rt     33 190    69   127            74            94  107.0
#> 5: 2020-02-26 gp_rt     35 197    70   134            84           106  115.0
#> 6: 2020-02-27 gp_rt     59 278   102   184           105           135  161.5
#>       mean       sd
#> 1:  52.477 24.88443
#> 2:  82.847 35.81293
#> 3: 102.704 42.63029
#> 4: 116.655 54.59431
#> 5: 124.161 52.88046
#> 6: 170.703 72.65797
```

A summary table is returned for rapidly understanding the results and
for reporting purposes.

``` r
knitr::kable(estimates$summary[-ncol(estimates$summary)])
```

| measure                               | estimate             | numeric\_estimate                              |
| :------------------------------------ | :------------------- | :--------------------------------------------- |
| New confirmed cases by infection date | 3740 (241 – 11052)   | 3740, 241, 11052, 615, 3940, 1835, 2993        |
| Expected change in daily cases        | Unsure               | 0.69                                           |
| Rate of growth                        | \-0.05 (-0.2 – 0.11) | \-0.05, -0.20, 0.11, -0.11, 0.00, -0.09, -0.05 |
| Doubling/halving time (days)          | \-14.8 (6.3 – -3.5)  | \-14.8, 6.3, -3.5                              |

A range of plots are returned (with the single summary plot shown
below).

``` r
estimates$plots$summary
```

![](man/figures/unnamed-chunk-14-1.png)<!-- -->

### [Regional epinow](https://epiforecasts.io/EpiNow2/reference/regional_epinow.html)

This function runs the the `epinow` function across multiple regions in
an efficient manner.

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

Run the pipeline on each region in turn.

``` r
estimates <- regional_epinow(reported_cases = reported_cases, 
                             generation_time = generation_time,
                             delays = list(incubation_period, reporting_delay),
                             stan_args = list(cores = 4))
#> INFO [2020-10-08 23:02:21] Producing following outputs: regions, summary, samples, plots, timings
#> INFO [2020-10-08 23:02:21] Reporting estimates using data up to: 2020-04-01
#> INFO [2020-10-08 23:02:21] Producing estimates for: testland, realland
#> INFO [2020-10-08 23:02:21] Regions excluded: none
#> INFO [2020-10-08 23:02:21] Showing progress using progressr. Modify this behaviour using progressr::handlers.
#> INFO [2020-10-08 23:07:45] Completed estimates for: testland
#> INFO [2020-10-08 23:13:20] Completed estimates for: realland
#> INFO [2020-10-08 23:13:20] Completed regional estimates
#> INFO [2020-10-08 23:13:20] Regions with estimates: 2
#> INFO [2020-10-08 23:13:20] Regions with runtime errors: 0
#> INFO [2020-10-08 23:13:20] Producing summary
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
| realland | 3770 (101 – 10695)                    | Unsure                         | 0.8 (0.3 – 1.4)            | \-0.04 (-0.21 – 0.11) | \-15.7 (6.2 – -3.3)          |
| testland | 3606 (63 – 10457)                     | Unsure                         | 0.8 (0.3 – 1.4)            | \-0.05 (-0.21 – 0.12) | \-14.6 (5.9 – -3.3)          |

A range of plots are again returned (with the single summary plot shown
below).

``` r
estimates$summary$summary_plot
```

![](man/figures/unnamed-chunk-18-1.png)<!-- -->

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
