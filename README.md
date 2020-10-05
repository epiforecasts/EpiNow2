
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
#> [1] 1.121562
#> 
#> $mean_sd
#> [1] 0.1641465
#> 
#> $sd
#> [1] 1.212076
#> 
#> $sd_sd
#> [1] 0.1357686
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
#> 1: infections infections    1 2020-02-11      1  1.713216  <NA> estimate
#> 2: infections infections    2 2020-02-12      1 10.361868  <NA> estimate
#> 3: infections infections    3 2020-02-13      1 18.112208  <NA> estimate
#> 4: infections infections    4 2020-02-14      1 28.833484  <NA> estimate
#> 5: infections infections    5 2020-02-15      1 43.713818  <NA> estimate
#> 6: infections infections    6 2020-02-16      1 51.122634  <NA> estimate
head(estimates$estimates$summarised)
#>          date variable strat     type   bottom      top    lower    upper
#> 1: 2020-02-22        R  <NA> estimate 1.460840 2.071162 1.614854 1.864393
#> 2: 2020-02-23        R  <NA> estimate 1.523846 2.005474 1.615059 1.818843
#> 3: 2020-02-24        R  <NA> estimate 1.566266 1.944036 1.663716 1.823922
#> 4: 2020-02-25        R  <NA> estimate 1.641185 1.941345 1.739435 1.866124
#> 5: 2020-02-26        R  <NA> estimate 1.657554 1.926520 1.759162 1.865628
#> 6: 2020-02-27        R  <NA> estimate 1.674716 1.936040 1.752127 1.853318
#>    central_lower central_upper   median     mean         sd
#> 1:      1.741297      1.835204 1.742897 1.746969 0.19668356
#> 2:      1.670770      1.746335 1.753774 1.758797 0.15415863
#> 3:      1.730337      1.786061 1.767448 1.771843 0.11923808
#> 4:      1.744617      1.786020 1.780971 1.785113 0.09431922
#> 5:      1.767667      1.802731 1.795138 1.797524 0.08173062
#> 6:      1.768931      1.805214 1.803765 1.807954 0.08062760
```

Reported cases are returned separately in order to ease reporting of
forecasts and model evaluation.

``` r
head(estimates$estimated_reported_cases$samples)
#>          date sample cases  type
#> 1: 2020-02-22      1    43 gp_rt
#> 2: 2020-02-23      1    57 gp_rt
#> 3: 2020-02-24      1   142 gp_rt
#> 4: 2020-02-25      1   200 gp_rt
#> 5: 2020-02-26      1    97 gp_rt
#> 6: 2020-02-27      1   119 gp_rt
head(estimates$estimated_reported_cases$summarised)
#>          date  type bottom top lower upper central_lower central_upper median
#> 1: 2020-02-22 gp_rt     17  76    25    47            35            42     42
#> 2: 2020-02-23 gp_rt     26 116    46    80            62            74     70
#> 3: 2020-02-24 gp_rt     29 139    60   103            78            93     87
#> 4: 2020-02-25 gp_rt     38 167    65   114            88           106    102
#> 5: 2020-02-26 gp_rt     33 175    66   122            66            88    109
#> 6: 2020-02-27 gp_rt     62 259    94   170           121           148    147
#>       mean       sd
#> 1:  45.414 19.91560
#> 2:  73.724 28.75581
#> 3:  91.419 37.51670
#> 4: 106.639 41.72730
#> 5: 113.897 46.27522
#> 6: 156.919 64.67302
```

A summary table is returned for rapidly understanding the results and
for reporting purposes.

``` r
estimates$summary
#>                                  measure             estimate  numeric_estimate
#> 1: New confirmed cases by infection date  3773 (164 -- 10052) <data.table[1x7]>
#> 2:        Expected change in daily cases               Unsure              0.71
#> 3:            Effective reproduction no.     0.8 (0.4 -- 1.4) <data.table[1x7]>
#> 4:                        Rate of growth -0.05 (-0.19 -- 0.1) <data.table[1x7]>
#> 5:          Doubling/halving time (days)  -14.9 (6.8 -- -3.6) <data.table[1x3]>
```

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
#> INFO [2020-10-05 16:24:40] Reporting estimates using data up to: 2020-04-01
#> INFO [2020-10-05 16:24:40] Producing estimates for: testland, realland
#> INFO [2020-10-05 16:24:40] Regions excluded: none
#> INFO [2020-10-05 16:33:29] Completed regional estimates
#> INFO [2020-10-05 16:33:29] Regions with estimates: 2
#> INFO [2020-10-05 16:33:29] Regions with runtime errors: 0
#> INFO [2020-10-05 16:33:29] Producing summary
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
estimates$summary$summarised_results$table
#>      Region New confirmed cases by infection date
#> 1: realland                   3816 (327 -- 10309)
#> 2: testland                    3854 (175 -- 9971)
#>    Expected change in daily cases Effective reproduction no.
#> 1:                         Unsure           0.8 (0.4 -- 1.4)
#> 2:                         Unsure           0.8 (0.4 -- 1.3)
#>           Rate of growth Doubling/halving time (days)
#> 1:  -0.04 (-0.19 -- 0.1)          -15.6 (6.6 -- -3.7)
#> 2: -0.04 (-0.19 -- 0.09)          -15.6 (7.9 -- -3.6)
```

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
