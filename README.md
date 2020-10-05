
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
#> Warning: UNRELIABLE VALUE: Future ('future_lapply-1') unexpectedly generated
#> random numbers without specifying argument '[future.]seed'. There is a risk that
#> those random numbers are not statistically sound and the overall results might
#> be invalid. To fix this, specify argument '[future.]seed', e.g. 'seed=TRUE'.
#> This ensures that proper, parallel-safe random numbers are produced via the
#> L'Ecuyer-CMRG method. To disable this check, use [future].seed=NULL, or set
#> option 'future.rng.onMisuse' to "ignore".
#> Warning: UNRELIABLE VALUE: Future ('future_lapply-2') unexpectedly generated
#> random numbers without specifying argument '[future.]seed'. There is a risk that
#> those random numbers are not statistically sound and the overall results might
#> be invalid. To fix this, specify argument '[future.]seed', e.g. 'seed=TRUE'.
#> This ensures that proper, parallel-safe random numbers are produced via the
#> L'Ecuyer-CMRG method. To disable this check, use [future].seed=NULL, or set
#> option 'future.rng.onMisuse' to "ignore".
#> Warning: UNRELIABLE VALUE: Future ('future_lapply-3') unexpectedly generated
#> random numbers without specifying argument '[future.]seed'. There is a risk that
#> those random numbers are not statistically sound and the overall results might
#> be invalid. To fix this, specify argument '[future.]seed', e.g. 'seed=TRUE'.
#> This ensures that proper, parallel-safe random numbers are produced via the
#> L'Ecuyer-CMRG method. To disable this check, use [future].seed=NULL, or set
#> option 'future.rng.onMisuse' to "ignore".
#> Warning: UNRELIABLE VALUE: Future ('future_lapply-4') unexpectedly generated
#> random numbers without specifying argument '[future.]seed'. There is a risk that
#> those random numbers are not statistically sound and the overall results might
#> be invalid. To fix this, specify argument '[future.]seed', e.g. 'seed=TRUE'.
#> This ensures that proper, parallel-safe random numbers are produced via the
#> L'Ecuyer-CMRG method. To disable this check, use [future].seed=NULL, or set
#> option 'future.rng.onMisuse' to "ignore".
#> Warning: UNRELIABLE VALUE: Future ('future_lapply-5') unexpectedly generated
#> random numbers without specifying argument '[future.]seed'. There is a risk that
#> those random numbers are not statistically sound and the overall results might
#> be invalid. To fix this, specify argument '[future.]seed', e.g. 'seed=TRUE'.
#> This ensures that proper, parallel-safe random numbers are produced via the
#> L'Ecuyer-CMRG method. To disable this check, use [future].seed=NULL, or set
#> option 'future.rng.onMisuse' to "ignore".
#> Warning: UNRELIABLE VALUE: Future ('future_lapply-6') unexpectedly generated
#> random numbers without specifying argument '[future.]seed'. There is a risk that
#> those random numbers are not statistically sound and the overall results might
#> be invalid. To fix this, specify argument '[future.]seed', e.g. 'seed=TRUE'.
#> This ensures that proper, parallel-safe random numbers are produced via the
#> L'Ecuyer-CMRG method. To disable this check, use [future].seed=NULL, or set
#> option 'future.rng.onMisuse' to "ignore".
#> Warning: UNRELIABLE VALUE: Future ('future_lapply-7') unexpectedly generated
#> random numbers without specifying argument '[future.]seed'. There is a risk that
#> those random numbers are not statistically sound and the overall results might
#> be invalid. To fix this, specify argument '[future.]seed', e.g. 'seed=TRUE'.
#> This ensures that proper, parallel-safe random numbers are produced via the
#> L'Ecuyer-CMRG method. To disable this check, use [future].seed=NULL, or set
#> option 'future.rng.onMisuse' to "ignore".
#> Warning: UNRELIABLE VALUE: Future ('future_lapply-8') unexpectedly generated
#> random numbers without specifying argument '[future.]seed'. There is a risk that
#> those random numbers are not statistically sound and the overall results might
#> be invalid. To fix this, specify argument '[future.]seed', e.g. 'seed=TRUE'.
#> This ensures that proper, parallel-safe random numbers are produced via the
#> L'Ecuyer-CMRG method. To disable this check, use [future].seed=NULL, or set
#> option 'future.rng.onMisuse' to "ignore".
#> Warning: UNRELIABLE VALUE: Future ('future_lapply-9') unexpectedly generated
#> random numbers without specifying argument '[future.]seed'. There is a risk that
#> those random numbers are not statistically sound and the overall results might
#> be invalid. To fix this, specify argument '[future.]seed', e.g. 'seed=TRUE'.
#> This ensures that proper, parallel-safe random numbers are produced via the
#> L'Ecuyer-CMRG method. To disable this check, use [future].seed=NULL, or set
#> option 'future.rng.onMisuse' to "ignore".
#> Warning: UNRELIABLE VALUE: Future ('future_lapply-10') unexpectedly generated
#> random numbers without specifying argument '[future.]seed'. There is a risk that
#> those random numbers are not statistically sound and the overall results might
#> be invalid. To fix this, specify argument '[future.]seed', e.g. 'seed=TRUE'.
#> This ensures that proper, parallel-safe random numbers are produced via the
#> L'Ecuyer-CMRG method. To disable this check, use [future].seed=NULL, or set
#> option 'future.rng.onMisuse' to "ignore".
reporting_delay
#> $mean
#> [1] 1.207047
#> 
#> $mean_sd
#> [1] 0.1490608
#> 
#> $sd
#> [1] 1.092844
#> 
#> $sd_sd
#> [1] 0.1133359
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
#> 1: infections infections    1 2020-02-11      1  2.102824  <NA> estimate
#> 2: infections infections    2 2020-02-12      1 10.833834  <NA> estimate
#> 3: infections infections    3 2020-02-13      1 15.412873  <NA> estimate
#> 4: infections infections    4 2020-02-14      1 27.575111  <NA> estimate
#> 5: infections infections    5 2020-02-15      1 49.794883  <NA> estimate
#> 6: infections infections    6 2020-02-16      1 58.579067  <NA> estimate
head(estimates$estimates$summarised)
#>          date variable strat     type   bottom      top    lower    upper
#> 1: 2020-02-22        R  <NA> estimate 1.483201 2.086330 1.674702 1.910886
#> 2: 2020-02-23        R  <NA> estimate 1.529891 2.007782 1.687103 1.870045
#> 3: 2020-02-24        R  <NA> estimate 1.622208 1.991562 1.741372 1.890153
#> 4: 2020-02-25        R  <NA> estimate 1.663970 1.958616 1.728444 1.849149
#> 5: 2020-02-26        R  <NA> estimate 1.686296 1.940857 1.774474 1.882358
#> 6: 2020-02-27        R  <NA> estimate 1.704416 1.951436 1.768909 1.873016
#>    central_lower central_upper   median     mean         sd
#> 1:      1.779339      1.859663 1.790496 1.782737 0.18483995
#> 2:      1.800382      1.864121 1.795852 1.791083 0.14549205
#> 3:      1.769671      1.820709 1.799376 1.800271 0.11296815
#> 4:      1.791555      1.831742 1.809690 1.809384 0.09037439
#> 5:      1.813028      1.852204 1.817353 1.817405 0.08027247
#> 6:      1.812926      1.850551 1.824700 1.823275 0.08108804
```

Reported cases are returned separately in order to ease reporting of
forecasts and model evaluation.

``` r
head(estimates$estimated_reported_cases$samples)
#>          date sample cases  type
#> 1: 2020-02-22      1    27 gp_rt
#> 2: 2020-02-23      1   110 gp_rt
#> 3: 2020-02-24      1    19 gp_rt
#> 4: 2020-02-25      1   113 gp_rt
#> 5: 2020-02-26      1    78 gp_rt
#> 6: 2020-02-27      1   120 gp_rt
head(estimates$estimated_reported_cases$summarised)
#>          date  type bottom top lower upper central_lower central_upper median
#> 1: 2020-02-22 gp_rt     14  67    22    43            29            36     39
#> 2: 2020-02-23 gp_rt     25 110    40    75            52            64     67
#> 3: 2020-02-24 gp_rt     30 141    57    98            63            76     84
#> 4: 2020-02-25 gp_rt     38 164    71   123            79            97     97
#> 5: 2020-02-26 gp_rt     31 171    63   118            77            95    101
#> 6: 2020-02-27 gp_rt     57 247    97   168           118           142    142
#>       mean       sd
#> 1:  41.536 17.45151
#> 2:  70.264 28.23820
#> 3:  89.427 36.45331
#> 4: 101.552 40.28372
#> 5: 107.767 44.24478
#> 6: 152.241 61.15496
```

A summary table is returned for rapidly understanding the results and
for reporting purposes.

``` r
estimates$summary
#>                                  measure            estimate  numeric_estimate
#> 1: New confirmed cases by infection date  3815 (249 -- 9867) <data.table[1x7]>
#> 2:        Expected change in daily cases              Unsure               0.7
#> 3:            Effective reproduction no.    0.9 (0.3 -- 1.3) <data.table[1x7]>
#> 4:                        Rate of growth -0.04 (-0.2 -- 0.1) <data.table[1x7]>
#> 5:          Doubling/halving time (days) -16.8 (7.1 -- -3.4) <data.table[1x3]>
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
#> INFO [2020-10-05 14:40:58] Reporting estimates using data up to: 2020-04-01
#> INFO [2020-10-05 14:40:58] Producing estimates for: testland, realland
#> INFO [2020-10-05 14:40:58] Regions excluded: none
#> INFO [2020-10-05 14:51:28] Completed regional estimates
#> INFO [2020-10-05 14:51:28] Regions with estimates: 2
#> INFO [2020-10-05 14:51:28] Regions with runtime errors: 0
#> INFO [2020-10-05 14:51:28] Producing summary
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
#> 1: realland                   3839 (177 -- 10141)
#> 2: testland                   3880 (281 -- 10712)
#>    Expected change in daily cases Effective reproduction no.
#> 1:                         Unsure           0.8 (0.3 -- 1.3)
#> 2:                         Unsure           0.9 (0.4 -- 1.4)
#>          Rate of growth Doubling/halving time (days)
#> 1:  -0.04 (-0.2 -- 0.1)            -15.4 (7 -- -3.4)
#> 2: -0.04 (-0.2 -- 0.11)          -17.1 (6.5 -- -3.4)
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
