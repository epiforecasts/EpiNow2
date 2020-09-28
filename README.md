
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

### Reporting delays, incubation period and generation time

Distributions can either be fitted using package functionality or
determined elsewhere and then defined with uncertainty for use in
`{EpiNow2}`. When data is supplied a subsampled bootstrapped lognormal
will be fit (to account for uncertainty in the observed data without
being biased by changes in incidence). An arbitrary number of delay
distributions are supported with the most common use case likely to be a
incubation period followed by a reporting delay.

``` r
reporting_delay <- bootstrapped_dist_fit(rlnorm(100, log(6), 1), max_value = 30)
reporting_delay
#> $mean
#> [1] 1.868661
#> 
#> $mean_sd
#> [1] 0.1367271
#> 
#> $sd
#> [1] 1.028525
#> 
#> $sd_sd
#> [1] 0.1168298
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
up may be needed*.

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
#> 1: infections infections    1 2020-02-06      1  1.941401  <NA> estimate
#> 2: infections infections    2 2020-02-07      1 10.484886  <NA> estimate
#> 3: infections infections    3 2020-02-08      1 21.686929  <NA> estimate
#> 4: infections infections    4 2020-02-09      1 31.920355  <NA> estimate
#> 5: infections infections    5 2020-02-10      1 49.524574  <NA> estimate
#> 6: infections infections    6 2020-02-11      1 50.905044  <NA> estimate
head(estimates$estimates$summarised)
#>          date variable strat     type    bottom      top    lower    upper
#> 1: 2020-02-22        R  <NA> estimate 0.8963336 1.681383 1.096868 1.443589
#> 2: 2020-02-23        R  <NA> estimate 0.9741795 1.645305 1.197950 1.487339
#> 3: 2020-02-24        R  <NA> estimate 1.0769909 1.634186 1.261085 1.488270
#> 4: 2020-02-25        R  <NA> estimate 1.1988843 1.641482 1.329062 1.510339
#> 5: 2020-02-26        R  <NA> estimate 1.2871408 1.642579 1.407255 1.547357
#> 6: 2020-02-27        R  <NA> estimate 1.3355205 1.654648 1.458141 1.581259
#>    central_lower central_upper   median     mean         sd
#> 1:      1.158482      1.287437 1.267170 1.266214 0.25134222
#> 2:      1.189465      1.298782 1.308636 1.306737 0.21231498
#> 3:      1.344476      1.429435 1.360103 1.353368 0.17399453
#> 4:      1.387850      1.452402 1.407981 1.404904 0.13865812
#> 5:      1.424001      1.475132 1.459508 1.459667 0.11120438
#> 6:      1.491214      1.535828 1.516591 1.515502 0.09929097
```

Reported cases are returned separately in order to ease reporting of
forecasts and model evaluation.

``` r
head(estimates$estimated_reported_cases$samples)
#>          date sample cases  type
#> 1: 2020-02-22      1    66 gp_rt
#> 2: 2020-02-23      1   144 gp_rt
#> 3: 2020-02-24      1   125 gp_rt
#> 4: 2020-02-25      1    87 gp_rt
#> 5: 2020-02-26      1   176 gp_rt
#> 6: 2020-02-27      1    79 gp_rt
head(estimates$estimated_reported_cases$summarised)
#>          date  type bottom top lower upper central_lower central_upper median
#> 1: 2020-02-22 gp_rt     15 185    41   106            74            98     91
#> 2: 2020-02-23 gp_rt     29 286    67   162            70           103    136
#> 3: 2020-02-24 gp_rt     37 333    71   184           111           148    157
#> 4: 2020-02-25 gp_rt     31 376    83   202           124           164    173
#> 5: 2020-02-26 gp_rt     34 367    82   206           122           167    175
#> 6: 2020-02-27 gp_rt     67 545   114   289           145           210    255
#>       mean        sd
#> 1: 103.696  64.01990
#> 2: 154.983  91.62024
#> 3: 181.663 104.65874
#> 4: 200.934 124.95775
#> 5: 201.815 124.78453
#> 6: 288.010 170.88681
```

A summary table is returned for rapidly understanding the results and
for reporting purposes.

``` r
estimates$summary
#>                                  measure              estimate
#> 1: New confirmed cases by infection date    3971 (14 -- 20455)
#> 2:        Expected change in daily cases                Unsure
#> 3:            Effective reproduction no.   0.87 (0.08 -- 1.53)
#> 4:                        Rate of growth -0.04 (-0.26 -- 0.18)
#> 5:          Doubling/halving time (days)   -19.4 (3.8 -- -2.7)
#>     numeric_estimate
#> 1: <data.table[1x7]>
#> 2:              0.61
#> 3: <data.table[1x7]>
#> 4: <data.table[1x7]>
#> 5: <data.table[1x3]>
```

A range of plots are returned (with the single summary plot shown
below).

``` r
estimates$plots$summary
```

![](man/figures/unnamed-chunk-13-1.png)<!-- -->

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
#> INFO [2020-09-28 14:21:46] Reporting estimates using data up to: 2020-04-01
#> INFO [2020-09-28 14:21:46] Producing estimates for: testland, realland
#> INFO [2020-09-28 14:21:46] Initialising estimates for: testland
#> WARN [2020-09-28 14:25:33] testland: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> http://mc-stan.org/misc/warnings.html#bulk-ess - 
#> WARN [2020-09-28 14:25:34] testland: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> http://mc-stan.org/misc/warnings.html#tail-ess - 
#> INFO [2020-09-28 14:25:35] Completed estimates for: testland
#> INFO [2020-09-28 14:25:35] Initialising estimates for: realland
#> WARN [2020-09-28 14:29:11] realland: There were 1 divergent transitions after warmup. See
#> http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#> to find out why this is a problem and how to eliminate them. - 
#> WARN [2020-09-28 14:29:11] realland: Examine the pairs() plot to diagnose sampling problems
#>  - 
#> WARN [2020-09-28 14:29:11] realland: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> http://mc-stan.org/misc/warnings.html#bulk-ess - 
#> WARN [2020-09-28 14:29:12] realland: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> http://mc-stan.org/misc/warnings.html#tail-ess - 
#> INFO [2020-09-28 14:29:13] Completed estimates for: realland
#> INFO [2020-09-28 14:29:13] Producing summary
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
#> 1: realland                    4399 (10 -- 24109)
#> 2: testland                     4124 (6 -- 22140)
#>    Expected change in daily cases Effective reproduction no.
#> 1:                         Unsure         0.89 (0.1 -- 1.64)
#> 2:                         Unsure        0.88 (0.13 -- 1.51)
#>           Rate of growth Doubling/halving time (days)
#> 1: -0.03 (-0.25 -- 0.21)          -22.2 (3.2 -- -2.8)
#> 2: -0.03 (-0.26 -- 0.16)          -21.2 (4.4 -- -2.7)
```

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
