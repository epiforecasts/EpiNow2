
# EpiNow2: Estimate real-time case counts and time-varying epidemiological parameters

[![R-CMD-check](https://github.com/epiforecasts/EpiNow2/workflows/R-CMD-check/badge.svg)](https://github.com/epiforecasts/EpiNow2/actions)
[![Codecov test
coverage](https://codecov.io/gh/epiforecasts/EpiNow2/branch/master/graph/badge.svg)](https://codecov.io/gh/epiforecasts/EpiNow2?branch=master)
[![DOI](https://zenodo.org/badge/272995211.svg)](https://zenodo.org/badge/latestdoi/272995211)

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
[`{EpiEstim}`](https://github.com/annecori/EpiEstim)). Imputed
infections are then mapped to observed data (for example cases by date
of report) via a series of uncertain delay distributions (in the
examples in the package documentation these are an incubation period and
a reporting delay) and a reporting model that can include weekly
periodicity.

Uncertainty is propagated from all inputs into the final parameter
estimates, helping to mitigate spurious findings. This is handled
internally. The time-varying reproduction estimates and the uncertain
generation time also give time-varying estimates of the rate of growth.

The default model uses a non-stationary Gaussian process to estimate the
time-varying reproduction number. Other options include:

  - A stationary Gaussian process (faster to estimate but currently
    gives reduced performance for real time estimates).
  - User specified breakpoints.
  - As a constant.
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
[`{EpiSoon}`](https://epiforecasts.io/EpiSoon) package, and converted to
a case forecast using the renewal equation. Alternatively, the
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
computationally demanding than also estimating Rt.

### Reporting delays, incubation period and generation time

Distributions can either be fitted using package functionality or
determined elsewhere and then defined with uncertainty for use in
`{EpiNow2}`. When data is supplied a subsampled bootstrapped lognormal
will be fit (to account for uncertainty in the observed data without
being biased by changes in incidence). An arbitrary number of delay
distributions are supported with the most common use case likely to be a
incubation period followed by a reporting delay.

``` r
reporting_delay <- EpiNow2::bootstrapped_dist_fit(rlnorm(100, log(6), 1))
## Set max allowed delay to 30 days to truncate computation
reporting_delay$max <- 30

reporting_delay
#> $mean
#> [1] 1.970138
#> 
#> $mean_sd
#> [1] 0.1681175
#> 
#> $sd
#> [1] 1.025332
#> 
#> $sd_sd
#> [1] 0.1058058
#> 
#> $max
#> [1] 30
```

Here we define the incubation period and generation time based on
literature estimates for Covid-19 (see
[here](https://github.com/epiforecasts/EpiNow/tree/master/data-raw) for
the code that generates these estimates).

``` r
generation_time <- list(mean = EpiNow2::covid_generation_times[1, ]$mean,
                        mean_sd = EpiNow2::covid_generation_times[1, ]$mean_sd,
                         sd = EpiNow2::covid_generation_times[1, ]$sd,
                         sd_sd = EpiNow2::covid_generation_times[1, ]$sd_sd,
                         max = 30)

incubation_period <- list(mean = EpiNow2::covid_incubation_period[1, ]$mean,
                          mean_sd = EpiNow2::covid_incubation_period[1, ]$mean_sd,
                          sd = EpiNow2::covid_incubation_period[1, ]$sd,
                          sd_sd = EpiNow2::covid_incubation_period[1, ]$sd_sd,
                          max = 30)
```

### [epinow](https://epiforecasts.io/EpiNow2/reference/epinow.html)

This function represents the core functionality of the package and
includes results reporting, plotting and optional saving. It requires a
data frame of cases by date of report and the distributions defined
above. An additional forecasting module is supported via `EpiSoon` and
companion packages (see documentation for an example).

Load example case data from `{EpiNow2}`.

``` r
reported_cases <- EpiNow2::example_confirmed[1:50]

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
estimates <- EpiNow2::epinow(reported_cases = reported_cases, 
                             generation_time = generation_time,
                             delays = list(incubation_period, reporting_delay),
                             horizon = 7, samples = 1000, warmup = 200, 
                             cores = 4, chains = 4, verbose = TRUE, 
                             adapt_delta = 0.95)

names(estimates)
#> [1] "estimates"                "estimated_reported_cases"
#> [3] "summary"                  "plots"
```

Both summary measures and posterior samples are returned for all
parameters in an easily explored format.

``` r
estimates$estimates
#> $samples
#>                 variable        parameter time       date sample       value
#>      1:       infections       infections    1 2020-02-05      1    1.867327
#>      2:       infections       infections    2 2020-02-06      1   10.415739
#>      3:       infections       infections    3 2020-02-07      1   16.015825
#>      4:       infections       infections    4 2020-02-08      1   34.386094
#>      5:       infections       infections    5 2020-02-09      1   48.276454
#>     ---                                                                     
#> 258070: prior_infections prior_infections   70 2020-04-14      1 2455.552770
#> 258071: prior_infections prior_infections   71 2020-04-15      1 2394.825740
#> 258072: prior_infections prior_infections   72 2020-04-16      1 2335.600519
#> 258073: prior_infections prior_infections   73 2020-04-17      1 2277.839967
#> 258074: prior_infections prior_infections   74 2020-04-18      1 2221.507862
#>         strat     type
#>      1:  <NA> estimate
#>      2:  <NA> estimate
#>      3:  <NA> estimate
#>      4:  <NA> estimate
#>      5:  <NA> estimate
#>     ---               
#> 258070:  <NA> forecast
#> 258071:  <NA> forecast
#> 258072:  <NA> forecast
#> 258073:  <NA> forecast
#> 258074:  <NA> forecast
#> 
#> $summarised
#>            date       variable strat     type      bottom         top
#>   1: 2020-02-22              R  <NA> estimate   0.5684197    1.326612
#>   2: 2020-02-23              R  <NA> estimate   0.6689788    1.327115
#>   3: 2020-02-24              R  <NA> estimate   0.7843623    1.335338
#>   4: 2020-02-25              R  <NA> estimate   0.9199542    1.374425
#>   5: 2020-02-26              R  <NA> estimate   1.0311702    1.403387
#>  ---                                                                 
#> 328: 2020-04-14 reported_cases  <NA> forecast 271.0000000 6530.000000
#> 329: 2020-04-15 reported_cases  <NA> forecast 250.0000000 5754.000000
#> 330: 2020-04-16 reported_cases  <NA> forecast 194.0000000 7529.000000
#> 331: 2020-04-17 reported_cases  <NA> forecast 175.0000000 9305.000000
#> 332: 2020-04-18 reported_cases  <NA> forecast 185.0000000 7511.000000
#>            lower       upper       median         mean           sd
#>   1:   0.7517412    1.047230    0.9197803    0.9378134 2.289810e-01
#>   2:   0.8574849    1.111401    0.9812533    0.9931448 2.000579e-01
#>   3:   0.9151091    1.137584    1.0556009    1.0586086 1.707855e-01
#>   4:   1.0358071    1.223475    1.1305832    1.1335666 1.420613e-01
#>   5:   1.1415192    1.296867    1.2133702    1.2167753 1.165027e-01
#>  ---                                                               
#> 328: 812.0000000 2861.000000 2619.0000000 3408.0960000 3.077887e+03
#> 329: 590.0000000 2305.000000 2080.5000000 3085.2360000 4.127134e+03
#> 330: 787.0000000 2813.000000 2438.0000000 3891.4520000 5.831976e+03
#> 331: 711.0000000 3160.000000 2891.5000000 5133.8350000 9.891106e+03
#> 332: 539.0000000 2367.000000 2188.0000000 5545.4320000 2.827589e+04
```

Reported cases are returned separately in order to ease reporting of
forecasts and model evaluation.

``` r
estimates$estimated_reported_cases
#> $samples
#>              date sample cases  type
#>     1: 2020-02-22      1   203 gp_rt
#>     2: 2020-02-23      1   341 gp_rt
#>     3: 2020-02-24      1    53 gp_rt
#>     4: 2020-02-25      1   110 gp_rt
#>     5: 2020-02-26      1    47 gp_rt
#>    ---                              
#> 56996: 2020-04-14   1000  3775 gp_rt
#> 56997: 2020-04-15   1000  4102 gp_rt
#> 56998: 2020-04-16   1000  5934 gp_rt
#> 56999: 2020-04-17   1000 11467 gp_rt
#> 57000: 2020-04-18   1000 11121 gp_rt
#> 
#> $summarised
#>           date  type bottom   top lower upper median     mean         sd
#>  1: 2020-02-22 gp_rt     35   330    57   175  159.0  178.002   103.4936
#>  2: 2020-02-23 gp_rt     30   430    69   227  211.0  240.528   150.2495
#>  3: 2020-02-24 gp_rt     38   493   121   292  242.0  276.120   161.1148
#>  4: 2020-02-25 gp_rt     44   508    91   264  237.5  274.978   163.7934
#>  5: 2020-02-26 gp_rt     48   478   105   264  230.5  266.161   162.8148
#>  6: 2020-02-27 gp_rt     70   634   153   366  298.0  342.531   202.6564
#>  7: 2020-02-28 gp_rt     82   879   184   471  388.5  458.013   275.4754
#>  8: 2020-02-29 gp_rt     55   706   149   410  350.5  394.607   248.7836
#>  9: 2020-03-01 gp_rt     64   877   143   464  418.5  486.548   287.7215
#> 10: 2020-03-02 gp_rt     94   965   230   537  441.0  508.601   321.3313
#> 11: 2020-03-03 gp_rt    109   979   195   513  440.5  513.409   319.7521
#> 12: 2020-03-04 gp_rt     50   922   175   482  414.5  494.278   335.9354
#> 13: 2020-03-05 gp_rt     90  1209   280   706  593.0  671.070   406.4826
#> 14: 2020-03-06 gp_rt    130  1690   403   978  782.5  899.535   555.0464
#> 15: 2020-03-07 gp_rt    118  1627   297   850  748.0  869.909   597.1382
#> 16: 2020-03-08 gp_rt    172  2150   420  1107  946.5 1136.650   725.3866
#> 17: 2020-03-09 gp_rt    216  2444   436  1301 1163.0 1334.771   819.6695
#> 18: 2020-03-10 gp_rt    170  2552   660  1511 1230.0 1408.187   856.6881
#> 19: 2020-03-11 gp_rt    260  2525   600  1456 1223.5 1398.798   868.1106
#> 20: 2020-03-12 gp_rt    190  3658   648  1949 1772.5 2030.691  1291.7425
#> 21: 2020-03-13 gp_rt    381  5188  1055  2824 2562.5 2852.796  1756.5564
#> 22: 2020-03-14 gp_rt    452  4969   957  2664 2279.0 2674.353  1687.2145
#> 23: 2020-03-15 gp_rt    500  6199  1171  3417 2980.5 3398.346  2109.9632
#> 24: 2020-03-16 gp_rt    689  7241  1586  4025 3417.5 3877.684  2368.5666
#> 25: 2020-03-17 gp_rt    477  6890  1622  3995 3330.0 3815.714  2380.7079
#> 26: 2020-03-18 gp_rt    516  6524  1775  3859 3147.5 3516.918  2162.5979
#> 27: 2020-03-19 gp_rt    668  8604  1734  4661 4011.0 4590.306  2838.5552
#> 28: 2020-03-20 gp_rt   1007 10713  2114  5702 5161.5 6012.918  3686.1710
#> 29: 2020-03-21 gp_rt    804  9585  1875  5014 4453.5 5174.663  3230.3387
#> 30: 2020-03-22 gp_rt   1060 11609  3181  7203 5339.5 6226.785  3804.4701
#> 31: 2020-03-23 gp_rt    928 11046  2548  6536 5295.0 6015.854  3787.9038
#> 32: 2020-03-24 gp_rt    714  9577  2208  5678 5035.5 5623.255  3131.5741
#> 33: 2020-03-25 gp_rt    804  9030  2327  5436 4453.5 4933.506  2894.2815
#> 34: 2020-03-26 gp_rt    897 10785  2496  6139 5192.0 5960.224  3556.3619
#> 35: 2020-03-27 gp_rt   1155 13188  3181  7527 6185.0 7120.340  4378.3017
#> 36: 2020-03-28 gp_rt    578  9950  2453  6294 5241.0 5726.994  3362.1852
#> 37: 2020-03-29 gp_rt    906 11538  2671  6388 5453.0 6247.617  3685.9409
#> 38: 2020-03-30 gp_rt    710 11576  2224  6272 5442.5 6238.940  3851.1992
#> 39: 2020-03-31 gp_rt    704  9942  2182  5711 4675.5 5458.867  3409.6736
#> 40: 2020-04-01 gp_rt    807  7992  1659  4347 3725.0 4271.485  2573.2166
#> 41: 2020-04-02 gp_rt    641  9259  2058  5431 4457.5 5100.306  3164.3220
#> 42: 2020-04-03 gp_rt    881 10902  2355  6344 5364.0 6018.117  3626.1481
#> 43: 2020-04-04 gp_rt    963  8466  2021  4932 4198.5 4744.331  2913.0317
#> 44: 2020-04-05 gp_rt    853  9021  2193  5106 4314.0 4995.766  3008.5090
#> 45: 2020-04-06 gp_rt    617  9093  1857  5242 4296.0 4924.858  3119.8174
#> 46: 2020-04-07 gp_rt    511  7265  1847  4312 3525.5 4102.753  2592.8138
#> 47: 2020-04-08 gp_rt    498  6118  1257  3397 2892.5 3340.726  2164.0422
#> 48: 2020-04-09 gp_rt    524  7257  1761  4320 3478.5 3989.107  2569.6792
#> 49: 2020-04-10 gp_rt    515  8716  1879  4772 3995.0 4709.114  3203.5281
#> 50: 2020-04-11 gp_rt    514  7192  1118  3446 3130.5 3684.906  2454.1989
#> 51: 2020-04-12 gp_rt    406  7921   999  3589 3221.0 3994.946  2896.3762
#> 52: 2020-04-13 gp_rt    326  7236   980  3419 3099.0 3783.081  2755.6814
#> 53: 2020-04-14 gp_rt    271  6530   812  2861 2619.0 3408.096  3077.8866
#> 54: 2020-04-15 gp_rt    250  5754   590  2305 2080.5 3085.236  4127.1344
#> 55: 2020-04-16 gp_rt    194  7529   787  2813 2438.0 3891.452  5831.9764
#> 56: 2020-04-17 gp_rt    175  9305   711  3160 2891.5 5133.835  9891.1058
#> 57: 2020-04-18 gp_rt    185  7511   539  2367 2188.0 5545.432 28275.8934
#>           date  type bottom   top lower upper median     mean         sd
```

A summary table is returned for rapidly understanding the results and
for reporting purposes.

``` r
estimates$summary
#>                                  measure              estimate
#> 1: New confirmed cases by infection date      1448 (8 -- 8726)
#> 2:        Expected change in daily cases                Unsure
#> 3:            Effective reproduction no.      0.7 (0.1 -- 1.5)
#> 4:                        Rate of growth -0.08 (-0.28 -- 0.15)
#> 5:          Doubling/halving time (days)    -8.8 (4.6 -- -2.5)
#>     numeric_estimate
#> 1: <data.table[1x5]>
#> 2:              0.73
#> 3: <data.table[1x5]>
#> 4: <data.table[1x5]>
#> 5: <data.table[1x3]>
```

A range of plots are returned (with the single summary plot shown
below).

``` r
estimates$plots$summary
```

![](man/figures/unnamed-chunk-12-1.png)<!-- -->

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

Run the pipeline on each region in turn. The commented code (requires
the `{future}` package) can be used to run regions in parallel (when in
most scenarios `cores` should be set to 1).

``` r
## future::plan("multisession")
estimates <- EpiNow2::regional_epinow(reported_cases = reported_cases, 
                                      generation_time = generation_time,
                                      delays = list(incubation_period, reporting_delay),
                                      horizon = 7, samples = 1000, warmup = 200,
                                      cores = 4, chains = 4, adapt_delta = 0.95,
                                      verbose = TRUE)
```

Results from each region are stored in a `regional` list with across
region summary measures and plots stored in a `summary` list. All
results can be set to be internally saved by setting the `target_folder`
and `summary_dir` arguments.

Summary measures that are returned include a table formatted for
reporting (along with raw results for further processing).

``` r
estimates$summary$summarised_results$table
#>      Region New confirmed cases by infection date
#> 1: realland                      1475 (1 -- 8980)
#> 2: testland                      1690 (2 -- 8781)
#>    Expected change in daily cases Effective reproduction no.
#> 1:                         Unsure           0.7 (0.1 -- 1.5)
#> 2:                         Unsure           0.8 (0.1 -- 1.5)
#>           Rate of growth Doubling/halving time (days)
#> 1: -0.08 (-0.29 -- 0.15)           -8.5 (4.5 -- -2.4)
#> 2: -0.07 (-0.32 -- 0.13)           -9.8 (5.4 -- -2.1)
```

A range of plots are again returned (with the single summary plot shown
below).

``` r
estimates$summary$summary_plot
```

![](man/figures/unnamed-chunk-16-1.png)<!-- -->

### Reporting templates

Rmarkdown templates are provided in the package (`templates`) for
semi-automated reporting of estimates. These are currently undocumented
but an example integration can be seen
[here](https://github.com/epiforecasts/covid/blob/master/_posts/national/united-kingdom/united-kingdom.Rmd).
If using these templates to report your results please highlight our
[limitations](https://doi.org/10.12688/wellcomeopenres.16006.1) as these
are key to understanding the results from `{EpiNow2}` .

## Contributing

File an issue [here](https://github.com/epiforecasts/EpiNow2/issues) if
you have identified an issue with the package. Please note that due to
operational constraints priority will be given to users informing
government policy or offering methodological insights. We welcome all
contributions, in particular those that improve the approach or the
robustness of the code base.
