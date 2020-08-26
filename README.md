
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
#> [1] 1.680835
#> 
#> $mean_sd
#> [1] 0.1177547
#> 
#> $sd
#> [1] 1.036805
#> 
#> $sd_sd
#> [1] 0.113312
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
#> DEBUG [2020-08-26 15:22:46] Running for 1000 samples (across 4 chains each with a warm up of 200 iterations each) and 71 time steps of which 7 are a forecast

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
#>      1:       infections       infections    1 2020-02-08      1    2.206644
#>      2:       infections       infections    2 2020-02-09      1    9.431478
#>      3:       infections       infections    3 2020-02-10      1   21.395270
#>      4:       infections       infections    4 2020-02-11      1   30.846361
#>      5:       infections       infections    5 2020-02-12      1   46.237732
#>     ---                                                                     
#> 255067: prior_infections prior_infections   67 2020-04-14      1 2647.130393
#> 255068: prior_infections prior_infections   68 2020-04-15      1 2581.665553
#> 255069: prior_infections prior_infections   69 2020-04-16      1 2517.819692
#> 255070: prior_infections prior_infections   70 2020-04-17      1 2455.552770
#> 255071: prior_infections prior_infections   71 2020-04-18      1 2394.825740
#>         strat     type
#>      1:  <NA> estimate
#>      2:  <NA> estimate
#>      3:  <NA> estimate
#>      4:  <NA> estimate
#>      5:  <NA> estimate
#>     ---               
#> 255067:  <NA> forecast
#> 255068:  <NA> forecast
#> 255069:  <NA> forecast
#> 255070:  <NA> forecast
#> 255071:  <NA> forecast
#> 
#> $summarised
#>            date       variable strat     type     bottom         top
#>   1: 2020-02-22              R  <NA> estimate   0.906830    1.568452
#>   2: 2020-02-23              R  <NA> estimate   1.017340    1.560870
#>   3: 2020-02-24              R  <NA> estimate   1.101175    1.552501
#>   4: 2020-02-25              R  <NA> estimate   1.196505    1.554018
#>   5: 2020-02-26              R  <NA> estimate   1.316793    1.607314
#>  ---                                                                
#> 322: 2020-04-14 reported_cases  <NA> forecast 695.000000 6529.000000
#> 323: 2020-04-15 reported_cases  <NA> forecast 419.000000 5621.000000
#> 324: 2020-04-16 reported_cases  <NA> forecast 364.000000 6466.000000
#> 325: 2020-04-17 reported_cases  <NA> forecast 397.000000 8587.000000
#> 326: 2020-04-18 reported_cases  <NA> forecast 289.000000 7005.000000
#>            lower       upper      median        mean           sd
#>   1:    1.044737    1.309596    1.223438    1.222365 2.022448e-01
#>   2:    1.139059    1.368749    1.266639    1.270122 1.696516e-01
#>   3:    1.230104    1.417863    1.323299    1.325053 1.385959e-01
#>   4:    1.312648    1.467412    1.381972    1.385947 1.109667e-01
#>   5:    1.402101    1.527912    1.448480    1.451127 9.032870e-02
#>  ---                                                             
#> 322: 1254.000000 3183.000000 2772.000000 3386.243000 2.378292e+03
#> 323:  903.000000 2562.000000 2342.000000 3033.583000 2.574506e+03
#> 324:  924.000000 2942.000000 2685.500000 3657.875000 4.357484e+03
#> 325: 1191.000000 3496.000000 3096.000000 5197.417000 1.469759e+04
#> 326:  854.000000 2691.000000 2355.000000 5417.498000 3.578082e+04
```

Reported cases are returned separately in order to ease reporting of
forecasts and model evaluation.

``` r
estimates$estimated_reported_cases
#> $samples
#>              date sample cases  type
#>     1: 2020-02-22      1    93 gp_rt
#>     2: 2020-02-23      1   170 gp_rt
#>     3: 2020-02-24      1   119 gp_rt
#>     4: 2020-02-25      1   200 gp_rt
#>     5: 2020-02-26      1    78 gp_rt
#>    ---                              
#> 56996: 2020-04-14   1000  1464 gp_rt
#> 56997: 2020-04-15   1000  1581 gp_rt
#> 56998: 2020-04-16   1000  1439 gp_rt
#> 56999: 2020-04-17   1000  1499 gp_rt
#> 57000: 2020-04-18   1000  2037 gp_rt
#> 
#> $summarised
#>           date  type bottom   top lower upper median     mean          sd
#>  1: 2020-02-22 gp_rt     31   175    48   108   96.5  104.307    50.51481
#>  2: 2020-02-23 gp_rt     43   243    83   163  139.5  148.957    69.30538
#>  3: 2020-02-24 gp_rt     46   269    96   187  159.0  168.320    74.53631
#>  4: 2020-02-25 gp_rt     56   303    91   186  161.0  175.871    84.52076
#>  5: 2020-02-26 gp_rt     60   297    85   175  156.5  169.978    76.10236
#>  6: 2020-02-27 gp_rt     58   397   123   247  215.0  237.670   119.01744
#>  7: 2020-02-28 gp_rt     74   524   161   332  287.5  320.237   158.99453
#>  8: 2020-02-29 gp_rt     91   504   155   313  272.0  298.097   139.30512
#>  9: 2020-03-01 gp_rt    112   652   186   399  350.0  381.556   183.33621
#> 10: 2020-03-02 gp_rt    102   697   235   473  398.0  426.200   210.03135
#> 11: 2020-03-03 gp_rt    119   745   244   476  393.0  431.998   219.71267
#> 12: 2020-03-04 gp_rt     98   708   214   447  389.0  428.876   212.76694
#> 13: 2020-03-05 gp_rt    190  1019   302   633  567.0  615.846   294.66908
#> 14: 2020-03-06 gp_rt    259  1528   403   885  784.5  879.237   444.90818
#> 15: 2020-03-07 gp_rt    261  1474   404   846  765.0  846.184   425.04547
#> 16: 2020-03-08 gp_rt    311  1963   633  1268 1070.5 1167.124   609.49882
#> 17: 2020-03-09 gp_rt    352  2301   753  1511 1252.5 1354.985   656.34476
#> 18: 2020-03-10 gp_rt    406  2451   796  1609 1322.5 1427.365   694.09473
#> 19: 2020-03-11 gp_rt    300  2380   726  1502 1323.0 1463.005   759.85016
#> 20: 2020-03-12 gp_rt    565  3445  1198  2362 1911.0 2086.378  1040.60247
#> 21: 2020-03-13 gp_rt    924  5119  1399  2979 2636.0 2919.580  1477.38699
#> 22: 2020-03-14 gp_rt    806  4664  1450  2936 2463.0 2705.797  1303.82767
#> 23: 2020-03-15 gp_rt    831  5917  2178  4188 3215.5 3537.972  1715.04663
#> 24: 2020-03-16 gp_rt    897  6518  2137  4256 3554.0 3883.479  1899.95173
#> 25: 2020-03-17 gp_rt   1176  6454  1931  3889 3351.5 3703.498  1747.03212
#> 26: 2020-03-18 gp_rt   1209  6201  1929  3789 3153.0 3491.560  1735.67271
#> 27: 2020-03-19 gp_rt   1262  7499  2709  5265 4191.0 4512.308  2115.62561
#> 28: 2020-03-20 gp_rt   1370  9582  3310  6452 5533.0 5950.403  2785.43400
#> 29: 2020-03-21 gp_rt   1392  8500  2949  5727 4694.5 5008.814  2342.54236
#> 30: 2020-03-22 gp_rt   1689 10023  3075  6316 5376.5 5891.644  2845.84631
#> 31: 2020-03-23 gp_rt   1820 10445  3198  6456 5507.0 6033.768  2923.72504
#> 32: 2020-03-24 gp_rt   1773  8987  3287  6124 5014.5 5363.948  2520.06870
#> 33: 2020-03-25 gp_rt   1332  7680  2535  4977 4247.0 4606.321  2182.78039
#> 34: 2020-03-26 gp_rt   1830  9924  3106  6214 5407.0 5803.149  2741.00916
#> 35: 2020-03-27 gp_rt   2308 11236  3566  7045 6161.0 6619.664  3299.92875
#> 36: 2020-03-28 gp_rt   1623  9285  3015  5923 5047.0 5427.988  2553.89665
#> 37: 2020-03-29 gp_rt   1677 10351  3857  7317 5761.5 6183.392  3004.75043
#> 38: 2020-03-30 gp_rt   1426  9718  3435  6368 5267.0 5739.116  2743.96398
#> 39: 2020-03-31 gp_rt   1328  8490  2644  5417 4719.5 5104.631  2417.34093
#> 40: 2020-04-01 gp_rt   1069  6956  2727  5083 4007.0 4277.459  1925.56661
#> 41: 2020-04-02 gp_rt   1260  8153  2611  5088 4376.0 4823.408  2350.00527
#> 42: 2020-04-03 gp_rt   1493 10095  2620  5668 5183.0 5747.842  3014.38984
#> 43: 2020-04-04 gp_rt   1205  7391  2393  4770 4106.5 4460.568  2107.54760
#> 44: 2020-04-05 gp_rt   1366  8411  2658  5458 4610.0 4964.977  2407.32976
#> 45: 2020-04-06 gp_rt   1229  7727  2552  4974 4203.0 4636.208  2238.66048
#> 46: 2020-04-07 gp_rt   1153  6839  2220  4397 3573.5 3992.999  2067.80000
#> 47: 2020-04-08 gp_rt   1208  5882  1661  3414 3025.0 3343.165  1639.06683
#> 48: 2020-04-09 gp_rt    789  6664  2286  4486 3515.5 3924.625  2061.85895
#> 49: 2020-04-10 gp_rt   1279  7899  2743  5200 4164.5 4597.328  2373.24618
#> 50: 2020-04-11 gp_rt    761  5915  2123  4139 3261.5 3613.558  1814.91540
#> 51: 2020-04-12 gp_rt    896  7087  2086  4609 3600.5 4110.504  2342.33555
#> 52: 2020-04-13 gp_rt    825  7385  1686  3904 3265.5 3911.608  2376.82439
#> 53: 2020-04-14 gp_rt    695  6529  1254  3183 2772.0 3386.243  2378.29173
#> 54: 2020-04-15 gp_rt    419  5621   903  2562 2342.0 3033.583  2574.50603
#> 55: 2020-04-16 gp_rt    364  6466   924  2942 2685.5 3657.875  4357.48423
#> 56: 2020-04-17 gp_rt    397  8587  1191  3496 3096.0 5197.417 14697.58863
#> 57: 2020-04-18 gp_rt    289  7005   854  2691 2355.0 5417.498 35780.82391
#>           date  type bottom   top lower upper median     mean          sd
```

A summary table is returned for rapidly understanding the results and
for reporting purposes.

``` r
estimates$summary
#>                                  measure              estimate
#> 1: New confirmed cases by infection date     2180 (85 -- 7609)
#> 2:        Expected change in daily cases                Unsure
#> 3:            Effective reproduction no.    0.8 (0.31 -- 1.49)
#> 4:                        Rate of growth -0.06 (-0.22 -- 0.12)
#> 5:          Doubling/halving time (days)   -12.1 (5.6 -- -3.2)
#>     numeric_estimate
#> 1: <data.table[1x5]>
#> 2:              0.69
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
#> INFO [2020-08-26 15:25:06] Reporting estimates using data up to: 2020-04-11
#> INFO [2020-08-26 15:25:06] Producing estimates for: testland, realland
#> INFO [2020-08-26 15:25:06] Initialising estimates for: testland
#> DEBUG [2020-08-26 15:25:06] Running for 1000 samples (across 4 chains each with a warm up of 200 iterations each) and 71 time steps of which 7 are a forecast
#> INFO [2020-08-26 15:27:15] Completed estimates for: testland
#> INFO [2020-08-26 15:27:15] Initialising estimates for: realland
#> DEBUG [2020-08-26 15:27:15] Running for 1000 samples (across 4 chains each with a warm up of 200 iterations each) and 71 time steps of which 7 are a forecast
#> INFO [2020-08-26 15:29:58] Completed estimates for: realland
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
#> 1: realland                    2236 (125 -- 7642)
#> 2: testland                     2066 (37 -- 7156)
#>    Expected change in daily cases Effective reproduction no.
#> 1:                         Unsure         0.81 (0.23 -- 1.4)
#> 2:                         Unsure        0.78 (0.18 -- 1.39)
#>           Rate of growth Doubling/halving time (days)
#> 1:  -0.05 (-0.2 -- 0.14)            -13.2 (5 -- -3.4)
#> 2: -0.06 (-0.25 -- 0.12)          -11.2 (5.8 -- -2.8)
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
