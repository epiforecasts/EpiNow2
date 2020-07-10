
# EpiNow2: Estimate realtime case counts and time-varying epidemiological parameters

[![R-CMD-check](https://github.com/epiforecasts/EpiNow2/workflows/R-CMD-check/badge.svg)](https://github.com/epiforecasts/EpiNow2/actions)
[![Codecov test
coverage](https://codecov.io/gh/epiforecasts/EpiNow2/branch/master/graph/badge.svg)](https://codecov.io/gh/epiforecasts/EpiNow2?branch=master)
[![DOI](https://zenodo.org/badge/247464257.svg)](https://zenodo.org/badge/latestdoi/247464257)

*This package is under development.*

This package estimates the time-varying reproduction number, rate of
spread, and doubling time using a range of open-source tools and current
best practices. It aims to help users avoid some of the limitations of
naive implementations in a framework that is informed by community
feedback and is under active development. It assumes that only limited
data is available on cases by date of onset and instead uses cases by
date of report. These are then imputed to case counts by date of
infection using an uncertain reporting delay and incubation period via a
Gaussian process based method. Right truncation of cases is dealt with
internally by `{EpiNow2}`, as is propagating uncertainty from all inputs
into the final parameter estimates (helping to mitigate spurious
findings). Time-varying estimates of the reproduction number are
estimated using a similar approach to that implemented in the
[`{EpiEstim}`](https://github.com/annecori/EpiEstim) package by date of
infection with a generation time estimate that includes uncertainty and
variation over time controlled using a Gaussian process. Time-varying
estimates of the rate of growth are derived from the time-varying
reproduction estimates and the uncertain generation time. Optionally,
the time-varying reproduction number can be forecast forwards in time
using an integration with the
[`{EpiSoon}`](https://epiforecasts.io/EpiSoon) package and converted to
a case forecast using the renewal equation. Alternatively, the
time-varying reproduction number and cases can be forecast using a
Gaussian process. See the
[methods](https://epiforecasts.io/covid/methods.html) section of our
Covid-19 site for a detailed discussion of the approach.

## Installation

Install the stable version of the package using
[`{drat}`](https://epiforecasts.io/drat/):

``` r
install.packages("drat")
drat:::add("epiforecasts")
install.packages("EpiNow2")
```

Install the development version of the package with:

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

`{EpiNow}` is designed to be used with a single function call or to be
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
the underyling infection case curve alone is substantially less
computationally demanding than also estimating Rt.

### Reporting delays, incubation period and generation time

Distributions can either be fitted using package functionality or
determined elsewhere and then defined with uncertainty for use in
`{EpiNow2}`. When data is supplied a subsampled bootstrapped lognormal
will be fit (to account for uncertainty in the observed data without
being biased by changes in incidence). An arbitary number of delay
distributions are supported with the most common use case likely to be a
incubation period followed by a reporting delay.

``` r
reporting_delay <- EpiNow2::bootstrapped_dist_fit(rlnorm(100, log(6), 1))
## Set max allowed delay to 30 days to truncate computation
reporting_delay$max <- 30

reporting_delay
#> $mean
#> [1] 1.69326
#> 
#> $mean_sd
#> [1] 0.1406907
#> 
#> $sd
#> [1] 1.044516
#> 
#> $sd_sd
#> [1] 0.1040565
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
#> [[1]]
#> Stan model 'estimate_infections' does not contain samples.

names(estimates)
#> [1] "estimates"                "estimated_reported_cases"
#> [3] "summary"                  "plots"
```

Both summary measures and posterior samples are returned for all
parameters in an easily explored format.

``` r
estimates$estimates
#> $samples
#>           variable          parameter time       date sample     value strat
#>      1: infections imputed_infections    1 2020-02-08      1  3.000000  <NA>
#>      2: infections imputed_infections    2 2020-02-09      1 11.000000  <NA>
#>      3: infections imputed_infections    3 2020-02-10      1 17.000000  <NA>
#>      4: infections imputed_infections    4 2020-02-11      1 36.000000  <NA>
#>      5: infections imputed_infections    5 2020-02-12      1 46.000000  <NA>
#>     ---                                                                     
#> 191317:      gt_sd              gt_sd   NA       <NA>    746  3.064648  <NA>
#> 191318:      gt_sd              gt_sd   NA       <NA>    747  3.047779  <NA>
#> 191319:      gt_sd              gt_sd   NA       <NA>    748  3.218911  <NA>
#> 191320:      gt_sd              gt_sd   NA       <NA>    749  3.263036  <NA>
#> 191321:      gt_sd              gt_sd   NA       <NA>    750  2.989850  <NA>
#>             type
#>      1: estimate
#>      2: estimate
#>      3: estimate
#>      4: estimate
#>      5: estimate
#>     ---         
#> 191317:     <NA>
#> 191318:     <NA>
#> 191319:     <NA>
#> 191320:     <NA>
#> 191321:     <NA>
#> 
#> $summarised
#>            date       variable strat     type      bottom         top
#>   1: 2020-02-22              R  <NA> estimate   0.8726573    1.634377
#>   2: 2020-02-23              R  <NA> estimate   0.9825272    1.619101
#>   3: 2020-02-24              R  <NA> estimate   1.1188984    1.627935
#>   4: 2020-02-25              R  <NA> estimate   1.2108618    1.617403
#>   5: 2020-02-26              R  <NA> estimate   1.3306117    1.658843
#>  ---                                                                 
#> 322: 2020-04-14 reported_cases  <NA> forecast 592.0000000 5994.000000
#> 323: 2020-04-15 reported_cases  <NA> forecast 507.0000000 5532.000000
#> 324: 2020-04-16 reported_cases  <NA> forecast 469.0000000 6467.000000
#> 325: 2020-04-17 reported_cases  <NA> forecast 438.0000000 8886.000000
#> 326: 2020-04-18 reported_cases  <NA> forecast 437.0000000 7358.000000
#>            lower       upper      median        mean           sd
#>   1:    1.112980    1.379794    1.241728    1.240338 2.225276e-01
#>   2:    1.167967    1.401843    1.294460    1.295174 1.883661e-01
#>   3:    1.263503    1.458975    1.362034    1.358207 1.551193e-01
#>   4:    1.358183    1.521900    1.429850    1.428257 1.247580e-01
#>   5:    1.434784    1.567164    1.503556    1.503574 1.014485e-01
#>  ---                                                             
#> 322: 1595.000000 3417.000000 2881.000000 3392.008011 2.369802e+03
#> 323: 1014.000000 2652.000000 2389.000000 3135.606142 4.132608e+03
#> 324: 1192.000000 3086.000000 2724.000000 4058.710280 9.202820e+03
#> 325: 1132.000000 3417.000000 3072.000000 5534.747664 1.975031e+04
#> 326:  922.000000 2807.000000 2494.000000 5893.279039 3.405957e+04
```

Reported cases are returned separately in order to ease reporting of
forecasts and model evaluation.

``` r
estimates$estimated_reported_cases
#> $samples
#>              date sample cases  type
#>     1: 2020-02-22      1    31 gp_rt
#>     2: 2020-02-23      1   161 gp_rt
#>     3: 2020-02-24      1   256 gp_rt
#>     4: 2020-02-25      1   196 gp_rt
#>     5: 2020-02-26      1    60 gp_rt
#>    ---                              
#> 42746: 2020-04-14    750  2822 gp_rt
#> 42747: 2020-04-15    750  2869 gp_rt
#> 42748: 2020-04-16    750  2582 gp_rt
#> 42749: 2020-04-17    750  5146 gp_rt
#> 42750: 2020-04-18    750 10425 gp_rt
#> 
#> $summarised
#>           date  type bottom   top lower upper median      mean          sd
#>  1: 2020-02-22 gp_rt     29   174    52   110     98  105.4326    49.48850
#>  2: 2020-02-23 gp_rt     39   235    77   151    133  145.3338    65.17587
#>  3: 2020-02-24 gp_rt     52   287   100   190    153  167.2924    80.20109
#>  4: 2020-02-25 gp_rt     49   291    83   179    157  172.8491    82.42469
#>  5: 2020-02-26 gp_rt     50   304    97   190    158  174.0614    86.12802
#>  6: 2020-02-27 gp_rt     60   374   111   227    205  223.3271   104.45555
#>  7: 2020-02-28 gp_rt     99   523   165   341    290  314.8304   153.53169
#>  8: 2020-02-29 gp_rt     77   462   139   289    259  283.5634   138.86265
#>  9: 2020-03-01 gp_rt     94   605   238   449    356  377.2310   176.99503
#> 10: 2020-03-02 gp_rt    161   748   190   404    375  415.4299   202.90916
#> 11: 2020-03-03 gp_rt    107   699   259   481    370  411.0881   206.02644
#> 12: 2020-03-04 gp_rt    132   755   274   511    401  440.2056   214.68632
#> 13: 2020-03-05 gp_rt    173   972   285   588    516  574.3792   272.24252
#> 14: 2020-03-06 gp_rt    213  1396   483   947    772  845.8371   422.92578
#> 15: 2020-03-07 gp_rt    245  1430   458   925    777  853.3231   414.76687
#> 16: 2020-03-08 gp_rt    370  1904   632  1245   1036 1126.7490   547.89372
#> 17: 2020-03-09 gp_rt    319  2219   678  1380   1230 1322.3578   629.87498
#> 18: 2020-03-10 gp_rt    491  2510   768  1517   1328 1448.4486   682.11564
#> 19: 2020-03-11 gp_rt    394  2446   749  1486   1311 1459.0347   691.89640
#> 20: 2020-03-12 gp_rt    562  3533  1206  2366   1886 2083.3151   998.53104
#> 21: 2020-03-13 gp_rt    774  4886  1415  2966   2637 2878.5260  1336.56089
#> 22: 2020-03-14 gp_rt    682  4632  1476  3047   2469 2683.7704  1327.23715
#> 23: 2020-03-15 gp_rt   1043  6197  1907  3745   3054 3470.2577  1708.48867
#> 24: 2020-03-16 gp_rt   1022  6460  2504  4583   3579 3853.9413  1897.60047
#> 25: 2020-03-17 gp_rt    922  6280  2026  4008   3452 3792.8518  1791.18231
#> 26: 2020-03-18 gp_rt    965  5964  1738  3688   3270 3531.8505  1734.17108
#> 27: 2020-03-19 gp_rt   1666  7982  2759  5273   4119 4535.1789  2103.09363
#> 28: 2020-03-20 gp_rt   1947 10031  3293  6456   5432 5873.0187  2825.81432
#> 29: 2020-03-21 gp_rt   1380  8367  2786  5586   4713 5072.3418  2346.25603
#> 30: 2020-03-22 gp_rt   1949 10053  3321  6450   5510 5802.5514  2632.80346
#> 31: 2020-03-23 gp_rt   1566  9760  3496  6647   5437 5936.4646  2795.68348
#> 32: 2020-03-24 gp_rt   1575  9049  2995  5809   4966 5491.5407  2786.13226
#> 33: 2020-03-25 gp_rt   1389  7818  2900  5353   4338 4718.4406  2188.89886
#> 34: 2020-03-26 gp_rt   1485  9483  3010  6085   5211 5639.6662  2719.20350
#> 35: 2020-03-27 gp_rt   2121 11833  3356  7037   6340 6896.7597  3383.47594
#> 36: 2020-03-28 gp_rt   1543  9107  3159  6166   5134 5437.5020  2548.24181
#> 37: 2020-03-29 gp_rt   1641  9934  3029  6341   5374 5901.6689  3000.84388
#> 38: 2020-03-30 gp_rt   1996 10190  3173  6498   5461 5856.3284  2780.76869
#> 39: 2020-03-31 gp_rt   1684  8504  2493  5289   4787 5081.4660  2293.50071
#> 40: 2020-04-01 gp_rt   1353  7042  2883  5322   3985 4302.8985  2048.45567
#> 41: 2020-04-02 gp_rt   1424  8311  2413  5189   4534 4940.4419  2320.38293
#> 42: 2020-04-03 gp_rt   1478  9492  2853  6027   5360 5790.8451  2739.01873
#> 43: 2020-04-04 gp_rt   1294  7530  2698  5112   4158 4555.0000  2159.26182
#> 44: 2020-04-05 gp_rt   1271  8599  2471  5104   4529 5021.9386  2473.35351
#> 45: 2020-04-06 gp_rt   1396  8228  2198  4994   4420 4846.1041  2363.49628
#> 46: 2020-04-07 gp_rt   1371  6860  2126  4445   3818 4109.3712  2059.14696
#> 47: 2020-04-08 gp_rt    910  5951  1601  3524   3026 3361.0614  1697.20272
#> 48: 2020-04-09 gp_rt   1083  6504  2363  4429   3574 3853.3965  1833.57328
#> 49: 2020-04-10 gp_rt    930  7334  2463  5070   4070 4463.3979  2257.31450
#> 50: 2020-04-11 gp_rt   1072  6062  1989  3846   3261 3610.2991  1788.17236
#> 51: 2020-04-12 gp_rt    997  7371  1838  3863   3417 4070.2256  2388.03581
#> 52: 2020-04-13 gp_rt    813  6717  1673  3801   3309 3854.0628  2414.28396
#> 53: 2020-04-14 gp_rt    592  5994  1595  3417   2881 3392.0080  2369.80232
#> 54: 2020-04-15 gp_rt    507  5532  1014  2652   2389 3135.6061  4132.60848
#> 55: 2020-04-16 gp_rt    469  6467  1192  3086   2724 4058.7103  9202.82012
#> 56: 2020-04-17 gp_rt    438  8886  1132  3417   3072 5534.7477 19750.31311
#> 57: 2020-04-18 gp_rt    437  7358   922  2807   2494 5893.2790 34059.56811
#>           date  type bottom   top lower upper median      mean          sd
```

A summary table is returned for rapidly understanding the results and
for reporting purposes.

``` r
estimates$summary
#>                                  measure              estimate numeric_estimate
#> 1: New confirmed cases by infection date     2181 (32 -- 7613)     <data.table>
#> 2:        Expected change in daily cases                Unsure             0.69
#> 3:            Effective reproduction no.      0.8 (0.2 -- 1.5)     <data.table>
#> 4:                        Rate of growth -0.06 (-0.23 -- 0.14)     <data.table>
#> 5:          Doubling/halving time (days)     -11.5 (4.8 -- -3)     <data.table>
```

A range of plots are returned (with the single summary plot shown
below).

``` r
estimates$plots$summary
```

![](man/figures/unnamed-chunk-11-1.png)<!-- -->

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
#> [[1]]
#> Stan model 'estimate_infections' does not contain samples.
#> 
#> [[2]]
#> Stan model 'estimate_infections' does not contain samples.
#> 
#> [[1]]
#> Stan model 'estimate_infections' does not contain samples.
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
#> 1: realland                     2057 (45 -- 7912)
#> 2: testland                     2082 (81 -- 7159)
#>    Expected change in daily cases Effective reproduction no.
#> 1:                         Unsure           0.8 (0.2 -- 1.5)
#> 2:                         Unsure           0.8 (0.2 -- 1.4)
#>           Rate of growth Doubling/halving time (days)
#> 1: -0.07 (-0.26 -- 0.14)          -10.4 (4.9 -- -2.7)
#> 2: -0.06 (-0.21 -- 0.16)          -11.3 (4.2 -- -3.3)
```

A range of plots are again returned (with the single summary plot shown
below).

``` r
estimates$summary$summary_plot
```

![](man/figures/unnamed-chunk-15-1.png)<!-- -->

### Reporting templates

Rmarkdown templates are provided in the package (`templates`) for
semi-automated reporting of estimates. These are currently undocumented
but an example integration can be seen
[here](https://github.com/epiforecasts/covid/blob/master/_posts/national/united-kingdom/united-kingdom.Rmd).
If using these templates to report your results please highlight our
[limitations](https://epiforecasts.io/covid/) as these are key to
understanding our results.

## Contributing

File an issue [here](https://github.com/epiforecasts/EpiNow2/issues) if
you have identified an issue with the package. Please note that due to
operational constraints priority will be given to users informing
government policy or offering methodological insights. We welcome all
contributions, in particular those that improve the approach or the
robustness of the code base.
