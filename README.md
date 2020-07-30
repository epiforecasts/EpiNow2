
# EpiNow2: Estimate real-time case counts and time-varying epidemiological parameters

[![R-CMD-check](https://github.com/epiforecasts/EpiNow2/workflows/R-CMD-check/badge.svg)](https://github.com/epiforecasts/EpiNow2/actions)
[![Codecov test
coverage](https://codecov.io/gh/epiforecasts/EpiNow2/branch/master/graph/badge.svg)](https://codecov.io/gh/epiforecasts/EpiNow2?branch=master)
[![DOI](https://zenodo.org/badge/272995211.svg)](https://zenodo.org/badge/latestdoi/272995211)

This package estimates the time-varying reproduction number, rate of
spread, and doubling time using a range of open-source tools ([Abbott et
al.](doi.org/10.12688/wellcomeopenres.16006.1)), and current best
practices ([Gostic et al.](doi.org/10.1101/2020.06.18.20134858)). It
aims to help users avoid some of the limitations of naive
implementations in a framework that is informed by community feedback
and is under active development. It estimates the time-varying
reproduction number on cases by date of infection (using a similar
approach to that implemented in the
[`{EpiEstim}`](https://github.com/annecori/EpiEstim)). Imputed
infections are then mapped to observed data (for example cases by date
of report) via a series of uncertain delay distributions (in the
examples in the package documentation these are an incubation period and
a reporting delay) and a reporting model that can include weekly
periodicity. The default model uses a non-stationary Gaussian process to
estimate the time-varying reproduction number but optionally a
stationary Gaussian process may be used (faster to estimate but reduced
performance for real time estimates) and arbitrary breakpoints can be
defined. A fixed reproduction number is also supported. When combined
with user defined breakpoints this gives the option of modelling the
time-varying reproduction number as piecewise linear. The documentation
for
[`estimate_infections`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.html)
provides examples of the different options available. Propagating
uncertainty from all inputs into the final parameter estimates (helping
to mitigate spurious findings) is handled internally. Time-varying
estimates of the rate of growth are derived from the time-varying
reproduction estimates and the uncertain generation time. Optionally,
the time-varying reproduction number can be forecast forwards in time
using an integration with the
[`{EpiSoon}`](https://epiforecasts.io/EpiSoon) package and converted to
a case forecast using the renewal equation. Alternatively, the
time-varying reproduction number and cases can be forecast using a
Gaussian process. As a standalone tool non-parametric back-calculation
is also supported using a novel formulation based on a smoothed mean
delay shift of reported cases combined with a Gaussian process to
determine the most likely outbreak trajectory.

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
the underlying infection case curve alone is substantially less
computationally demanding than also estimating Rt.

### Reporting delays, incubation period and generation time

Distributions can either be fitted using package functionality or
determined elsewhere and then defined with uncertainty for use in
`{EpiNow2}`. When data is supplied a subsampled bootstrapped lognormal
will be fit (to account for uncertainty in the observed data without
being biased by changes in incidence). An arbitrary number of delay
distributions are supported with the most common use case likely to be a
incubation period followed by a reporting
delay.

``` r
reporting_delay <- EpiNow2::bootstrapped_dist_fit(rlnorm(100, log(6), 1))
## Set max allowed delay to 30 days to truncate computation
reporting_delay$max <- 30

reporting_delay
#> $mean
#> [1] 1.545216
#> 
#> $mean_sd
#> [1] 0.1324599
#> 
#> $sd
#> [1] 1.049099
#> 
#> $sd_sd
#> [1] 0.1069301
#> 
#> $max
#> [1] 30
```

Here we define the incubation period and generation time based on
literature estimates for Covid-19 (see
[here](https://github.com/epiforecasts/EpiNow/tree/master/data-raw) for
the code that generates these
estimates).

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
#>      1:       infections       infections    1 2020-02-09      1    1.930685
#>      2:       infections       infections    2 2020-02-10      1   12.296683
#>      3:       infections       infections    3 2020-02-11      1   18.511373
#>      4:       infections       infections    4 2020-02-12      1   34.002092
#>      5:       infections       infections    5 2020-02-13      1   50.878446
#>     ---                                                                     
#> 254066: prior_infections prior_infections   66 2020-04-14      1 2714.255263
#> 254067: prior_infections prior_infections   67 2020-04-15      1 2647.130393
#> 254068: prior_infections prior_infections   68 2020-04-16      1 2581.665553
#> 254069: prior_infections prior_infections   69 2020-04-17      1 2517.819692
#> 254070: prior_infections prior_infections   70 2020-04-18      1 2455.552770
#>         strat     type
#>      1:  <NA> estimate
#>      2:  <NA> estimate
#>      3:  <NA> estimate
#>      4:  <NA> estimate
#>      5:  <NA> estimate
#>     ---               
#> 254066:  <NA> forecast
#> 254067:  <NA> forecast
#> 254068:  <NA> forecast
#> 254069:  <NA> forecast
#> 254070:  <NA> forecast
#> 
#> $summarised
#>            date       variable strat     type      bottom         top
#>   1: 2020-02-22              R  <NA> estimate   0.9976886    1.632718
#>   2: 2020-02-23              R  <NA> estimate   1.1341943    1.661147
#>   3: 2020-02-24              R  <NA> estimate   1.2077832    1.629813
#>   4: 2020-02-25              R  <NA> estimate   1.3241587    1.651037
#>   5: 2020-02-26              R  <NA> estimate   1.4040138    1.667489
#>  ---                                                                 
#> 320: 2020-04-14 reported_cases  <NA> forecast 645.0000000 6444.000000
#> 321: 2020-04-15 reported_cases  <NA> forecast 514.0000000 5265.000000
#> 322: 2020-04-16 reported_cases  <NA> forecast 493.0000000 6875.000000
#> 323: 2020-04-17 reported_cases  <NA> forecast 472.0000000 8417.000000
#> 324: 2020-04-18 reported_cases  <NA> forecast 484.0000000 7679.000000
#>            lower       upper      median        mean           sd
#>   1:    1.161898    1.432021    1.326492    1.332901 1.955253e-01
#>   2:    1.278200    1.500735    1.373163    1.378565 1.604993e-01
#>   3:    1.361885    1.538032    1.425563    1.429665 1.282570e-01
#>   4:    1.431844    1.566878    1.486481    1.484806 1.008667e-01
#>   5:    1.492757    1.599931    1.542450    1.542226 8.177660e-02
#>  ---                                                             
#> 320: 1491.000000 3263.000000 2935.000000 3533.298000 2.386467e+03
#> 321: 1212.000000 2857.000000 2432.500000 2988.752000 2.445418e+03
#> 322: 1139.000000 3186.000000 2804.500000 3801.958000 4.097769e+03
#> 323: 1372.000000 3685.000000 3198.000000 5214.320000 1.880585e+04
#> 324:  956.000000 2894.000000 2531.000000 4865.491000 2.108619e+04
```

Reported cases are returned separately in order to ease reporting of
forecasts and model evaluation.

``` r
estimates$estimated_reported_cases
#> $samples
#>              date sample cases  type
#>     1: 2020-02-22      1    72 gp_rt
#>     2: 2020-02-23      1   179 gp_rt
#>     3: 2020-02-24      1   253 gp_rt
#>     4: 2020-02-25      1   150 gp_rt
#>     5: 2020-02-26      1   133 gp_rt
#>    ---                              
#> 56996: 2020-04-14   1000  3577 gp_rt
#> 56997: 2020-04-15   1000  1793 gp_rt
#> 56998: 2020-04-16   1000  2103 gp_rt
#> 56999: 2020-04-17   1000   472 gp_rt
#> 57000: 2020-04-18   1000  2418 gp_rt
#> 
#> $summarised
#>           date  type bottom   top lower upper median     mean          sd
#>  1: 2020-02-22 gp_rt     29   147    55   100   82.0   87.506    37.87097
#>  2: 2020-02-23 gp_rt     36   209    67   137  120.0  127.122    56.40657
#>  3: 2020-02-24 gp_rt     51   240    86   161  134.0  146.775    65.09033
#>  4: 2020-02-25 gp_rt     57   234    77   155  142.0  148.686    59.62819
#>  5: 2020-02-26 gp_rt     51   249    75   156  142.0  153.766    67.69020
#>  6: 2020-02-27 gp_rt     63   330   125   229  191.5  205.435    88.86615
#>  7: 2020-02-28 gp_rt     90   473   144   297  266.0  286.814   127.44537
#>  8: 2020-02-29 gp_rt    102   451   151   287  251.0  266.803   113.18316
#>  9: 2020-03-01 gp_rt    119   600   180   370  325.0  357.672   171.15426
#> 10: 2020-03-02 gp_rt    140   660   269   466  388.5  407.169   171.44603
#> 11: 2020-03-03 gp_rt    155   700   255   465  384.0  421.291   185.03628
#> 12: 2020-03-04 gp_rt    145   701   218   448  399.0  424.808   186.79433
#> 13: 2020-03-05 gp_rt    182  1003   329   640  568.5  612.244   270.90482
#> 14: 2020-03-06 gp_rt    333  1509   541   986  803.5  878.620   393.86980
#> 15: 2020-03-07 gp_rt    256  1356   454   888  801.0  866.045   373.52259
#> 16: 2020-03-08 gp_rt    391  1912   746  1338 1073.0 1162.320   507.78628
#> 17: 2020-03-09 gp_rt    419  2179   840  1562 1294.0 1381.053   578.44088
#> 18: 2020-03-10 gp_rt    499  2384   875  1578 1333.0 1428.703   619.72803
#> 19: 2020-03-11 gp_rt    580  2450   945  1705 1372.0 1455.251   634.73807
#> 20: 2020-03-12 gp_rt    597  3427  1241  2260 1929.0 2075.379   922.69655
#> 21: 2020-03-13 gp_rt    998  4721  1789  3248 2667.5 2848.014  1280.35860
#> 22: 2020-03-14 gp_rt    825  4422  1810  3261 2541.0 2716.711  1180.18493
#> 23: 2020-03-15 gp_rt   1115  5687  2041  3970 3251.5 3539.352  1630.38673
#> 24: 2020-03-16 gp_rt   1167  6252  2177  4161 3535.0 3798.382  1698.41383
#> 25: 2020-03-17 gp_rt   1130  6021  2330  4159 3520.5 3792.849  1699.05103
#> 26: 2020-03-18 gp_rt   1174  5699  2066  3832 3242.0 3492.269  1478.47621
#> 27: 2020-03-19 gp_rt   1516  7421  2604  4816 4173.5 4506.203  1940.88979
#> 28: 2020-03-20 gp_rt   1581  9287  3630  6642 5421.0 5835.917  2567.43570
#> 29: 2020-03-21 gp_rt   1648  8094  2945  5596 4688.5 4960.858  2177.20211
#> 30: 2020-03-22 gp_rt   2300  9718  3035  6267 5582.5 5865.234  2543.39541
#> 31: 2020-03-23 gp_rt   2046  9434  3076  6028 5545.5 5871.242  2504.05670
#> 32: 2020-03-24 gp_rt   1966  9077  3125  5995 5145.0 5526.615  2429.53809
#> 33: 2020-03-25 gp_rt   1433  7544  2858  5105 4317.0 4661.903  2115.48837
#> 34: 2020-03-26 gp_rt   1529  8713  3187  5926 5102.5 5555.179  2518.98884
#> 35: 2020-03-27 gp_rt   2330 10569  4246  7549 6134.5 6565.456  2845.39111
#> 36: 2020-03-28 gp_rt   1786  8842  3131  5825 5041.0 5402.342  2361.78178
#> 37: 2020-03-29 gp_rt   2167  9810  3204  6071 5527.5 6031.692  2588.90665
#> 38: 2020-03-30 gp_rt   2240  9985  3447  6605 5449.5 5880.109  2581.86211
#> 39: 2020-03-31 gp_rt   1280  8171  2882  5577 4611.0 5043.097  2393.20006
#> 40: 2020-04-01 gp_rt   1394  6799  2566  4615 3899.5 4192.570  1866.59196
#> 41: 2020-04-02 gp_rt   1645  8030  2739  5158 4556.0 4874.705  2174.57274
#> 42: 2020-04-03 gp_rt   1700  9283  3225  6061 5295.0 5751.349  2597.19810
#> 43: 2020-04-04 gp_rt   1635  7286  2853  5097 4217.5 4393.266  1852.38997
#> 44: 2020-04-05 gp_rt   1893  8614  3002  5578 4752.5 5061.313  2222.68043
#> 45: 2020-04-06 gp_rt   1427  7669  2720  5157 4395.0 4715.568  2269.74026
#> 46: 2020-04-07 gp_rt   1281  6627  2446  4535 3755.0 4052.492  1765.71419
#> 47: 2020-04-08 gp_rt   1237  5748  1886  3627 3114.5 3368.006  1534.13088
#> 48: 2020-04-09 gp_rt    930  6474  1896  4004 3521.0 3928.244  1870.66115
#> 49: 2020-04-10 gp_rt   1277  7421  2272  4808 4254.0 4571.091  2176.19018
#> 50: 2020-04-11 gp_rt   1071  6154  2027  3939 3252.5 3612.370  1767.25537
#> 51: 2020-04-12 gp_rt    848  6992  2282  4520 3655.5 4093.223  2175.25044
#> 52: 2020-04-13 gp_rt   1107  6857  1958  3975 3445.5 3904.479  2099.78271
#> 53: 2020-04-14 gp_rt    645  6444  1491  3263 2935.0 3533.298  2386.46652
#> 54: 2020-04-15 gp_rt    514  5265  1212  2857 2432.5 2988.752  2445.41752
#> 55: 2020-04-16 gp_rt    493  6875  1139  3186 2804.5 3801.958  4097.76886
#> 56: 2020-04-17 gp_rt    472  8417  1372  3685 3198.0 5214.320 18805.85271
#> 57: 2020-04-18 gp_rt    484  7679   956  2894 2531.0 4865.491 21086.18578
#>           date  type bottom   top lower upper median     mean          sd
```

A summary table is returned for rapidly understanding the results and
for reporting purposes.

``` r
estimates$summary
#>                                  measure              estimate
#> 1: New confirmed cases by infection date    2568 (150 -- 7862)
#> 2:        Expected change in daily cases                Unsure
#> 3:            Effective reproduction no.      0.8 (0.3 -- 1.5)
#> 4:                        Rate of growth -0.04 (-0.22 -- 0.12)
#> 5:          Doubling/halving time (days)   -15.4 (5.6 -- -3.2)
#>     numeric_estimate
#> 1: <data.table[1x5]>
#> 2:              0.67
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
#> 1: realland                    2524 (132 -- 7240)
#> 2: testland                    2564 (116 -- 7585)
#>    Expected change in daily cases Effective reproduction no.
#> 1:                         Unsure           0.8 (0.4 -- 1.4)
#> 2:                         Unsure           0.9 (0.4 -- 1.5)
#>           Rate of growth Doubling/halving time (days)
#> 1: -0.05 (-0.19 -- 0.13)            -14 (5.4 -- -3.7)
#> 2:  -0.04 (-0.2 -- 0.14)          -16.5 (5.1 -- -3.5)
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
[limitations](https://epiforecasts.io/covid/) as these are key to
understanding our results.

## Contributing

File an issue [here](https://github.com/epiforecasts/EpiNow2/issues) if
you have identified an issue with the package. Please note that due to
operational constraints priority will be given to users informing
government policy or offering methodological insights. We welcome all
contributions, in particular those that improve the approach or the
robustness of the code base.
