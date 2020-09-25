
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
#> [1] 1.699973
#> 
#> $mean_sd
#> [1] 0.1330694
#> 
#> $sd
#> [1] 1.122008
#> 
#> $sd_sd
#> [1] 0.1333347
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
                             horizon = 7, samples = 2000, warmup = 500, 
                             cores = 4, chains = 4,
                             adapt_delta = 0.95)
#> DEBUG [2020-09-25 09:52:42] Running for 2000 samples (across 4 chains each with a warm up of 500 iterations each) and 72 time steps of which 7 are a forecast

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
#>      1:       infections       infections    1 2020-02-07      1    2.079038
#>      2:       infections       infections    2 2020-02-08      1    8.911437
#>      3:       infections       infections    3 2020-02-09      1   18.287918
#>      4:       infections       infections    4 2020-02-10      1   34.939783
#>      5:       infections       infections    5 2020-02-11      1   46.300247
#>     ---                                                                     
#> 512068: prior_infections prior_infections   68 2020-04-14      1 2581.665553
#> 512069: prior_infections prior_infections   69 2020-04-15      1 2517.819692
#> 512070: prior_infections prior_infections   70 2020-04-16      1 2455.552770
#> 512071: prior_infections prior_infections   71 2020-04-17      1 2394.825740
#> 512072: prior_infections prior_infections   72 2020-04-18      1 2335.600519
#>         strat     type
#>      1:  <NA> estimate
#>      2:  <NA> estimate
#>      3:  <NA> estimate
#>      4:  <NA> estimate
#>      5:  <NA> estimate
#>     ---               
#> 512068:  <NA> forecast
#> 512069:  <NA> forecast
#> 512070:  <NA> forecast
#> 512071:  <NA> forecast
#> 512072:  <NA> forecast
#> 
#> $summarised
#>            date       variable strat     type      bottom         top
#>   1: 2020-02-22              R  <NA> estimate   0.7865656    1.483450
#>   2: 2020-02-23              R  <NA> estimate   0.9088766    1.505936
#>   3: 2020-02-24              R  <NA> estimate   0.9932831    1.482851
#>   4: 2020-02-25              R  <NA> estimate   1.0993605    1.493949
#>   5: 2020-02-26              R  <NA> estimate   1.2026843    1.522864
#>  ---                                                                 
#> 324: 2020-04-14 reported_cases  <NA> forecast 459.0000000 6383.000000
#> 325: 2020-04-15 reported_cases  <NA> forecast 280.0000000 5989.000000
#> 326: 2020-04-16 reported_cases  <NA> forecast 300.0000000 7257.000000
#> 327: 2020-04-17 reported_cases  <NA> forecast 374.0000000 9538.000000
#> 328: 2020-04-18 reported_cases  <NA> forecast 294.0000000 8577.000000
#>             lower       upper central_lower central_upper      median
#>   1:    0.9173804    1.204700      1.100957      1.200238    1.114530
#>   2:    1.0347892    1.272916      1.167491      1.250299    1.168776
#>   3:    1.1129007    1.304168      1.232717      1.304168    1.223973
#>   4:    1.2234453    1.380543      1.250095      1.308676    1.291733
#>   5:    1.3132086    1.440152      1.342231      1.389712    1.365992
#>  ---                                                                 
#> 324: 1151.0000000 3182.000000   1750.000000   2462.000000 2814.500000
#> 325:  727.0000000 2464.000000   1216.000000   1811.000000 2287.500000
#> 326:  830.0000000 2906.000000   1156.000000   1886.000000 2641.000000
#> 327: 1109.0000000 3539.000000   1335.000000   2173.000000 3071.000000
#> 328:  758.0000000 2730.000000    980.000000   1643.000000 2424.500000
#>             mean           sd
#>   1:    1.111590 2.134847e-01
#>   2:    1.164311 1.810044e-01
#>   3:    1.225330 1.493345e-01
#>   4:    1.293591 1.203529e-01
#>   5:    1.367477 9.798791e-02
#>  ---                         
#> 324: 3481.602500 2.787764e+03
#> 325: 3186.685000 4.008561e+03
#> 326: 4013.834500 7.778462e+03
#> 327: 5812.774500 1.855728e+04
#> 328: 8875.457500 9.518929e+04
```

Reported cases are returned separately in order to ease reporting of
forecasts and model evaluation.

``` r
estimates$estimated_reported_cases
#> $samples
#>               date sample cases  type
#>      1: 2020-02-22      1   279 gp_rt
#>      2: 2020-02-23      1    42 gp_rt
#>      3: 2020-02-24      1    96 gp_rt
#>      4: 2020-02-25      1   235 gp_rt
#>      5: 2020-02-26      1    71 gp_rt
#>     ---                              
#> 113996: 2020-04-14   2000  1893 gp_rt
#> 113997: 2020-04-15   2000  1343 gp_rt
#> 113998: 2020-04-16   2000  2326 gp_rt
#> 113999: 2020-04-17   2000  2391 gp_rt
#> 114000: 2020-04-18   2000  1539 gp_rt
#> 
#> $summarised
#>           date  type bottom   top lower upper central_lower central_upper
#>  1: 2020-02-22 gp_rt     32   228    60   138            84           112
#>  2: 2020-02-23 gp_rt     40   305    93   196           105           141
#>  3: 2020-02-24 gp_rt     50   358    96   214           144           184
#>  4: 2020-02-25 gp_rt     46   364    83   201           135           177
#>  5: 2020-02-26 gp_rt     45   340    91   206           134           176
#>  6: 2020-02-27 gp_rt     73   481   116   271           156           211
#>  7: 2020-02-28 gp_rt     77   615   185   394           196           269
#>  8: 2020-02-29 gp_rt     78   553   135   321           174           241
#>  9: 2020-03-01 gp_rt    106   735   190   423           230           314
#> 10: 2020-03-02 gp_rt    100   764   222   470           279           363
#> 11: 2020-03-03 gp_rt    103   787   217   472           269           359
#> 12: 2020-03-04 gp_rt     81   774   204   455           223           316
#> 13: 2020-03-05 gp_rt    172  1108   318   662           433           548
#> 14: 2020-03-06 gp_rt    215  1571   456   951           540           716
#> 15: 2020-03-07 gp_rt    166  1458   437   924           642           827
#> 16: 2020-03-08 gp_rt    240  1915   515  1187           697           929
#> 17: 2020-03-09 gp_rt    274  2258   612  1365           713           998
#> 18: 2020-03-10 gp_rt    346  2412   623  1468           895          1213
#> 19: 2020-03-11 gp_rt    272  2427   772  1586          1077          1358
#> 20: 2020-03-12 gp_rt    447  3671   946  2169          1279          1698
#> 21: 2020-03-13 gp_rt    644  5036  1220  2886          1692          2293
#> 22: 2020-03-14 gp_rt    604  4647  1302  2897          2025          2590
#> 23: 2020-03-15 gp_rt    823  5916  1848  3819          2270          3020
#> 24: 2020-03-16 gp_rt    983  6840  1681  4022          2090          2940
#> 25: 2020-03-17 gp_rt    933  6634  2216  4444          2770          3568
#> 26: 2020-03-18 gp_rt    880  6096  1775  3729          1894          2596
#> 27: 2020-03-19 gp_rt    994  7958  2477  5105          3078          4034
#> 28: 2020-03-20 gp_rt   1399 10487  2715  6027          3684          4786
#> 29: 2020-03-21 gp_rt   1396  9046  2435  5347          2873          3957
#> 30: 2020-03-22 gp_rt   1388 10554  2681  6364          3808          5126
#> 31: 2020-03-23 gp_rt   1237 10284  2658  6159          4287          5595
#> 32: 2020-03-24 gp_rt   1307  9677  2581  5792          3953          5087
#> 33: 2020-03-25 gp_rt   1179  8109  2388  5139          2706          3729
#> 34: 2020-03-26 gp_rt   1517  9797  2757  5966          3437          4568
#> 35: 2020-03-27 gp_rt   1664 12067  3243  7214          4243          5658
#> 36: 2020-03-28 gp_rt   1434  9409  2487  5698          3298          4438
#> 37: 2020-03-29 gp_rt   1338 10510  3210  6743          4665          5932
#> 38: 2020-03-30 gp_rt   1480 10384  2467  5976          3451          4700
#> 39: 2020-03-31 gp_rt   1199  8636  2048  5136          3015          4045
#> 40: 2020-04-01 gp_rt   1095  7330  1868  4351          2559          3457
#> 41: 2020-04-02 gp_rt   1226  8588  2431  5383          3591          4612
#> 42: 2020-04-03 gp_rt   1192  9932  2912  6230          3452          4686
#> 43: 2020-04-04 gp_rt   1094  8032  2185  4883          3082          4080
#> 44: 2020-04-05 gp_rt   1390  9024  2332  5213          3204          4217
#> 45: 2020-04-06 gp_rt    882  8056  2405  5195          2956          3949
#> 46: 2020-04-07 gp_rt    965  7287  1735  4082          2574          3419
#> 47: 2020-04-08 gp_rt    845  5863  1652  3596          1785          2521
#> 48: 2020-04-09 gp_rt    785  6638  1904  4243          2500          3288
#> 49: 2020-04-10 gp_rt   1046  8220  2001  4713          2791          3768
#> 50: 2020-04-11 gp_rt    664  6475  1501  3662          1839          2658
#> 51: 2020-04-12 gp_rt    753  7376  1474  3922          2120          3031
#> 52: 2020-04-13 gp_rt    341  7180  1332  3588          2161          2990
#> 53: 2020-04-14 gp_rt    459  6383  1151  3182          1750          2462
#> 54: 2020-04-15 gp_rt    280  5989   727  2464          1216          1811
#> 55: 2020-04-16 gp_rt    300  7257   830  2906          1156          1886
#> 56: 2020-04-17 gp_rt    374  9538  1109  3539          1335          2173
#> 57: 2020-04-18 gp_rt    294  8577   758  2730           980          1643
#>           date  type bottom   top lower upper central_lower central_upper
#>     median      mean          sd
#>  1:  120.0  131.0695    66.57942
#>  2:  163.0  179.0265    93.29099
#>  3:  186.0  207.2420   106.36687
#>  4:  185.0  206.4170   113.04972
#>  5:  180.5  198.7905   103.73238
#>  6:  240.0  265.9635   138.24034
#>  7:  321.0  350.8570   187.88202
#>  8:  284.0  313.6820   166.29289
#>  9:  361.0  405.6225   226.97511
#> 10:  394.0  444.1590   238.06693
#> 11:  395.0  443.8075   235.59984
#> 12:  395.5  446.0700   248.49740
#> 13:  550.0  621.3825   331.27373
#> 14:  773.0  869.9965   468.55737
#> 15:  776.0  847.2055   443.63276
#> 16: 1019.0 1127.0815   585.70567
#> 17: 1176.0 1295.5815   694.93379
#> 18: 1309.0 1425.1065   771.92785
#> 19: 1286.0 1419.3205   730.33221
#> 20: 1854.5 2104.6920  1162.62211
#> 21: 2601.0 2893.5565  1555.08839
#> 22: 2468.0 2707.2475  1391.34067
#> 23: 3181.5 3460.1925  1761.53252
#> 24: 3471.5 3855.6345  2048.62795
#> 25: 3448.0 3817.4360  2015.79716
#> 26: 3177.0 3517.5660  1841.87718
#> 27: 4178.0 4633.0810  2378.55265
#> 28: 5244.5 5892.3000  3215.09103
#> 29: 4623.0 5093.4985  2561.66819
#> 30: 5414.5 6015.4280  3288.01779
#> 31: 5430.5 6024.3250  3202.70043
#> 32: 4953.0 5416.3920  2852.72253
#> 33: 4336.0 4725.6335  2380.98457
#> 34: 5088.0 5597.0330  2888.69884
#> 35: 6128.5 6765.4540  3611.40551
#> 36: 5051.0 5576.1980  2853.95715
#> 37: 5644.0 6196.5880  3136.35164
#> 38: 5335.5 5914.8235  3119.34784
#> 39: 4610.0 5076.7565  2608.27082
#> 40: 3834.0 4273.9930  2348.47438
#> 41: 4509.0 4992.7280  2576.46414
#> 42: 5192.5 5723.1980  3019.28694
#> 43: 4200.0 4595.9340  2338.96771
#> 44: 4508.0 5043.2575  2793.96333
#> 45: 4251.0 4722.2560  2522.83371
#> 46: 3645.5 4101.6280  2319.41491
#> 47: 3013.5 3329.5855  1806.78297
#> 48: 3452.5 3873.5595  2074.41659
#> 49: 4111.0 4621.5405  2545.72707
#> 50: 3295.5 3688.0430  2083.87543
#> 51: 3469.5 4030.6275  2609.48747
#> 52: 3261.0 3976.0310  3002.01583
#> 53: 2814.5 3481.6025  2787.76354
#> 54: 2287.5 3186.6850  4008.56135
#> 55: 2641.0 4013.8345  7778.46188
#> 56: 3071.0 5812.7745 18557.27967
#> 57: 2424.5 8875.4575 95189.28769
#>     median      mean          sd
```

A summary table is returned for rapidly understanding the results and
for reporting purposes.

``` r
estimates$summary
#>                                  measure              estimate
#> 1: New confirmed cases by infection date      2123 (3 -- 9009)
#> 2:        Expected change in daily cases                Unsure
#> 3:            Effective reproduction no.     0.8 (0.2 -- 1.56)
#> 4:                        Rate of growth -0.06 (-0.24 -- 0.16)
#> 5:          Doubling/halving time (days)   -12.1 (4.3 -- -2.8)
#>     numeric_estimate
#> 1: <data.table[1x7]>
#> 2:               0.7
#> 3: <data.table[1x7]>
#> 4: <data.table[1x7]>
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
estimates <- EpiNow2::regional_epinow(reported_cases = reported_cases, 
                                      generation_time = generation_time,
                                      delays = list(incubation_period, reporting_delay),
                                      horizon = 7, samples = 2000, warmup = 500,
                                      cores = 4, chains = 4, adapt_delta = 0.95)
#> INFO [2020-09-25 09:57:37] Reporting estimates using data up to: 2020-04-11
#> INFO [2020-09-25 09:57:37] Producing estimates for: testland, realland
#> INFO [2020-09-25 09:57:37] Initialising estimates for: testland
#> DEBUG [2020-09-25 09:57:37] Running for 2000 samples (across 4 chains each with a warm up of 500 iterations each) and 72 time steps of which 7 are a forecast
#> WARN [2020-09-25 10:02:23] testland: There were 10 divergent transitions after warmup. See
#> http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#> to find out why this is a problem and how to eliminate them. - 
#> WARN [2020-09-25 10:02:23] testland: Examine the pairs() plot to diagnose sampling problems
#>  - 
#> INFO [2020-09-25 10:02:26] Completed estimates for: testland
#> INFO [2020-09-25 10:02:26] Initialising estimates for: realland
#> DEBUG [2020-09-25 10:02:26] Running for 2000 samples (across 4 chains each with a warm up of 500 iterations each) and 72 time steps of which 7 are a forecast
#> WARN [2020-09-25 10:07:14] realland: There were 1 divergent transitions after warmup. See
#> http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#> to find out why this is a problem and how to eliminate them. - 
#> WARN [2020-09-25 10:07:14] realland: Examine the pairs() plot to diagnose sampling problems
#>  - 
#> INFO [2020-09-25 10:07:17] Completed estimates for: realland
#> INFO [2020-09-25 10:07:17] Producing summary
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
#> 1: realland                     2082 (14 -- 8539)
#> 2: testland                     1944 (14 -- 8789)
#>    Expected change in daily cases Effective reproduction no.
#> 1:                         Unsure        0.78 (0.18 -- 1.47)
#> 2:                         Unsure        0.77 (0.19 -- 1.49)
#>           Rate of growth Doubling/halving time (days)
#> 1: -0.06 (-0.26 -- 0.13)            -11 (5.2 -- -2.7)
#> 2: -0.07 (-0.25 -- 0.14)            -10.4 (5 -- -2.8)
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
