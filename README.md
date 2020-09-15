
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
#> [1] 1.676779
#> 
#> $mean_sd
#> [1] 0.2056219
#> 
#> $sd
#> [1] 1.244499
#> 
#> $sd_sd
#> [1] 0.1550176
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
#> DEBUG [2020-09-15 16:35:42] Running for 2000 samples (across 4 chains each with a warm up of 500 iterations each) and 74 time steps of which 7 are a forecast

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
#>      1:       infections       infections    1 2020-02-05      1    2.014487
#>      2:       infections       infections    2 2020-02-06      1   10.974381
#>      3:       infections       infections    3 2020-02-07      1   17.427544
#>      4:       infections       infections    4 2020-02-08      1   31.419357
#>      5:       infections       infections    5 2020-02-09      1   45.886551
#>     ---                                                                     
#> 516070: prior_infections prior_infections   70 2020-04-14      1 2455.552770
#> 516071: prior_infections prior_infections   71 2020-04-15      1 2394.825740
#> 516072: prior_infections prior_infections   72 2020-04-16      1 2335.600519
#> 516073: prior_infections prior_infections   73 2020-04-17      1 2277.839967
#> 516074: prior_infections prior_infections   74 2020-04-18      1 2221.507862
#>         strat     type
#>      1:  <NA> estimate
#>      2:  <NA> estimate
#>      3:  <NA> estimate
#>      4:  <NA> estimate
#>      5:  <NA> estimate
#>     ---               
#> 516070:  <NA> forecast
#> 516071:  <NA> forecast
#> 516072:  <NA> forecast
#> 516073:  <NA> forecast
#> 516074:  <NA> forecast
#> 
#> $summarised
#>            date       variable strat     type      bottom          top
#>   1: 2020-02-22              R  <NA> estimate   0.5665344     1.271446
#>   2: 2020-02-23              R  <NA> estimate   0.6782017     1.304048
#>   3: 2020-02-24              R  <NA> estimate   0.7748911     1.313507
#>   4: 2020-02-25              R  <NA> estimate   0.8965638     1.351562
#>   5: 2020-02-26              R  <NA> estimate   0.9957927     1.370703
#>  ---                                                                  
#> 328: 2020-04-14 reported_cases  <NA> forecast 274.0000000  7004.000000
#> 329: 2020-04-15 reported_cases  <NA> forecast 208.0000000  6096.000000
#> 330: 2020-04-16 reported_cases  <NA> forecast 193.0000000  7920.000000
#> 331: 2020-04-17 reported_cases  <NA> forecast 214.0000000 10387.000000
#> 332: 2020-04-18 reported_cases  <NA> forecast 102.0000000  8569.000000
#>            lower       upper central_lower central_upper       median
#>   1:   0.7597714    1.043041     0.8120725     0.9142816    0.9095079
#>   2:   0.8626605    1.113986     0.8693239     0.9607174    0.9624416
#>   3:   0.8944272    1.113733     0.9712188     1.0543613    1.0280508
#>   4:   0.9994108    1.182810     1.0539693     1.1242116    1.0984733
#>   5:   1.0968683    1.249503     1.1468294     1.2020313    1.1802968
#>  ---                                                                 
#> 328: 695.0000000 2892.000000  1294.0000000  2068.0000000 2681.0000000
#> 329: 675.0000000 2487.000000  1092.0000000  1718.0000000 2258.5000000
#> 330: 714.0000000 2940.000000   887.0000000  1666.0000000 2640.0000000
#> 331: 806.0000000 3323.000000  1020.0000000  1870.0000000 2981.5000000
#> 332: 581.0000000 2539.000000  1026.0000000  1705.0000000 2280.5000000
#>              mean           sd
#>   1:    0.9151120 2.142730e-01
#>   2:    0.9674665 1.894689e-01
#>   3:    1.0299317 1.641049e-01
#>   4:    1.1021522 1.386968e-01
#>   5:    1.1832221 1.153774e-01
#>  ---                          
#> 328: 3504.0675000 3.030134e+03
#> 329: 3079.0185000 3.194333e+03
#> 330: 4391.7530000 1.668404e+04
#> 331: 6197.5460000 2.865345e+04
#> 332: 8622.2515000 1.126467e+05
```

Reported cases are returned separately in order to ease reporting of
forecasts and model evaluation.

``` r
estimates$estimated_reported_cases
#> $samples
#>               date sample cases  type
#>      1: 2020-02-22      1   256 gp_rt
#>      2: 2020-02-23      1   151 gp_rt
#>      3: 2020-02-24      1   292 gp_rt
#>      4: 2020-02-25      1   227 gp_rt
#>      5: 2020-02-26      1   362 gp_rt
#>     ---                              
#> 113996: 2020-04-14   2000  2878 gp_rt
#> 113997: 2020-04-15   2000  1213 gp_rt
#> 113998: 2020-04-16   2000  1598 gp_rt
#> 113999: 2020-04-17   2000  1197 gp_rt
#> 114000: 2020-04-18   2000  1642 gp_rt
#> 
#> $summarised
#>           date  type bottom   top lower upper central_lower central_upper
#>  1: 2020-02-22 gp_rt     15   347    70   196           106           153
#>  2: 2020-02-23 gp_rt     30   461   109   281           117           180
#>  3: 2020-02-24 gp_rt     32   521   115   299           165           236
#>  4: 2020-02-25 gp_rt     33   528    91   282           189           259
#>  5: 2020-02-26 gp_rt     30   479   110   282            95           159
#>  6: 2020-02-27 gp_rt     51   643   130   345           228           307
#>  7: 2020-02-28 gp_rt     38   794   147   442           277           382
#>  8: 2020-02-29 gp_rt     54   709   136   383           230           323
#>  9: 2020-03-01 gp_rt     60   860   177   472           297           406
#> 10: 2020-03-02 gp_rt     66   935   195   509           275           388
#> 11: 2020-03-03 gp_rt     62   921   181   501           283           400
#> 12: 2020-03-04 gp_rt     52   882   149   451           234           336
#> 13: 2020-03-05 gp_rt    107  1241   231   647           331           484
#> 14: 2020-03-06 gp_rt     99  1668   295   882           503           720
#> 15: 2020-03-07 gp_rt    106  1582   277   834           458           661
#> 16: 2020-03-08 gp_rt    147  2086   395  1117           530           796
#> 17: 2020-03-09 gp_rt    189  2422   472  1291           578           882
#> 18: 2020-03-10 gp_rt    176  2642   565  1460           871          1180
#> 19: 2020-03-11 gp_rt    240  2643   454  1359           589           919
#> 20: 2020-03-12 gp_rt    273  3765   875  2203           907          1388
#> 21: 2020-03-13 gp_rt    418  5484   913  2782          1380          2074
#> 22: 2020-03-14 gp_rt    334  4913  1005  2697          1369          1952
#> 23: 2020-03-15 gp_rt    442  6385  1132  3418          2357          3189
#> 24: 2020-03-16 gp_rt    480  6985  1351  3778          2142          3053
#> 25: 2020-03-17 gp_rt    591  7289  1287  3752          2295          3153
#> 26: 2020-03-18 gp_rt    592  7083  1630  4059          2073          2889
#> 27: 2020-03-19 gp_rt   1016  9393  1960  5090          2181          3266
#> 28: 2020-03-20 gp_rt    968 11308  1963  5871          3148          4509
#> 29: 2020-03-21 gp_rt    542  9548  2120  5451          2909          4131
#> 30: 2020-03-22 gp_rt   1134 11666  1783  5897          3000          4552
#> 31: 2020-03-23 gp_rt    947 11520  2627  6523          2683          4019
#> 32: 2020-03-24 gp_rt    887  9991  2086  5658          2790          4070
#> 33: 2020-03-25 gp_rt    578  8892  2048  5183          2872          4045
#> 34: 2020-03-26 gp_rt   1219 11169  2390  6121          3957          5332
#> 35: 2020-03-27 gp_rt    800 12694  2407  6873          4560          6209
#> 36: 2020-03-28 gp_rt    779 10338  1992  5492          3274          4574
#> 37: 2020-03-29 gp_rt   1011 11405  2196  6133          3425          4906
#> 38: 2020-03-30 gp_rt   1169 10812  2177  5866          2681          3994
#> 39: 2020-03-31 gp_rt    715  9434  1860  5069          2732          3934
#> 40: 2020-04-01 gp_rt    677  8026  1385  4156          2672          3638
#> 41: 2020-04-02 gp_rt    449  9502  1656  5039          2696          3948
#> 42: 2020-04-03 gp_rt    838 10917  2791  6500          3558          4840
#> 43: 2020-04-04 gp_rt    721  8404  1700  4707          2789          3818
#> 44: 2020-04-05 gp_rt    478  9059  2252  5468          2549          3703
#> 45: 2020-04-06 gp_rt    737  8774  1527  4669          2029          3136
#> 46: 2020-04-07 gp_rt    465  7463  1469  3945          2784          3690
#> 47: 2020-04-08 gp_rt    425  6418  1002  3238          1710          2487
#> 48: 2020-04-09 gp_rt    597  7784  1422  3975          1926          2867
#> 49: 2020-04-10 gp_rt    423  8531  1723  4607          2209          3184
#> 50: 2020-04-11 gp_rt    405  6772  1316  3696          1826          2701
#> 51: 2020-04-12 gp_rt    446  7881  1198  3844          1868          2816
#> 52: 2020-04-13 gp_rt    343  7729   864  3409          1954          2864
#> 53: 2020-04-14 gp_rt    274  7004   695  2892          1294          2068
#> 54: 2020-04-15 gp_rt    208  6096   675  2487          1092          1718
#> 55: 2020-04-16 gp_rt    193  7920   714  2940           887          1666
#> 56: 2020-04-17 gp_rt    214 10387   806  3323          1020          1870
#> 57: 2020-04-18 gp_rt    102  8569   581  2539          1026          1705
#>           date  type bottom   top lower upper central_lower central_upper
#>     median      mean          sd
#>  1:  172.0  194.6785    115.1521
#>  2:  234.0  261.8810    162.4129
#>  3:  259.0  292.3515    179.7871
#>  4:  256.0  290.8500    180.4993
#>  5:  233.5  267.1450    162.7493
#>  6:  306.5  348.3495    204.2193
#>  7:  377.0  441.5980    274.2764
#>  8:  330.0  380.4780    240.0123
#>  9:  406.5  471.0925    291.9033
#> 10:  432.0  500.9335    322.1205
#> 11:  428.5  499.8670    334.5180
#> 12:  403.0  484.3570    324.6151
#> 13:  571.5  669.1060    428.9929
#> 14:  789.0  917.2940    593.9049
#> 15:  745.5  863.3360    551.9338
#> 16:  987.5 1139.8855    726.5049
#> 17: 1124.5 1299.7050    843.7761
#> 18: 1212.0 1424.5870    900.6360
#> 19: 1222.5 1417.4410    900.1050
#> 20: 1792.0 2055.5425   1288.6167
#> 21: 2500.5 2934.1315   1879.1590
#> 22: 2311.5 2687.0815   1697.6603
#> 23: 3021.5 3481.0935   2151.7005
#> 24: 3334.5 3830.7875   2394.8046
#> 25: 3300.5 3873.9010   2385.5311
#> 26: 3219.5 3753.5690   2337.1012
#> 27: 4233.5 4907.4770   3076.0572
#> 28: 5237.5 6157.5350   3865.0461
#> 29: 4612.0 5290.6180   3239.4402
#> 30: 5537.0 6250.7020   3863.0255
#> 31: 5300.0 6252.7220   4142.5959
#> 32: 4880.0 5493.9115   3367.7736
#> 33: 4322.5 4927.1560   3024.3764
#> 34: 5194.0 5967.5905   3635.2171
#> 35: 6032.0 6990.7215   4446.2742
#> 36: 4892.0 5603.0500   3399.9895
#> 37: 5347.5 6140.7030   3733.7339
#> 38: 5044.5 5859.3580   3571.8049
#> 39: 4417.5 5060.1570   3172.9787
#> 40: 3644.5 4300.5695   2715.9152
#> 41: 4489.5 5230.2725   3386.8678
#> 42: 5168.5 5935.0320   3549.4847
#> 43: 4083.5 4661.5575   2833.1023
#> 44: 4262.5 4961.5780   3268.7882
#> 45: 4162.5 4816.8275   2950.4614
#> 46: 3488.5 4096.3105   2693.3750
#> 47: 2943.0 3439.4995   2210.3428
#> 48: 3478.5 4054.0400   2616.7393
#> 49: 4066.0 4684.0950   2970.8481
#> 50: 3146.5 3633.7170   2421.6056
#> 51: 3303.5 4048.7940   2898.2216
#> 52: 3131.0 3988.6185   3295.2783
#> 53: 2681.0 3504.0675   3030.1344
#> 54: 2258.5 3079.0185   3194.3329
#> 55: 2640.0 4391.7530  16684.0416
#> 56: 2981.5 6197.5460  28653.4524
#> 57: 2280.5 8622.2515 112646.6501
#>     median      mean          sd
```

A summary table is returned for rapidly understanding the results and
for reporting purposes.

``` r
estimates$summary
#>                                  measure              estimate
#> 1: New confirmed cases by infection date      2008 (5 -- 9123)
#> 2:        Expected change in daily cases                Unsure
#> 3:            Effective reproduction no.   0.77 (0.14 -- 1.49)
#> 4:                        Rate of growth -0.06 (-0.27 -- 0.14)
#> 5:          Doubling/halving time (days)   -10.7 (4.8 -- -2.6)
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
#> INFO [2020-09-15 16:40:19] Reporting estimates using data up to: 2020-04-11
#> INFO [2020-09-15 16:40:19] Producing estimates for: testland, realland
#> INFO [2020-09-15 16:40:19] Initialising estimates for: testland
#> DEBUG [2020-09-15 16:40:19] Running for 2000 samples (across 4 chains each with a warm up of 500 iterations each) and 74 time steps of which 7 are a forecast
#> WARN [2020-09-15 16:44:49] testland: There were 26 divergent transitions after warmup. See
#> http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#> to find out why this is a problem and how to eliminate them. - 
#> WARN [2020-09-15 16:44:49] testland: Examine the pairs() plot to diagnose sampling problems
#>  - 
#> INFO [2020-09-15 16:44:52] Completed estimates for: testland
#> INFO [2020-09-15 16:44:52] Initialising estimates for: realland
#> DEBUG [2020-09-15 16:44:52] Running for 2000 samples (across 4 chains each with a warm up of 500 iterations each) and 74 time steps of which 7 are a forecast
#> WARN [2020-09-15 16:50:41] realland: There were 22 divergent transitions after warmup. See
#> http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#> to find out why this is a problem and how to eliminate them. - 
#> WARN [2020-09-15 16:50:41] realland: Examine the pairs() plot to diagnose sampling problems
#>  - 
#> INFO [2020-09-15 16:50:44] Completed estimates for: realland
#> INFO [2020-09-15 16:50:44] Producing summary
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
#> 1: realland                      1994 (4 -- 9243)
#> 2: testland                      1952 (3 -- 9716)
#>    Expected change in daily cases Effective reproduction no.
#> 1:                         Unsure         0.75 (0.1 -- 1.51)
#> 2:                         Unsure        0.75 (0.18 -- 1.54)
#>           Rate of growth Doubling/halving time (days)
#> 1: -0.07 (-0.28 -- 0.15)           -9.9 (4.6 -- -2.5)
#> 2: -0.07 (-0.24 -- 0.16)           -9.5 (4.3 -- -2.8)
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
