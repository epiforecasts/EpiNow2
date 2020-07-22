
# EpiNow2: Estimate realtime case counts and time-varying epidemiological parameters

[![R-CMD-check](https://github.com/epiforecasts/EpiNow2/workflows/R-CMD-check/badge.svg)](https://github.com/epiforecasts/EpiNow2/actions)
[![Codecov test
coverage](https://codecov.io/gh/epiforecasts/EpiNow2/branch/master/graph/badge.svg)](https://codecov.io/gh/epiforecasts/EpiNow2?branch=master)
[![DOI](https://zenodo.org/badge/247464257.svg)](https://zenodo.org/badge/latestdoi/247464257)

*This package is under development.*

This package estimates the time-varying reproduction number, rate of
spread, and doubling time using a range of open-source tools, a novel
methodology, and current best practices. It aims to help users avoid
some of the limitations of naive implementations in a framework that is
informed by community feedback and is under active development. It
estimates the time-varying reproduction number on cases by date of
infection (using a similar approach to that implemented in the
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
#> [1] 1.699705
#> 
#> $mean_sd
#> [1] 0.1420145
#> 
#> $sd
#> [1] 1.068181
#> 
#> $sd_sd
#> [1] 0.1137165
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
#>                 variable          parameter time       date sample    value
#>      1:       infections imputed_infections    1 2020-02-07      1    1.000
#>      2:       infections imputed_infections    2 2020-02-08      1   15.000
#>      3:       infections imputed_infections    3 2020-02-09      1   23.000
#>      4:       infections imputed_infections    4 2020-02-10      1   34.000
#>      5:       infections imputed_infections    5 2020-02-11      1   51.000
#>     ---                                                                    
#> 256068: prior_infections   prior_infections   68 2020-04-14      1 2581.666
#> 256069: prior_infections   prior_infections   69 2020-04-15      1 2517.820
#> 256070: prior_infections   prior_infections   70 2020-04-16      1 2455.553
#> 256071: prior_infections   prior_infections   71 2020-04-17      1 2394.826
#> 256072: prior_infections   prior_infections   72 2020-04-18      1 2335.601
#>         strat     type
#>      1:  <NA> estimate
#>      2:  <NA> estimate
#>      3:  <NA> estimate
#>      4:  <NA> estimate
#>      5:  <NA> estimate
#>     ---               
#> 256068:  <NA> forecast
#> 256069:  <NA> forecast
#> 256070:  <NA> forecast
#> 256071:  <NA> forecast
#> 256072:  <NA> forecast
#> 
#> $summarised
#>            date       variable strat     type      bottom         top
#>   1: 2020-02-22              R  <NA> estimate   0.7785920    1.453871
#>   2: 2020-02-23              R  <NA> estimate   0.8613026    1.436633
#>   3: 2020-02-24              R  <NA> estimate   0.9641179    1.463684
#>   4: 2020-02-25              R  <NA> estimate   1.0765142    1.492637
#>   5: 2020-02-26              R  <NA> estimate   1.1929482    1.541527
#>  ---                                                                 
#> 324: 2020-04-14 reported_cases  <NA> forecast 644.0000000 6218.000000
#> 325: 2020-04-15 reported_cases  <NA> forecast 365.0000000 5798.000000
#> 326: 2020-04-16 reported_cases  <NA> forecast 294.0000000 7533.000000
#> 327: 2020-04-17 reported_cases  <NA> forecast 370.0000000 9125.000000
#> 328: 2020-04-18 reported_cases  <NA> forecast 267.0000000 7668.000000
#>            lower       upper      median        mean           sd
#>   1:   0.9500692    1.235120    1.094163    1.104249 2.080596e-01
#>   2:   1.0321570    1.272706    1.153697    1.157658 1.797030e-01
#>   3:   1.1126323    1.305289    1.217496    1.219025 1.520271e-01
#>   4:   1.2182891    1.381654    1.283089    1.287329 1.261925e-01
#>   5:   1.2979138    1.432452    1.359999    1.361040 1.047211e-01
#>  ---                                                             
#> 324: 956.0000000 3072.000000 2839.000000 3534.239000 3.045324e+03
#> 325: 815.0000000 2622.000000 2358.000000 3238.994000 4.359559e+03
#> 326: 857.0000000 2935.000000 2726.500000 4081.914000 6.367737e+03
#> 327: 974.0000000 3356.000000 3020.500000 5037.197000 1.029392e+04
#> 328: 737.0000000 2694.000000 2370.500000 7036.856000 5.827773e+04
```

Reported cases are returned separately in order to ease reporting of
forecasts and model evaluation.

``` r
estimates$estimated_reported_cases
#> $samples
#>              date sample cases  type
#>     1: 2020-02-22      1   164 gp_rt
#>     2: 2020-02-23      1   280 gp_rt
#>     3: 2020-02-24      1   272 gp_rt
#>     4: 2020-02-25      1   239 gp_rt
#>     5: 2020-02-26      1   181 gp_rt
#>    ---                              
#> 56996: 2020-04-14   1000  1555 gp_rt
#> 56997: 2020-04-15   1000   964 gp_rt
#> 56998: 2020-04-16   1000  1263 gp_rt
#> 56999: 2020-04-17   1000  3996 gp_rt
#> 57000: 2020-04-18   1000  3504 gp_rt
#> 
#> $summarised
#>           date  type bottom   top lower upper median     mean          sd
#>  1: 2020-02-22 gp_rt     28   213    65   139  120.0  130.521    63.40696
#>  2: 2020-02-23 gp_rt     54   323    95   197  167.5  183.695    93.40610
#>  3: 2020-02-24 gp_rt     42   358   100   220  191.5  209.290   109.19967
#>  4: 2020-02-25 gp_rt     49   363   100   226  192.0  211.233   112.35759
#>  5: 2020-02-26 gp_rt     45   341   122   240  186.0  203.061   101.02495
#>  6: 2020-02-27 gp_rt     49   466   121   281  245.5  271.345   139.64219
#>  7: 2020-02-28 gp_rt     75   630   176   390  335.0  366.891   189.30072
#>  8: 2020-02-29 gp_rt     64   549   159   339  286.0  321.372   182.76915
#>  9: 2020-03-01 gp_rt    111   689   197   414  359.5  395.150   208.51301
#> 10: 2020-03-02 gp_rt     87   762   212   468  385.5  439.933   243.56722
#> 11: 2020-03-03 gp_rt    108   775   234   492  405.5  447.328   247.65442
#> 12: 2020-03-04 gp_rt    110   766   220   467  394.0  432.767   235.72250
#> 13: 2020-03-05 gp_rt    159  1133   252   611  538.0  607.340   321.61622
#> 14: 2020-03-06 gp_rt    188  1573   378   867  780.0  905.221   511.70806
#> 15: 2020-03-07 gp_rt    194  1565   435   944  786.5  867.797   472.09868
#> 16: 2020-03-08 gp_rt    239  1961   588  1204 1007.0 1135.609   605.68379
#> 17: 2020-03-09 gp_rt    283  2297   716  1501 1201.5 1340.364   713.42411
#> 18: 2020-03-10 gp_rt    281  2373   595  1389 1242.0 1392.353   751.68514
#> 19: 2020-03-11 gp_rt    353  2576   761  1575 1275.0 1437.807   781.39324
#> 20: 2020-03-12 gp_rt    430  3502  1019  2184 1799.0 2008.637  1053.92920
#> 21: 2020-03-13 gp_rt    778  5139  1444  3161 2668.5 2907.981  1569.09790
#> 22: 2020-03-14 gp_rt    562  4789  1417  2995 2376.5 2660.263  1448.75739
#> 23: 2020-03-15 gp_rt    703  5983  1728  3825 3253.0 3562.777  1934.69643
#> 24: 2020-03-16 gp_rt    841  7195  1895  4117 3546.0 3975.854  2136.50338
#> 25: 2020-03-17 gp_rt   1051  6526  1708  3836 3454.5 3788.356  1928.67631
#> 26: 2020-03-18 gp_rt    771  6025  1851  3961 3206.5 3562.536  1815.04506
#> 27: 2020-03-19 gp_rt   1043  7753  2037  4617 3993.0 4538.319  2404.28424
#> 28: 2020-03-20 gp_rt   1803 10810  3268  6753 5500.0 5996.681  3154.14872
#> 29: 2020-03-21 gp_rt   1455  9067  2490  5341 4627.0 5219.820  2680.50452
#> 30: 2020-03-22 gp_rt   1331 10208  2601  5795 5200.0 5850.597  3011.26015
#> 31: 2020-03-23 gp_rt   1584 10490  2700  6031 5297.5 5954.743  3109.79386
#> 32: 2020-03-24 gp_rt   1307  9788  2450  5566 4995.0 5617.876  3003.66155
#> 33: 2020-03-25 gp_rt   1228  8072  1821  4528 4127.0 4601.332  2391.88276
#> 34: 2020-03-26 gp_rt   1077  9570  2845  6115 5239.0 5737.452  3106.26145
#> 35: 2020-03-27 gp_rt   1386 11817  3130  7199 6081.5 6737.608  3613.29595
#> 36: 2020-03-28 gp_rt   1100  9345  2622  5648 4899.5 5462.963  2777.61744
#> 37: 2020-03-29 gp_rt   1468 11196  2925  6565 5584.0 6118.370  3194.15567
#> 38: 2020-03-30 gp_rt   1035  9789  3095  6409 5302.5 5839.717  3005.26869
#> 39: 2020-03-31 gp_rt   1152  8669  2613  5498 4471.5 5123.269  2727.23182
#> 40: 2020-04-01 gp_rt    888  7309  2038  4557 3802.5 4232.029  2200.05476
#> 41: 2020-04-02 gp_rt   1462  8722  2397  5237 4511.5 4956.494  2536.30377
#> 42: 2020-04-03 gp_rt   1500 10267  2663  5955 5253.5 5759.969  2978.29972
#> 43: 2020-04-04 gp_rt   1108  7974  2194  4803 4047.5 4541.833  2487.21426
#> 44: 2020-04-05 gp_rt   1328  8826  2303  5288 4622.0 5054.548  2567.29846
#> 45: 2020-04-06 gp_rt   1106  8488  2549  5275 4365.5 4873.493  2496.29349
#> 46: 2020-04-07 gp_rt    958  6918  1807  4058 3513.0 3990.226  2155.92463
#> 47: 2020-04-08 gp_rt    874  6051  1730  3687 3021.5 3410.974  1889.47115
#> 48: 2020-04-09 gp_rt    656  6805  1743  4112 3476.5 3924.690  2197.65436
#> 49: 2020-04-10 gp_rt   1238  8511  2058  4783 4071.0 4581.578  2492.82674
#> 50: 2020-04-11 gp_rt    608  6591  1568  3749 3309.0 3764.755  2104.78723
#> 51: 2020-04-12 gp_rt    945  8076  1871  4413 3610.5 4237.102  2727.29857
#> 52: 2020-04-13 gp_rt    622  7495  1171  3416 3235.0 4027.069  2789.37967
#> 53: 2020-04-14 gp_rt    644  6218   956  3072 2839.0 3534.239  3045.32397
#> 54: 2020-04-15 gp_rt    365  5798   815  2622 2358.0 3238.994  4359.55891
#> 55: 2020-04-16 gp_rt    294  7533   857  2935 2726.5 4081.914  6367.73695
#> 56: 2020-04-17 gp_rt    370  9125   974  3356 3020.5 5037.197 10293.91995
#> 57: 2020-04-18 gp_rt    267  7668   737  2694 2370.5 7036.856 58277.73309
#>           date  type bottom   top lower upper median     mean          sd
```

A summary table is returned for rapidly understanding the results and
for reporting purposes.

``` r
estimates$summary
#>                                  measure              estimate numeric_estimate
#> 1: New confirmed cases by infection date     2164 (35 -- 8684)     <data.table>
#> 2:        Expected change in daily cases                Unsure             0.69
#> 3:            Effective reproduction no.      0.8 (0.2 -- 1.5)     <data.table>
#> 4:                        Rate of growth -0.06 (-0.23 -- 0.16)     <data.table>
#> 5:          Doubling/halving time (days)     -12.3 (4.3 -- -3)     <data.table>
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
#> 1: realland                      2036 (6 -- 8607)
#> 2: testland                      2054 (6 -- 8857)
#>    Expected change in daily cases Effective reproduction no.
#> 1:                         Unsure           0.8 (0.2 -- 1.5)
#> 2:                         Unsure           0.8 (0.1 -- 1.5)
#>           Rate of growth Doubling/halving time (days)
#> 1: -0.06 (-0.24 -- 0.18)            -10.8 (3.9 -- -3)
#> 2: -0.06 (-0.25 -- 0.15)          -11.3 (4.6 -- -2.8)
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
