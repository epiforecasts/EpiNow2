
# EpiNow2: Estimate realtime case counts and time-varying epidemiological parameters

[![R-CMD-check](https://github.com/epiforecasts/EpiNow2/workflows/R-CMD-check/badge.svg)](https://github.com/epiforecasts/EpiNow2/actions)
[![Codecov test
coverage](https://codecov.io/gh/epiforecasts/EpiNow2/branch/master/graph/badge.svg)](https://codecov.io/gh/epiforecasts/EpiNow2?branch=master)
[![DOI](https://zenodo.org/badge/247464257.svg)](https://zenodo.org/badge/latestdoi/247464257)

*This package is under development with breaking changes likely.*

This package estimates the time-varying reproduction number, rate of
spread, and doubling time using a range of open-source tools and current
best practices. It aims to help users avoid some of the limitations of
naive implementations in a framework that is informed by community
feedback and is under active development. It assumes that only limited
data is available on cases by date of onset and instead uses cases by
date of report. These are then imputed to case counts by date of
infection using an uncertain reporting delay and incubation period via a
Gaussian process based method. Right truncation of cases is dealt with
internally by `{EpiNow2}`, as is propogating uncertainty from all inputs
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
A working implementation for COVID-19 can be found
[here](https://github.com/epiforecasts/covid-global/blob/master/update_nowcasts.R).

### Reporting delays, incubation period and generation time

Distributions can either be fitted using package functionality or
determined elsewhere and then defined with uncertainty for use in
`{EpiNow2}`. When data is supplied a subsampled bootstrapped lognormal
will be fit (to account for uncertainty in the observed data without
being biased by changes in incidence).

``` r
reporting_delay <- EpiNow2::bootstrapped_dist_fit(rlnorm(100, log(6), 1))
## Set max allowed delay to 60 days to truncate computation
reporting_delay$max <- 60

reporting_delay
#> $mean
#> [1] 1.662454
#> 
#> $mean_sd
#> [1] 0.1328819
#> 
#> $sd
#> [1] 0.9682874
#> 
#> $sd_sd
#> [1] 0.09942634
#> 
#> $max
#> [1] 60
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
above. An additional forecasting mdoule is supported via `EpiSoon` and
companion packages (see documentation for an example).

Load example case data from `{EpiNow2}`.

``` r
reported_cases <- EpiNow2::example_confirmed[1:40]

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
                             incubation_period = incubation_period, 
                             reporting_delay = reporting_delay,
                             horizon = 7, samples = 1000, warmup = 250, cores = 4,
                             chains = 4, verbose = TRUE)

names(estimates)
#> [1] "estimates"                "estimated_reported_cases"
#> [3] "summary"                  "plots"
```

Both summary measures and posterior samples are returned for all
parameters in an easily explored format.

``` r
estimates$estimates
#> $samples
#>           variable          parameter time       date sample    value strat
#>      1: infections imputed_infections    1 2020-02-12      1 1.000000  <NA>
#>      2: infections imputed_infections    2 2020-02-13      1 0.000000  <NA>
#>      3: infections imputed_infections    3 2020-02-14      1 1.000000  <NA>
#>      4: infections imputed_infections    4 2020-02-15      1 3.000000  <NA>
#>      5: infections imputed_infections    5 2020-02-16      1 3.000000  <NA>
#>     ---                                                                    
#> 325053:      gt_sd              gt_sd   NA       <NA>    996 3.001101  <NA>
#> 325054:      gt_sd              gt_sd   NA       <NA>    997 3.182484  <NA>
#> 325055:      gt_sd              gt_sd   NA       <NA>    998 3.038965  <NA>
#> 325056:      gt_sd              gt_sd   NA       <NA>    999 3.023792  <NA>
#> 325057:      gt_sd              gt_sd   NA       <NA>   1000 2.879233  <NA>
#>             type
#>      1: estimate
#>      2: estimate
#>      3: estimate
#>      4: estimate
#>      5: estimate
#>     ---         
#> 325053:     <NA>
#> 325054:     <NA>
#> 325055:     <NA>
#> 325056:     <NA>
#> 325057:     <NA>
#> 
#> $summarised
#>            date          variable strat     type      bottom          top
#>   1: 2020-02-22                 R  <NA> estimate    1.156754     2.797370
#>   2: 2020-02-23                 R  <NA> estimate    1.296550     2.729608
#>   3: 2020-02-24                 R  <NA> estimate    1.311231     2.543682
#>   4: 2020-02-25                 R  <NA> estimate    1.446166     2.496869
#>   5: 2020-02-26                 R  <NA> estimate    1.522439     2.463755
#>  ---                                                                     
#> 378: 2020-04-04 reported_cases_rt  <NA> forecast 2135.000000  7441.000000
#> 379: 2020-04-05 reported_cases_rt  <NA> forecast 2520.000000 10170.000000
#> 380: 2020-04-06 reported_cases_rt  <NA> forecast 1838.000000  9102.000000
#> 381: 2020-04-07 reported_cases_rt  <NA> forecast 1499.000000  9120.000000
#> 382: 2020-04-08 reported_cases_rt  <NA> forecast 1277.000000  7783.000000
#>            lower       upper      median        mean           sd
#>   1:    1.497166    2.098036    1.962373    2.036219    0.5589665
#>   2:    1.674100    2.202104    1.981915    2.027192    0.4596344
#>   3:    1.652546    2.117968    1.989787    2.015804    0.3826886
#>   4:    1.706090    2.120343    1.994046    2.002998    0.3302092
#>   5:    1.798144    2.179997    1.981706    1.989194    0.3001369
#>  ---                                                             
#> 378: 2860.000000 4943.000000 4414.000000 4815.370000 1973.2458880
#> 379: 3396.000000 6118.000000 5564.500000 6156.496000 2939.0876524
#> 380: 2807.000000 5536.000000 4991.500000 5661.789000 2963.7546278
#> 381: 2807.000000 5442.000000 4597.500000 5348.858000 3180.6970172
#> 382: 2152.000000 4284.000000 3767.000000 4510.149000 3300.7763125
```

Reported cases are returned seperately in order to ease reporting of
forecasts and model evaluation.

``` r
estimates$estimated_reported_cases
#> $samples
#>        sample       date cases
#>     1:      1 2020-02-19     1
#>     2:      1 2020-02-20     4
#>     3:      1 2020-02-21     4
#>     4:      1 2020-02-22    16
#>     5:      1 2020-02-23    29
#>    ---                        
#> 44105:   1000 2020-03-28  4558
#> 44106:   1000 2020-03-29  4158
#> 44107:   1000 2020-03-30  4020
#> 44108:   1000 2020-03-31  3906
#> 44109:   1000 2020-04-01  3640
#> 
#> $summarised
#>           date bottom  top lower upper median         mean          sd
#>  1: 2020-02-12      1    1     1     1    1.0    1.0000000          NA
#>  2: 2020-02-13      0    1     1     1    1.0    0.8888889   0.3333333
#>  3: 2020-02-14      0    1     1     1    1.0    0.7931034   0.4122508
#>  4: 2020-02-15      0    1     1     1    1.0    0.7662338   0.5103173
#>  5: 2020-02-16      0    1     1     1    1.0    0.7582418   0.5916670
#>  6: 2020-02-17      0    2     1     1    1.0    0.9317585   0.7473210
#>  7: 2020-02-18      0    2     0     1    1.0    1.1223140   0.9874618
#>  8: 2020-02-19      0    4     1     2    2.0    1.8586698   1.4549034
#>  9: 2020-02-20      0    7     1     3    3.0    3.6581892   2.3681422
#> 10: 2020-02-21      2   14     4     8    8.0    8.0440000   3.8914274
#> 11: 2020-02-22      6   26    10    18   17.0   17.2440000   6.3678107
#> 12: 2020-02-23     16   48    23    36   33.0   33.5800000   9.9833495
#> 13: 2020-02-24     33   81    47    66   58.0   58.9160000  15.0423807
#> 14: 2020-02-25     54  122    71    98   91.0   92.3830000  20.9684999
#> 15: 2020-02-26     88  173   103   139  131.0  133.1000000  27.0934319
#> 16: 2020-02-27    124  231   143   186  174.0  177.0050000  33.9796349
#> 17: 2020-02-28    154  277   197   246  219.0  221.6140000  39.2536094
#> 18: 2020-02-29    199  337   226   281  263.0  266.3560000  44.3661540
#> 19: 2020-03-01    237  394   267   331  309.0  313.7930000  49.4660797
#> 20: 2020-03-02    275  453   328   401  361.5  363.7840000  56.3115649
#> 21: 2020-03-03    316  521   353   438  421.0  425.3980000  65.0757286
#> 22: 2020-03-04    387  630   437   536  495.0  503.3630000  77.2578022
#> 23: 2020-03-05    470  754   516   638  606.0  612.2960000  93.1009246
#> 24: 2020-03-06    565  923   660   802  750.0  755.4340000 114.9814523
#> 25: 2020-03-07    711 1156   791   973  928.0  938.3250000 141.8219759
#> 26: 2020-03-08    886 1432   991  1215 1140.0 1154.9610000 174.2888869
#> 27: 2020-03-09   1072 1710  1227  1489 1380.5 1397.8530000 202.9293565
#> 28: 2020-03-10   1281 2021  1460  1765 1628.5 1653.2110000 235.2190394
#> 29: 2020-03-11   1498 2310  1707  2050 1897.5 1912.8840000 262.9577177
#> 30: 2020-03-12   1745 2660  1901  2289 2152.0 2173.4500000 292.7500437
#> 31: 2020-03-13   1893 2908  2200  2616 2416.0 2439.8840000 322.9941699
#> 32: 2020-03-14   2184 3334  2435  2893 2705.0 2731.2150000 357.4303418
#> 33: 2020-03-15   2445 3733  2692  3195 3014.0 3052.4130000 402.4753568
#> 34: 2020-03-16   2716 4138  3071  3650 3383.0 3423.0520000 443.2885539
#> 35: 2020-03-17   3050 4634  3320  3979 3817.5 3844.1300000 498.1829457
#> 36: 2020-03-18   3492 5258  3848  4555 4270.0 4300.7580000 555.3512766
#> 37: 2020-03-19   3726 5731  4294  5087 4710.0 4764.7250000 618.6138217
#> 38: 2020-03-20   4155 6326  4713  5557 5156.0 5200.7490000 671.1446484
#> 39: 2020-03-21   4535 6757  5047  5920 5499.0 5549.2820000 693.4248095
#> 40: 2020-03-22   4577 6823  5213  6089 5720.0 5777.6180000 699.3120487
#> 41: 2020-03-23   4723 6911  5298  6163 5808.0 5877.2530000 689.3284781
#> 42: 2020-03-24   4853 6995  5253  6111 5798.5 5858.4620000 671.5710523
#> 43: 2020-03-25   4775 6827  5227  6081 5693.5 5731.6540000 646.7182081
#> 44: 2020-03-26   4531 6548  5002  5834 5508.0 5547.6810000 621.3255046
#> 45: 2020-03-27   4324 6285  4858  5646 5312.5 5337.1550000 599.2072198
#> 46: 2020-03-28   4084 5988  4627  5378 5104.5 5126.4890000 578.8177484
#> 47: 2020-03-29   4043 5931  4429  5186 4917.5 4955.0100000 587.1533112
#> 48: 2020-03-30   3913 5879  4362  5134 4790.0 4838.8380000 633.0311669
#> 49: 2020-03-31   3728 6027  4234  5111 4712.5 4793.4010000 741.4541648
#> 50: 2020-04-01   3338 6115  3989  5070 4692.0 4814.8010000 932.3111130
#>           date bottom  top lower upper median         mean          sd
```

A summary table is returned for rapidly understanding the results and
for reporting purposes.

``` r
estimates$summary
#>                                  measure              estimate numeric_estimate
#> 1: New confirmed cases by infection date    4366 (51 -- 16522)     <data.table>
#> 2:        Expected change in daily cases                Unsure             0.56
#> 3:            Effective reproduction no.      0.9 (0.4 -- 1.6)     <data.table>
#> 4:                        Rate of growth -0.02 (-0.19 -- 0.18)     <data.table>
#> 5:          Doubling/halving time (days)   -38.8 (3.9 -- -3.6)     <data.table>
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
the `{future}` package) can be used to run regions in parallel.

``` r
## future::plan("multisession")
estimates <- EpiNow2::regional_epinow(reported_cases = reported_cases, 
                                      generation_time = generation_time,
                                      incubation_period = incubation_period, 
                                      reporting_delay = reporting_delay,
                                      rt_prior = list(mean = 1, sd = 1), horizon = 7,
                                      samples = 2000, warmup = 500, cores = 4, 
                                      chains = 4, verbose = TRUE)
```

If the results have been saved to a folder (using `target_folder`) then
the `regional_summary` function can be used to produce summary output
(*work in progress*).

An example of the summary output can be seen
[here](https://github.com/epiforecasts/covid-uk/tree/master/nowcast/regional-summary).

### Reporting templates

Rmarkdown templates are provided in the package (`templates`) for
semi-automated reporting of the estimates. These are currently
undocumented but an example integration can be seen
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
