
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
#> [1] 1.547764
#> 
#> $mean_sd
#> [1] 0.1273535
#> 
#> $sd
#> [1] 1.015712
#> 
#> $sd_sd
#> [1] 0.1464265
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
results).

``` r
estimates <- EpiNow2::epinow(reported_cases = reported_cases, 
                             generation_time = generation_time,
                             incubation_period = incubation_period, 
                             reporting_delay = reporting_delay,
                             rt_prior = list(mean = 1, sd = 1), horizon = 7,
                             samples = 2000, warmup = 500, cores = 4,
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
#>           variable          parameter time       date sample     value strat
#>      1: infections imputed_infections    1 2020-02-13      1  0.000000  <NA>
#>      2: infections imputed_infections    2 2020-02-14      1  3.000000  <NA>
#>      3: infections imputed_infections    3 2020-02-15      1  2.000000  <NA>
#>      4: infections imputed_infections    4 2020-02-16      1  6.000000  <NA>
#>      5: infections imputed_infections    5 2020-02-17      1 15.000000  <NA>
#>     ---                                                                     
#> 644052:      gt_sd              gt_sd   NA       <NA>   1996  3.003842  <NA>
#> 644053:      gt_sd              gt_sd   NA       <NA>   1997  3.129203  <NA>
#> 644054:      gt_sd              gt_sd   NA       <NA>   1998  3.139484  <NA>
#> 644055:      gt_sd              gt_sd   NA       <NA>   1999  3.038718  <NA>
#> 644056:      gt_sd              gt_sd   NA       <NA>   2000  3.086568  <NA>
#>             type
#>      1: estimate
#>      2: estimate
#>      3: estimate
#>      4: estimate
#>      5: estimate
#>     ---         
#> 644052:     <NA>
#> 644053:     <NA>
#> 644054:     <NA>
#> 644055:     <NA>
#> 644056:     <NA>
#> 
#> $summarised
#>            date          variable strat     type       bottom          top
#>   1: 2020-02-22                 R  <NA> estimate    0.9576556     2.872523
#>   2: 2020-02-23                 R  <NA> estimate    1.1438913     2.833355
#>   3: 2020-02-24                 R  <NA> estimate    1.1814293     2.684051
#>   4: 2020-02-25                 R  <NA> estimate    1.3069173     2.622552
#>   5: 2020-02-26                 R  <NA> estimate    1.3214892     2.554116
#>  ---                                                                      
#> 374: 2020-04-04 reported_cases_rt  <NA> forecast 1770.0000000  8853.000000
#> 375: 2020-04-05 reported_cases_rt  <NA> forecast 2137.0000000 11144.000000
#> 376: 2020-04-06 reported_cases_rt  <NA> forecast 1464.0000000 10692.000000
#> 377: 2020-04-07 reported_cases_rt  <NA> forecast 1262.0000000 10346.000000
#> 378: 2020-04-08 reported_cases_rt  <NA> forecast 1051.0000000  9725.000000
#>            lower       upper      median        mean           sd
#>   1:    1.402097    2.077067    1.821874    1.952533    0.6888162
#>   2:    1.428691    2.060433    1.852965    1.950328    0.5792119
#>   3:    1.452427    2.028615    1.869351    1.941271    0.4868749
#>   4:    1.545945    2.073126    1.874078    1.929720    0.4242823
#>   5:    1.533114    2.036106    1.882422    1.919576    0.3919481
#>  ---                                                             
#> 374: 2805.000000 5486.000000 4832.000000 5295.914000 2453.0477652
#> 375: 3029.000000 6525.000000 5945.000000 6638.267500 3419.8089996
#> 376: 2547.000000 5710.000000 5264.000000 6150.691500 3627.6395159
#> 377: 2148.000000 5255.000000 4826.500000 5805.635000 3889.0593283
#> 378: 1802.000000 4632.000000 4227.000000 5202.255000 4041.1521965
```

Reported cases are returned seperately in order to ease reporting of
forecasts and model evaluation.

``` r
estimates$estimated_reported_cases
#> $samples
#>        sample       date cases
#>     1:      1 2020-02-16     1
#>     2:      1 2020-02-17     0
#>     3:      1 2020-02-18     0
#>     4:      1 2020-02-19     0
#>     5:      1 2020-02-20     1
#>    ---                        
#> 87603:   2000 2020-03-28  4217
#> 87604:   2000 2020-03-29  3947
#> 87605:   2000 2020-03-30  3523
#> 87606:   2000 2020-03-31  3292
#> 87607:   2000 2020-04-01  2822
#> 
#> $summarised
#>           date bottom  top lower upper median         mean          sd
#>  1: 2020-02-13      1    1     1     1    1.0    1.0000000   0.0000000
#>  2: 2020-02-14      0    1     1     1    1.0    0.9600000   0.3511885
#>  3: 2020-02-15      0    1     1     1    1.0    0.8608696   0.4370346
#>  4: 2020-02-16      0    1     1     1    1.0    0.8417910   0.5322389
#>  5: 2020-02-17      0    2     1     1    1.0    0.8806405   0.7425161
#>  6: 2020-02-18      0    2     0     1    1.0    1.1082569   1.0065103
#>  7: 2020-02-19      0    3     0     1    1.0    1.5795756   1.4447238
#>  8: 2020-02-20      0    6     0     2    2.0    2.8902045   2.3332489
#>  9: 2020-02-21      0   12     1     6    6.0    7.0332159   4.0430593
#> 10: 2020-02-22      6   27     8    17   16.0   16.9255000   6.8445141
#> 11: 2020-02-23     19   53    24    38   35.0   35.9740000  11.1641678
#> 12: 2020-02-24     33   87    45    67   62.0   63.6920000  17.0826826
#> 13: 2020-02-25     56  129    73   102   94.0   96.6800000  23.1708447
#> 14: 2020-02-26     85  176   105   141  129.0  131.9285000  30.2487666
#> 15: 2020-02-27    106  223   130   176  164.0  168.3105000  37.2462982
#> 16: 2020-02-28    128  267   168   222  202.0  206.2360000  43.8527746
#> 17: 2020-02-29    171  329   196   259  245.0  251.7520000  50.9631840
#> 18: 2020-03-01    217  410   266   339  302.0  307.7845000  60.2415935
#> 19: 2020-03-02    254  482   309   398  369.0  375.4110000  71.7071129
#> 20: 2020-03-03    320  585   384   488  448.0  454.5570000  83.6869494
#> 21: 2020-03-04    379  700   460   586  541.0  546.9870000 100.3508993
#> 22: 2020-03-05    464  832   568   716  642.0  653.1520000 117.7261510
#> 23: 2020-03-06    577 1024   673   856  773.0  786.0105000 141.8741186
#> 24: 2020-03-07    678 1225   785  1008  934.0  950.1255000 172.0147764
#> 25: 2020-03-08    842 1502   955  1214 1118.0 1146.8220000 207.1235172
#> 26: 2020-03-09    965 1723  1172  1478 1342.0 1371.5960000 240.6791682
#> 27: 2020-03-10   1180 2066  1372  1722 1590.0 1617.3780000 276.8191676
#> 28: 2020-03-11   1403 2392  1583  1974 1847.0 1875.6975000 314.1714928
#> 29: 2020-03-12   1580 2667  1838  2292 2111.5 2145.5340000 354.9651180
#> 30: 2020-03-13   1799 3030  2114  2619 2387.5 2429.0510000 399.6620814
#> 31: 2020-03-14   2019 3438  2381  2953 2697.0 2746.3850000 455.6761450
#> 32: 2020-03-15   2209 3810  2621  3249 3047.0 3102.3845000 515.0063246
#> 33: 2020-03-16   2598 4415  3033  3721 3450.0 3510.0340000 574.3452813
#> 34: 2020-03-17   2997 5027  3398  4187 3875.0 3946.9770000 639.5641732
#> 35: 2020-03-18   3277 5458  3734  4643 4294.0 4385.0565000 705.6369527
#> 36: 2020-03-19   3466 5913  4056  5045 4712.5 4789.4210000 767.5087730
#> 37: 2020-03-20   3827 6377  4478  5509 5058.5 5126.4465000 815.4373821
#> 38: 2020-03-21   4134 6799  4762  5818 5284.5 5384.2180000 846.1710626
#> 39: 2020-03-22   4161 6863  4788  5865 5482.5 5555.2265000 863.0342996
#> 40: 2020-03-23   4241 6971  5073  6168 5573.0 5645.3955000 859.5306339
#> 41: 2020-03-24   4235 6977  4947  6010 5582.5 5652.7635000 853.0566519
#> 42: 2020-03-25   4410 7129  4857  5889 5516.5 5597.5595000 827.8021706
#> 43: 2020-03-26   4161 6797  4762  5767 5420.0 5485.5085000 805.2587856
#> 44: 2020-03-27   4183 6709  4734  5707 5279.0 5341.2480000 773.3881639
#> 45: 2020-03-28   4002 6410  4498  5463 5125.0 5186.7035000 751.9511692
#> 46: 2020-03-29   3865 6252  4372  5344 4972.0 5026.6010000 738.8220441
#> 47: 2020-03-30   3728 6168  4301  5282 4844.0 4902.7375000 753.7862127
#> 48: 2020-03-31   3537 6123  4191  5214 4761.0 4818.7160000 813.1925771
#> 49: 2020-04-01   3197 6263  4104  5284 4693.0 4807.6260000 967.9490042
#>           date bottom  top lower upper median         mean          sd
```

A summary table is returned for rapidly understanding the results and
for reporting purposes.

``` r
estimates$summary
#>                                  measure             estimate numeric_estimate
#> 1: New confirmed cases by infection date   4692 (19 -- 17314)     <data.table>
#> 2:        Expected change in daily cases               Unsure             0.42
#> 3:            Effective reproduction no.     1.1 (0.3 -- 1.9)     <data.table>
#> 4:                        Rate of growth 0.03 (-0.17 -- 0.27)     <data.table>
#> 5:          Doubling/halving time (days)     25.2 (2.5 -- -4)     <data.table>
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
