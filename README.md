
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

For simple deployment/development a prebuilt docker image is also
available (see documentation
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
being biased by changes in
incidence).

``` r
reporting_delay <- EpiNow2::bootstrapped_dist_fit(rlnorm(100, log(6), 1))
## Set max allowed delay to 60 days to truncate computation
reporting_delay$max <- 60

reporting_delay
#> $mean
#> [1] 1.648603
#> 
#> $mean_sd
#> [1] 0.1660936
#> 
#> $sd
#> [1] 1.171771
#> 
#> $sd_sd
#> [1] 0.1484032
#> 
#> $max
#> [1] 60
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
#>           variable          parameter time       date sample    value strat
#>      1: infections imputed_infections    1 2020-02-12      1 2.000000  <NA>
#>      2: infections imputed_infections    2 2020-02-13      1 3.000000  <NA>
#>      3: infections imputed_infections    3 2020-02-14      1 5.000000  <NA>
#>      4: infections imputed_infections    4 2020-02-15      1 0.000000  <NA>
#>      5: infections imputed_infections    5 2020-02-16      1 7.000000  <NA>
#>     ---                                                                    
#> 650053:      gt_sd              gt_sd   NA       <NA>   1996 3.122579  <NA>
#> 650054:      gt_sd              gt_sd   NA       <NA>   1997 3.116010  <NA>
#> 650055:      gt_sd              gt_sd   NA       <NA>   1998 3.101795  <NA>
#> 650056:      gt_sd              gt_sd   NA       <NA>   1999 2.894121  <NA>
#> 650057:      gt_sd              gt_sd   NA       <NA>   2000 3.051195  <NA>
#>             type
#>      1: estimate
#>      2: estimate
#>      3: estimate
#>      4: estimate
#>      5: estimate
#>     ---         
#> 650053:     <NA>
#> 650054:     <NA>
#> 650055:     <NA>
#> 650056:     <NA>
#> 650057:     <NA>
#> 
#> $summarised
#>            date          variable strat     type       bottom          top
#>   1: 2020-02-22                 R  <NA> estimate    0.9447286     2.800905
#>   2: 2020-02-23                 R  <NA> estimate    1.1419612     2.802958
#>   3: 2020-02-24                 R  <NA> estimate    1.1819344     2.617406
#>   4: 2020-02-25                 R  <NA> estimate    1.3053191     2.607255
#>   5: 2020-02-26                 R  <NA> estimate    1.2471799     2.485897
#>  ---                                                                      
#> 378: 2020-04-04 reported_cases_rt  <NA> forecast 1792.0000000  8795.000000
#> 379: 2020-04-05 reported_cases_rt  <NA> forecast 1713.0000000 10811.000000
#> 380: 2020-04-06 reported_cases_rt  <NA> forecast 1655.0000000 10985.000000
#> 381: 2020-04-07 reported_cases_rt  <NA> forecast 1094.0000000 10468.000000
#> 382: 2020-04-08 reported_cases_rt  <NA> forecast  973.0000000  9521.000000
#>            lower       upper      median        mean           sd
#>   1:    1.342480    1.978584    1.787757    1.920892 6.664423e-01
#>   2:    1.411726    2.013949    1.821209    1.922828 5.566562e-01
#>   3:    1.442635    1.996374    1.846152    1.919490 4.771371e-01
#>   4:    1.556334    2.070884    1.851773    1.914723 4.298293e-01
#>   5:    1.500422    1.992664    1.856260    1.908789 4.081294e-01
#>  ---                                                             
#> 378: 3059.000000 5678.000000 4875.000000 5383.285000 2.876527e+03
#> 379: 3500.000000 6873.000000 5827.000000 6798.311500 6.264367e+03
#> 380: 2557.000000 5737.000000 5289.500000 6508.368000 7.938064e+03
#> 381: 2143.000000 5299.000000 4810.500000 6556.782000 1.262880e+04
#> 382: 1777.000000 4567.000000 4144.500000 5861.780500 1.328673e+04
```

Reported cases are returned seperately in order to ease reporting of
forecasts and model evaluation.

``` r
estimates$estimated_reported_cases
#> $samples
#>        sample       date cases
#>     1:      1 2020-02-18     2
#>     2:      1 2020-02-19     0
#>     3:      1 2020-02-20     3
#>     4:      1 2020-02-21     2
#>     5:      1 2020-02-22     4
#>    ---                        
#> 87680:   2000 2020-03-28  5362
#> 87681:   2000 2020-03-29  4987
#> 87682:   2000 2020-03-30  4685
#> 87683:   2000 2020-03-31  4321
#> 87684:   2000 2020-04-01  3986
#> 
#> $summarised
#>           date bottom  top lower upper median         mean           sd
#>  1: 2020-02-12      1    1     1     1    1.0    1.0000000    0.0000000
#>  2: 2020-02-13      0    1     1     1    1.0    0.8888889    0.3233808
#>  3: 2020-02-14      0    1     1     1    1.0    0.8095238    0.4346724
#>  4: 2020-02-15      0    1     1     1    1.0    0.7920792    0.5147928
#>  5: 2020-02-16      0    1     1     1    1.0    0.7835052    0.7221387
#>  6: 2020-02-17      0    2     1     1    1.0    0.8326118    0.7056573
#>  7: 2020-02-18      0    2     0     1    1.0    0.9735812    0.9379822
#>  8: 2020-02-19      0    3     0     1    1.0    1.3697595    1.2348308
#>  9: 2020-02-20      0    5     0     2    2.0    2.6497297    2.0621790
#> 10: 2020-02-21      1   12     3     7    6.0    6.5002511    3.8166358
#> 11: 2020-02-22      5   26    10    18   16.0   16.4965000    6.6715997
#> 12: 2020-02-23     16   52    27    41   35.0   35.7705000   11.3956465
#> 13: 2020-02-24     35   88    50    71   62.0   63.8080000   16.8035805
#> 14: 2020-02-25     58  131    75   104   95.0   97.4350000   23.6143514
#> 15: 2020-02-26     84  180   110   146  130.0  133.1170000   30.3460995
#> 16: 2020-02-27    105  221   135   179  166.0  169.2750000   37.2348707
#> 17: 2020-02-28    144  279   171   225  205.0  209.0465000   44.1197078
#> 18: 2020-02-29    176  338   213   278  249.0  254.3480000   50.8898888
#> 19: 2020-03-01    216  409   254   327  305.0  310.9855000   59.9937490
#> 20: 2020-03-02    263  489   312   403  371.0  379.4580000   74.2808593
#> 21: 2020-03-03    314  583   378   484  447.0  456.9735000   87.3186296
#> 22: 2020-03-04    383  711   451   578  535.0  548.2460000  105.0799809
#> 23: 2020-03-05    458  853   543   692  638.0  655.8340000  123.4148494
#> 24: 2020-03-06    534 1002   675   857  772.0  789.1035000  149.2364820
#> 25: 2020-03-07    675 1224   807  1025  933.5  950.5805000  176.1329212
#> 26: 2020-03-08    831 1465   963  1222 1128.0 1147.3395000  206.0434206
#> 27: 2020-03-09    977 1729  1179  1475 1341.0 1371.5885000  238.0406433
#> 28: 2020-03-10   1211 2088  1355  1710 1587.0 1618.5135000  276.1628990
#> 29: 2020-03-11   1354 2353  1563  1971 1847.0 1879.1805000  314.1359158
#> 30: 2020-03-12   1598 2723  1785  2243 2116.5 2154.0400000  356.9061100
#> 31: 2020-03-13   1811 3064  2100  2602 2408.0 2446.5820000  398.9792532
#> 32: 2020-03-14   2039 3460  2419  2983 2731.5 2774.4365000  453.9827763
#> 33: 2020-03-15   2350 3963  2679  3320 3089.0 3137.0950000  513.3199866
#> 34: 2020-03-16   2596 4418  3127  3826 3492.0 3535.3910000  573.3988640
#> 35: 2020-03-17   2876 4867  3448  4203 3908.0 3959.6565000  623.7739001
#> 36: 2020-03-18   3397 5538  3828  4667 4331.0 4393.8750000  676.9036147
#> 37: 2020-03-19   3606 5919  4230  5141 4738.5 4801.8075000  736.4556836
#> 38: 2020-03-20   3861 6350  4468  5433 5079.0 5156.1215000  795.7547553
#> 39: 2020-03-21   4145 6801  4613  5620 5334.0 5431.6060000  838.2746083
#> 40: 2020-03-22   4228 7021  4833  5907 5519.5 5610.0430000  866.4784871
#> 41: 2020-03-23   4248 7052  4958  6062 5622.0 5700.2735000  879.8847659
#> 42: 2020-03-24   4239 7042  4834  5923 5617.0 5702.8245000  872.5393562
#> 43: 2020-03-25   4285 6987  4891  5942 5559.5 5638.7540000  849.1566705
#> 44: 2020-03-26   4213 6856  4738  5766 5452.0 5518.7070000  826.1600174
#> 45: 2020-03-27   4060 6593  4751  5751 5295.5 5359.8690000  796.2403935
#> 46: 2020-03-28   3896 6362  4552  5520 5128.5 5185.5510000  769.1221066
#> 47: 2020-03-29   3784 6280  4346  5298 4951.5 5015.2845000  752.5503070
#> 48: 2020-03-30   3741 6212  4254  5250 4824.5 4869.7730000  767.6077542
#> 49: 2020-03-31   3421 6079  4241  5311 4692.5 4774.9500000  851.1629719
#> 50: 2020-04-01   3130 6276  3817  5035 4628.5 4764.4555000 1050.2817345
#>           date bottom  top lower upper median         mean           sd
```

A summary table is returned for rapidly understanding the results and
for reporting purposes.

``` r
estimates$summary
#>                                  measure             estimate numeric_estimate
#> 1: New confirmed cases by infection date   4424 (27 -- 18797)     <data.table>
#> 2:        Expected change in daily cases               Unsure             0.42
#> 3:            Effective reproduction no.     1.1 (0.3 -- 1.9)     <data.table>
#> 4:                        Rate of growth 0.03 (-0.22 -- 0.24)     <data.table>
#> 5:          Doubling/halving time (days)   26.3 (2.9 -- -3.1)     <data.table>
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
