
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
#> [1] 1.387786
#> 
#> $mean_sd
#> [1] 0.1530942
#> 
#> $sd
#> [1] 1.166175
#> 
#> $sd_sd
#> [1] 0.124649
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
#>           variable          parameter time       date sample    value strat
#>      1: infections imputed_infections    1 2020-02-13      1 0.000000  <NA>
#>      2: infections imputed_infections    2 2020-02-14      1 3.000000  <NA>
#>      3: infections imputed_infections    3 2020-02-15      1 1.000000  <NA>
#>      4: infections imputed_infections    4 2020-02-16      1 1.000000  <NA>
#>      5: infections imputed_infections    5 2020-02-17      1 1.000000  <NA>
#>     ---                                                                    
#> 644052:      gt_sd              gt_sd   NA       <NA>   1996 3.017106  <NA>
#> 644053:      gt_sd              gt_sd   NA       <NA>   1997 3.179745  <NA>
#> 644054:      gt_sd              gt_sd   NA       <NA>   1998 3.160715  <NA>
#> 644055:      gt_sd              gt_sd   NA       <NA>   1999 2.910008  <NA>
#> 644056:      gt_sd              gt_sd   NA       <NA>   2000 3.069040  <NA>
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
#>            date          variable strat     type      bottom          top
#>   1: 2020-02-22                 R  <NA> estimate    1.146918     2.917250
#>   2: 2020-02-23                 R  <NA> estimate    1.217705     2.791583
#>   3: 2020-02-24                 R  <NA> estimate    1.382606     2.751726
#>   4: 2020-02-25                 R  <NA> estimate    1.386299     2.615556
#>   5: 2020-02-26                 R  <NA> estimate    1.468794     2.622165
#>  ---                                                                     
#> 374: 2020-04-04 reported_cases_rt  <NA> forecast 1795.000000  7783.000000
#> 375: 2020-04-05 reported_cases_rt  <NA> forecast 2005.000000 10153.000000
#> 376: 2020-04-06 reported_cases_rt  <NA> forecast 1498.000000  9583.000000
#> 377: 2020-04-07 reported_cases_rt  <NA> forecast 1531.000000 10089.000000
#> 378: 2020-04-08 reported_cases_rt  <NA> forecast 1044.000000  8701.000000
#>            lower       upper      median        mean           sd
#>   1:    1.540641    2.171026    1.932382    2.038973    0.6148886
#>   2:    1.641394    2.214338    1.968551    2.039234    0.5088655
#>   3:    1.651149    2.166174    1.980159    2.033923    0.4336207
#>   4:    1.677256    2.149057    1.989053    2.026158    0.3873871
#>   5:    1.697602    2.133558    1.988337    2.014056    0.3603880
#>  ---                                                             
#> 374: 2975.000000 5166.000000 4614.000000 5046.501500 2277.9064050
#> 375: 3546.000000 6401.000000 5595.500000 6272.373000 3334.1141166
#> 376: 2838.000000 5608.000000 5130.500000 5919.254500 4058.5241614
#> 377: 2441.000000 5179.000000 4628.000000 5679.274000 4725.3473526
#> 378: 2111.000000 4556.000000 3905.500000 5143.104500 6536.9635029
```

Reported cases are returned seperately in order to ease reporting of
forecasts and model evaluation.

``` r
estimates$estimated_reported_cases
#> $samples
#>        sample       date cases
#>     1:      1 2020-02-20     4
#>     2:      1 2020-02-21     8
#>     3:      1 2020-02-22    17
#>     4:      1 2020-02-23    38
#>     5:      1 2020-02-24    65
#>    ---                        
#> 87431:   2000 2020-03-28  4079
#> 87432:   2000 2020-03-29  4201
#> 87433:   2000 2020-03-30  3987
#> 87434:   2000 2020-03-31  3602
#> 87435:   2000 2020-04-01  3324
#> 
#> $summarised
#>           date bottom  top lower upper median         mean          sd
#>  1: 2020-02-13      1    1     1     1    1.0    1.0000000   0.0000000
#>  2: 2020-02-14      1    1     1     1    1.0    0.9090909   0.2942449
#>  3: 2020-02-15      0    1     1     1    1.0    0.8750000   0.3826178
#>  4: 2020-02-16      0    1     1     1    1.0    0.8509317   0.5260939
#>  5: 2020-02-17      0    2     1     1    1.0    0.8761468   0.7414318
#>  6: 2020-02-18      0    2     0     1    1.0    1.0375723   0.9272135
#>  7: 2020-02-19      0    3     0     1    1.0    1.4255319   1.2431599
#>  8: 2020-02-20      0    6     0     2    2.0    2.7878953   2.1326050
#>  9: 2020-02-21      1   12     3     7    6.0    6.8263924   3.8879855
#> 10: 2020-02-22      5   27    11    19   17.0   17.5780000   6.7778231
#> 11: 2020-02-23     18   53    25    39   36.0   37.2565000  11.1925129
#> 12: 2020-02-24     36   88    51    71   64.0   65.3835000  16.1778951
#> 13: 2020-02-25     65  131    76   102   95.0   96.8665000  20.7821950
#> 14: 2020-02-26     81  164   104   137  126.0  127.9685000  26.0228573
#> 15: 2020-02-27    107  206   133   174  158.5  160.4115000  31.6654941
#> 16: 2020-02-28    141  254   160   206  195.0  196.9820000  35.7033477
#> 17: 2020-02-29    175  307   202   258  239.5  243.2585000  42.4412571
#> 18: 2020-03-01    218  386   254   321  298.0  302.9380000  53.5366439
#> 19: 2020-03-02    268  468   320   402  367.0  374.9120000  65.2806636
#> 20: 2020-03-03    340  580   375   471  446.0  454.7275000  76.8491714
#> 21: 2020-03-04    408  684   461   570  535.0  542.4440000  86.9686474
#> 22: 2020-03-05    474  794   554   683  630.0  641.4790000 101.0746833
#> 23: 2020-03-06    576  944   650   803  754.0  762.7140000 116.9926102
#> 24: 2020-03-07    680 1128   796   980  907.0  920.8060000 141.2148086
#> 25: 2020-03-08    849 1399   984  1217 1106.5 1121.3515000 174.5047217
#> 26: 2020-03-09   1004 1668  1151  1427 1344.5 1357.0790000 212.5261924
#> 27: 2020-03-10   1223 2017  1437  1749 1599.5 1616.6520000 253.9112194
#> 28: 2020-03-11   1435 2354  1628  1990 1854.0 1881.5885000 292.1895284
#> 29: 2020-03-12   1637 2669  1829  2243 2112.5 2148.6050000 325.5781174
#> 30: 2020-03-13   1835 2966  2110  2555 2378.0 2419.2565000 358.0400118
#> 31: 2020-03-14   2121 3364  2308  2819 2685.5 2722.3725000 396.4321240
#> 32: 2020-03-15   2367 3789  2637  3204 3036.0 3072.1595000 446.2166814
#> 33: 2020-03-16   2677 4286  3039  3681 3443.5 3485.7730000 501.0032751
#> 34: 2020-03-17   2983 4760  3534  4257 3892.5 3938.1820000 560.6644453
#> 35: 2020-03-18   3385 5301  3913  4686 4352.5 4402.6080000 611.9708363
#> 36: 2020-03-19   3738 5866  4311  5139 4768.0 4823.5075000 660.1882604
#> 37: 2020-03-20   4061 6260  4501  5411 5095.0 5174.1895000 701.6753885
#> 38: 2020-03-21   4248 6536  4700  5648 5365.0 5435.9595000 731.2411987
#> 39: 2020-03-22   4343 6730  4932  5865 5540.5 5604.4970000 742.4349748
#> 40: 2020-03-23   4495 6870  5095  6037 5632.0 5698.3510000 745.8929192
#> 41: 2020-03-24   4547 6919  5116  6028 5667.0 5722.1630000 735.2530639
#> 42: 2020-03-25   4556 6901  5165  6031 5626.5 5686.7415000 719.9537238
#> 43: 2020-03-26   4480 6802  4992  5859 5543.0 5584.2890000 704.5686205
#> 44: 2020-03-27   4313 6547  4968  5830 5392.5 5434.4505000 681.7075863
#> 45: 2020-03-28   4173 6304  4670  5538 5214.0 5250.6655000 666.1448447
#> 46: 2020-03-29   3991 6121  4561  5401 5016.0 5061.2930000 658.9783505
#> 47: 2020-03-30   3788 5935  4306  5154 4847.0 4889.0755000 668.6955366
#> 48: 2020-03-31   3594 5899  4183  5119 4687.5 4766.4740000 730.0227270
#> 49: 2020-04-01   3240 6026  3834  4920 4613.0 4721.7535000 891.0539671
#>           date bottom  top lower upper median         mean          sd
```

A summary table is returned for rapidly understanding the results and
for reporting purposes.

``` r
estimates$summary
#>                                  measure            estimate numeric_estimate
#> 1: New confirmed cases by infection date  4508 (83 -- 17752)     <data.table>
#> 2:        Expected change in daily cases              Unsure             0.49
#> 3:            Effective reproduction no.      1 (0.3 -- 1.8)     <data.table>
#> 4:                        Rate of growth   0 (-0.21 -- 0.22)     <data.table>
#> 5:          Doubling/halving time (days) 194.5 (3.2 -- -3.3)     <data.table>
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
