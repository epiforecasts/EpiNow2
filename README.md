
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
being biased by changes in incidence).

``` r
reporting_delay <- EpiNow2::bootstrapped_dist_fit(rlnorm(100, log(6), 1))
## Set max allowed delay to 30 days to truncate computation
reporting_delay$max <- 30

reporting_delay
#> $mean
#> [1] 1.851392
#> 
#> $mean_sd
#> [1] 0.1228672
#> 
#> $sd
#> [1] 0.9931765
#> 
#> $sd_sd
#> [1] 0.1115861
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
                             incubation_period = incubation_period, 
                             reporting_delay = reporting_delay,
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
#>           variable          parameter time       date sample     value strat
#>      1: infections imputed_infections    1 2020-02-11      1  2.000000  <NA>
#>      2: infections imputed_infections    2 2020-02-12      1 14.000000  <NA>
#>      3: infections imputed_infections    3 2020-02-13      1 18.000000  <NA>
#>      4: infections imputed_infections    4 2020-02-14      1 30.000000  <NA>
#>      5: infections imputed_infections    5 2020-02-15      1 41.000000  <NA>
#>     ---                                                                     
#> 445064:      gt_sd              gt_sd   NA       <NA>    996  3.028559  <NA>
#> 445065:      gt_sd              gt_sd   NA       <NA>    997  3.160311  <NA>
#> 445066:      gt_sd              gt_sd   NA       <NA>    998  3.010675  <NA>
#> 445067:      gt_sd              gt_sd   NA       <NA>    999  3.013735  <NA>
#> 445068:      gt_sd              gt_sd   NA       <NA>   1000  3.098191  <NA>
#>             type
#>      1: estimate
#>      2: estimate
#>      3: estimate
#>      4: estimate
#>      5: estimate
#>     ---         
#> 445064:     <NA>
#> 445065:     <NA>
#> 445066:     <NA>
#> 445067:     <NA>
#> 445068:     <NA>
#> 
#> $summarised
#>            date          variable strat     type      bottom         top
#>   1: 2020-02-22                 R  <NA> estimate    1.501892    2.667404
#>   2: 2020-02-23                 R  <NA> estimate    1.537981    2.564502
#>   3: 2020-02-24                 R  <NA> estimate    1.633183    2.544770
#>   4: 2020-02-25                 R  <NA> estimate    1.663295    2.474529
#>   5: 2020-02-26                 R  <NA> estimate    1.666235    2.398062
#>  ---                                                                    
#> 509: 2020-04-14 reported_cases_rt  <NA> forecast 1635.000000 4619.000000
#> 510: 2020-04-15 reported_cases_rt  <NA> forecast 1417.000000 3920.000000
#> 511: 2020-04-16 reported_cases_rt  <NA> forecast 1627.000000 4608.000000
#> 512: 2020-04-17 reported_cases_rt  <NA> forecast 1870.000000 5621.000000
#> 513: 2020-04-18 reported_cases_rt  <NA> forecast 1427.000000 4794.000000
#>            lower       upper      median        mean           sd
#>   1:    1.833586    2.283295    2.067344    2.092490    0.3679219
#>   2:    1.806131    2.213249    2.058731    2.080386    0.3226472
#>   3:    1.878691    2.238740    2.050003    2.068483    0.2839785
#>   4:    1.836540    2.159936    2.036259    2.056260    0.2519825
#>   5:    1.831241    2.124125    2.024778    2.043170    0.2265238
#>  ---                                                             
#> 509: 2360.000000 3493.000000 3073.000000 3215.978000  987.1810208
#> 510: 2014.000000 3013.000000 2591.500000 2686.466000  822.8030759
#> 511: 2225.000000 3481.000000 2953.500000 3073.704000  996.1365491
#> 512: 2385.000000 3769.000000 3361.000000 3593.950000 1291.9960252
#> 513: 2123.000000 3335.000000 2891.500000 3113.378000 1154.0598864
```

Reported cases are returned separately in order to ease reporting of
forecasts and model evaluation.

``` r
estimates$estimated_reported_cases
#> $samples
#>        sample       date cases
#>     1:      1 2020-02-16     1
#>     2:      1 2020-02-17     7
#>     3:      1 2020-02-18     8
#>     4:      1 2020-02-19    10
#>     5:      1 2020-02-20    10
#>    ---                        
#> 57467:   1000 2020-04-07  4116
#> 57468:   1000 2020-04-08  4025
#> 57469:   1000 2020-04-09  3879
#> 57470:   1000 2020-04-10  3856
#> 57471:   1000 2020-04-11  3740
#> 
#> $summarised
#>           date bottom  top lower upper median         mean          sd
#>  1: 2020-02-11      1    1     1     1    1.0    1.0000000   0.0000000
#>  2: 2020-02-12      0    1     1     1    1.0    0.9642857   0.3313433
#>  3: 2020-02-13      0    2     1     1    1.0    1.0243902   0.5189799
#>  4: 2020-02-14      0    2     1     1    1.0    1.1323829   0.7455926
#>  5: 2020-02-15      0    3     1     2    2.0    1.6711491   1.1425557
#>  6: 2020-02-16      0    5     1     3    3.0    2.8293692   1.7962596
#>  7: 2020-02-17      1    8     2     5    5.0    4.9109109   2.4918848
#>  8: 2020-02-18      3   13     4     8    8.0    8.0700000   3.3211640
#>  9: 2020-02-19      6   19     8    13   12.0   12.5070000   4.1568293
#> 10: 2020-02-20     10   27    12    19   18.0   18.6240000   5.5459338
#> 11: 2020-02-21     16   37    19    28   26.0   26.8690000   6.9161892
#> 12: 2020-02-22     24   50    32    42   38.0   37.7820000   8.2097427
#> 13: 2020-02-23     34   67    40    54   50.0   50.7040000  10.4918284
#> 14: 2020-02-24     46   86    58    74   68.0   67.6660000  12.2798713
#> 15: 2020-02-25     60  109    78    97   87.0   87.7240000  14.9173171
#> 16: 2020-02-26     82  139    95   119  112.0  112.5100000  17.9486505
#> 17: 2020-02-27    105  174   118   147  141.0  142.2950000  21.2733291
#> 18: 2020-02-28    135  217   157   190  179.5  179.0820000  24.8293732
#> 19: 2020-02-29    176  271   193   233  223.0  224.3280000  29.9034552
#> 20: 2020-03-01    220  337   250   298  279.0  279.3800000  35.8477986
#> 21: 2020-03-02    272  408   315   370  345.0  344.8620000  42.5188759
#> 22: 2020-03-03    341  504   379   444  423.0  424.6710000  50.4451928
#> 23: 2020-03-04    421  617   476   555  520.0  522.0840000  60.4898998
#> 24: 2020-03-05    529  760   591   683  638.0  637.1530000  72.2154461
#> 25: 2020-03-06    624  903   702   816  768.0  772.8550000  86.0532459
#> 26: 2020-03-07    763 1089   860   997  925.0  928.8020000 101.6358137
#> 27: 2020-03-08    909 1284  1007  1166 1104.0 1108.2430000 117.2386748
#> 28: 2020-03-09   1088 1523  1230  1407 1304.5 1308.4250000 134.5964880
#> 29: 2020-03-10   1300 1804  1396  1611 1524.5 1532.4770000 158.6410932
#> 30: 2020-03-11   1486 2062  1646  1890 1769.0 1783.5730000 180.5044945
#> 31: 2020-03-12   1718 2386  1859  2137 2043.5 2057.9040000 205.5849020
#> 32: 2020-03-13   1973 2719  2125  2439 2342.5 2356.5950000 232.9936244
#> 33: 2020-03-14   2262 3095  2419  2770 2664.5 2673.2330000 261.7289758
#> 34: 2020-03-15   2533 3432  2758  3137 2993.0 3010.8590000 285.3732907
#> 35: 2020-03-16   2849 3824  3051  3448 3325.0 3345.9050000 304.9690642
#> 36: 2020-03-17   3132 4170  3398  3816 3654.5 3671.1910000 326.9711036
#> 37: 2020-03-18   3432 4525  3746  4187 3969.5 3982.3540000 343.5109641
#> 38: 2020-03-19   3681 4803  3971  4411 4249.5 4269.4940000 355.1281797
#> 39: 2020-03-20   3926 5082  4249  4716 4502.5 4523.2140000 362.2878893
#> 40: 2020-03-21   4099 5263  4503  4972 4712.0 4739.9480000 361.0119651
#> 41: 2020-03-22   4310 5438  4663  5135 4891.0 4912.6080000 359.2910831
#> 42: 2020-03-23   4499 5636  4752  5233 5026.0 5045.0850000 356.5717392
#> 43: 2020-03-24   4581 5678  4855  5304 5117.5 5137.5830000 349.9878893
#> 44: 2020-03-25   4635 5713  4929  5364 5172.0 5190.7700000 341.5032292
#> 45: 2020-03-26   4668 5708  4955  5383 5202.5 5208.1860000 332.7783690
#> 46: 2020-03-27   4682 5714  4964  5371 5174.5 5188.3540000 321.2751226
#> 47: 2020-03-28   4648 5623  4932  5319 5123.5 5144.1560000 309.5141170
#> 48: 2020-03-29   4589 5545  4876  5243 5068.0 5085.7260000 302.8466923
#> 49: 2020-03-30   4493 5438  4801  5158 4999.0 5007.4210000 291.4951306
#> 50: 2020-03-31   4407 5357  4777  5148 4910.5 4918.5030000 290.9900569
#> 51: 2020-04-01   4366 5284  4661  5021 4811.5 4819.9070000 285.2051689
#> 52: 2020-04-02   4269 5152  4518  4872 4722.0 4714.3440000 275.6333895
#> 53: 2020-04-03   4216 5098  4423  4773 4614.5 4607.7680000 277.3306988
#> 54: 2020-04-04   4087 4961  4288  4644 4505.5 4507.7020000 273.1259544
#> 55: 2020-04-05   3921 4801  4217  4556 4410.0 4408.8090000 268.5323670
#> 56: 2020-04-06   3882 4740  4131  4448 4304.5 4306.5310000 263.1154248
#> 57: 2020-04-07   3795 4632  4076  4406 4210.0 4218.1890000 262.7302085
#> 58: 2020-04-08   3717 4567  3964  4290 4121.5 4119.2470000 263.0980398
#> 59: 2020-04-09   3581 4435  3850  4186 4036.0 4030.9530000 263.7660655
#> 60: 2020-04-10   3509 4394  3768  4093 3943.5 3941.1710000 268.0961904
#> 61: 2020-04-11   3461 4343  3662  3992 3853.5 3853.9540000 265.9008145
#>           date bottom  top lower upper median         mean          sd
```

A summary table is returned for rapidly understanding the results and
for reporting purposes.

``` r
estimates$summary
#>                                  measure              estimate numeric_estimate
#> 1: New confirmed cases by infection date   3048 (2499 -- 3621)     <data.table>
#> 2:        Expected change in daily cases                Unsure             0.73
#> 3:            Effective reproduction no.      0.8 (0.4 -- 1.4)     <data.table>
#> 4:                        Rate of growth -0.05 (-0.18 -- 0.12)     <data.table>
#> 5:          Doubling/halving time (days)   -12.8 (5.8 -- -3.9)     <data.table>
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
                                      incubation_period = incubation_period, 
                                      reporting_delay = reporting_delay,
                                      rt_prior = list(mean = 2, sd = 1), horizon = 7,
                                      samples = 1000, warmup = 200, cores = 4, 
                                      chains = 4, adapt_delta = 0.95, verbose = TRUE)
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
#> 1: realland                   3062 (2596 -- 3652)
#> 2: testland                   3055 (2476 -- 3545)
#>    Expected change in daily cases Effective reproduction no.
#> 1:                         Unsure           0.8 (0.4 -- 1.4)
#> 2:                         Unsure           0.8 (0.4 -- 1.4)
#>           Rate of growth Doubling/halving time (days)
#> 1: -0.04 (-0.18 -- 0.11)          -15.7 (6.4 -- -3.8)
#> 2: -0.05 (-0.19 -- 0.11)          -13.2 (6.5 -- -3.7)
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
