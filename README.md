
# EpiNow2: Estimate real-time case counts and time-varying epidemiological parameters

[![R-CMD-check](https://github.com/epiforecasts/EpiNow2/workflows/R-CMD-check/badge.svg)](https://github.com/epiforecasts/EpiNow2/actions)
[![Codecov test
coverage](https://codecov.io/gh/epiforecasts/EpiNow2/branch/master/graph/badge.svg)](https://codecov.io/gh/epiforecasts/EpiNow2?branch=master)
[![DOI](https://zenodo.org/badge/272995211.svg)](https://zenodo.org/badge/latestdoi/272995211)

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
incubation period followed by a reporting delay.

``` r
reporting_delay <- EpiNow2::bootstrapped_dist_fit(rlnorm(100, log(6), 1))
## Set max allowed delay to 30 days to truncate computation
reporting_delay$max <- 30

reporting_delay
#> $mean
#> [1] 1.721957
#> 
#> $mean_sd
#> [1] 0.1534482
#> 
#> $sd
#> [1] 1.142638
#> 
#> $sd_sd
#> [1] 0.1149631
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
#>      1:       infections imputed_infections    1 2020-02-06      1    3.000
#>      2:       infections imputed_infections    2 2020-02-07      1   20.000
#>      3:       infections imputed_infections    3 2020-02-08      1   17.000
#>      4:       infections imputed_infections    4 2020-02-09      1   33.000
#>      5:       infections imputed_infections    5 2020-02-10      1   52.000
#>     ---                                                                    
#> 257069: prior_infections   prior_infections   69 2020-04-14      1 2517.820
#> 257070: prior_infections   prior_infections   70 2020-04-15      1 2455.553
#> 257071: prior_infections   prior_infections   71 2020-04-16      1 2394.826
#> 257072: prior_infections   prior_infections   72 2020-04-17      1 2335.601
#> 257073: prior_infections   prior_infections   73 2020-04-18      1 2277.840
#>         strat     type
#>      1:  <NA> estimate
#>      2:  <NA> estimate
#>      3:  <NA> estimate
#>      4:  <NA> estimate
#>      5:  <NA> estimate
#>     ---               
#> 257069:  <NA> forecast
#> 257070:  <NA> forecast
#> 257071:  <NA> forecast
#> 257072:  <NA> forecast
#> 257073:  <NA> forecast
#> 
#> $summarised
#>            date       variable strat     type      bottom         top
#>   1: 2020-02-22              R  <NA> estimate   0.6708930    1.350132
#>   2: 2020-02-23              R  <NA> estimate   0.7615030    1.348369
#>   3: 2020-02-24              R  <NA> estimate   0.8603248    1.360395
#>   4: 2020-02-25              R  <NA> estimate   0.9838832    1.395237
#>   5: 2020-02-26              R  <NA> estimate   1.0960380    1.435963
#>  ---                                                                 
#> 326: 2020-04-14 reported_cases  <NA> forecast 306.0000000 7365.000000
#> 327: 2020-04-15 reported_cases  <NA> forecast 252.0000000 6094.000000
#> 328: 2020-04-16 reported_cases  <NA> forecast 301.0000000 8189.000000
#> 329: 2020-04-17 reported_cases  <NA> forecast 299.0000000 9656.000000
#> 330: 2020-04-18 reported_cases  <NA> forecast 194.0000000 8764.000000
#>             lower       upper       median         mean           sd
#>   1:    0.8662631    1.149935    0.9932811     1.001154 2.099197e-01
#>   2:    0.9429966    1.193017    1.0492295     1.054697 1.819906e-01
#>   3:    1.0241949    1.239025    1.1128812     1.117725 1.543191e-01
#>   4:    1.0848338    1.256820    1.1869933     1.189520 1.278238e-01
#>   5:    1.1940862    1.334632    1.2697479     1.268800 1.050527e-01
#>  ---                                                                
#> 326: 1147.0000000 3313.000000 2786.5000000  3694.842000 3.628527e+03
#> 327:  834.0000000 2586.000000 2238.5000000  3334.160000 5.209645e+03
#> 328:  858.0000000 2885.000000 2586.0000000  4478.385000 1.324734e+04
#> 329:  963.0000000 3414.000000 2930.0000000  6301.453000 2.473975e+04
#> 330:  472.0000000 2410.000000 2325.5000000 10700.431000 1.170326e+05
```

Reported cases are returned separately in order to ease reporting of
forecasts and model evaluation.

``` r
estimates$estimated_reported_cases
#> $samples
#>              date sample cases  type
#>     1: 2020-02-22      1    92 gp_rt
#>     2: 2020-02-23      1   321 gp_rt
#>     3: 2020-02-24      1   318 gp_rt
#>     4: 2020-02-25      1   259 gp_rt
#>     5: 2020-02-26      1   140 gp_rt
#>    ---                              
#> 56996: 2020-04-14   1000 16745 gp_rt
#> 56997: 2020-04-15   1000  1667 gp_rt
#> 56998: 2020-04-16   1000  7687 gp_rt
#> 56999: 2020-04-17   1000 29495 gp_rt
#> 57000: 2020-04-18   1000 13009 gp_rt
#> 
#> $summarised
#>           date  type bottom   top lower upper median      mean           sd
#>  1: 2020-02-22 gp_rt     31   290    80   181  147.0   163.543     91.97664
#>  2: 2020-02-23 gp_rt     45   389   124   257  198.5   220.196    119.69485
#>  3: 2020-02-24 gp_rt     57   431    98   247  220.0   243.551    133.52454
#>  4: 2020-02-25 gp_rt     54   453    98   252  221.0   248.571    146.05288
#>  5: 2020-02-26 gp_rt     28   411    95   231  202.0   229.279    128.99355
#>  6: 2020-02-27 gp_rt     55   534   132   317  270.5   300.040    164.71779
#>  7: 2020-02-28 gp_rt     85   693   166   411  346.0   390.478    222.15777
#>  8: 2020-02-29 gp_rt     69   619   140   346  310.5   351.793    212.72217
#>  9: 2020-03-01 gp_rt     87   797   225   507  409.0   446.055    241.59149
#> 10: 2020-03-02 gp_rt     76   842   197   476  417.0   473.956    274.41425
#> 11: 2020-03-03 gp_rt     86   795   179   462  401.5   453.669    256.30181
#> 12: 2020-03-04 gp_rt     81   832   216   496  397.5   468.490    274.80312
#> 13: 2020-03-05 gp_rt    113  1177   297   686  558.5   648.331    380.50664
#> 14: 2020-03-06 gp_rt    151  1561   440  1046  825.0   910.923    557.63675
#> 15: 2020-03-07 gp_rt     94  1514   357   872  765.0   863.887    521.95433
#> 16: 2020-03-08 gp_rt    128  1941   477  1164  988.0  1114.090    636.98393
#> 17: 2020-03-09 gp_rt    250  2301   560  1356 1157.5  1304.389    770.92981
#> 18: 2020-03-10 gp_rt    209  2437   754  1593 1237.5  1404.622    875.94893
#> 19: 2020-03-11 gp_rt    307  2670   577  1478 1281.0  1427.402    826.58682
#> 20: 2020-03-12 gp_rt    223  3724   597  1918 1823.0  2077.376   1232.57327
#> 21: 2020-03-13 gp_rt    623  5091  1373  3109 2525.0  2871.435   1612.72907
#> 22: 2020-03-14 gp_rt    500  4772  1121  2768 2393.0  2677.670   1547.68083
#> 23: 2020-03-15 gp_rt    652  6243  1490  3710 3107.0  3519.338   2000.95015
#> 24: 2020-03-16 gp_rt    650  6590  1567  3765 3263.5  3748.554   2140.56659
#> 25: 2020-03-17 gp_rt    599  6908  1537  3856 3371.0  3769.574   2245.92421
#> 26: 2020-03-18 gp_rt    880  6814  1396  3698 3246.5  3688.986   2187.75775
#> 27: 2020-03-19 gp_rt    922  8721  2256  5136 4273.0  4783.850   2783.63192
#> 28: 2020-03-20 gp_rt   1359 10561  2185  5947 5158.5  5870.760   3334.42059
#> 29: 2020-03-21 gp_rt   1107  9079  2304  5540 4651.5  5094.227   2788.86572
#> 30: 2020-03-22 gp_rt    870 10911  2535  6386 5391.5  6051.941   3604.63266
#> 31: 2020-03-23 gp_rt    993 11061  2955  6671 5321.5  6101.066   3557.32889
#> 32: 2020-03-24 gp_rt   1016 10184  1966  5472 4882.5  5597.355   3322.51134
#> 33: 2020-03-25 gp_rt   1037  8910  2099  4917 4128.5  4784.335   2792.44059
#> 34: 2020-03-26 gp_rt   1189 10270  2499  5962 5107.5  5872.427   3434.07801
#> 35: 2020-03-27 gp_rt   1143 12117  2979  7152 6121.0  6914.383   4082.43468
#> 36: 2020-03-28 gp_rt   1174 10188  2534  5868 4871.0  5597.669   3332.40710
#> 37: 2020-03-29 gp_rt   1274 11135  2794  6807 5777.5  6294.260   3483.14166
#> 38: 2020-03-30 gp_rt    937 10486  2747  6338 5132.5  5875.669   3546.86785
#> 39: 2020-03-31 gp_rt    970  9106  2377  5413 4534.5  5108.442   2963.15169
#> 40: 2020-04-01 gp_rt   1038  7714  1589  4074 3726.5  4264.967   2417.20946
#> 41: 2020-04-02 gp_rt    963  9294  2476  5838 4573.5  5144.862   2956.30179
#> 42: 2020-04-03 gp_rt   1072 10303  2466  6168 5193.5  5850.861   3493.11645
#> 43: 2020-04-04 gp_rt    700  8020  2293  5242 4242.0  4691.635   2680.43065
#> 44: 2020-04-05 gp_rt    640  8555  2107  5095 4478.5  5030.609   2791.61598
#> 45: 2020-04-06 gp_rt    959  8250  2139  4971 4199.5  4697.445   2640.95002
#> 46: 2020-04-07 gp_rt    896  7508  1268  3748 3490.5  3992.851   2457.22062
#> 47: 2020-04-08 gp_rt    717  6267  1409  3471 2986.0  3391.929   2011.60178
#> 48: 2020-04-09 gp_rt    790  7209  2039  4378 3447.0  3939.647   2334.54506
#> 49: 2020-04-10 gp_rt    999  8975  1989  4759 4006.5  4741.115   3014.19324
#> 50: 2020-04-11 gp_rt    533  6795  1601  3840 3223.5  3693.759   2281.16005
#> 51: 2020-04-12 gp_rt    637  8342  1168  3838 3472.5  4208.872   2842.40885
#> 52: 2020-04-13 gp_rt    363  7605  1228  3721 3327.5  4041.100   3185.24400
#> 53: 2020-04-14 gp_rt    306  7365  1147  3313 2786.5  3694.842   3628.52654
#> 54: 2020-04-15 gp_rt    252  6094   834  2586 2238.5  3334.160   5209.64487
#> 55: 2020-04-16 gp_rt    301  8189   858  2885 2586.0  4478.385  13247.33568
#> 56: 2020-04-17 gp_rt    299  9656   963  3414 2930.0  6301.453  24739.75236
#> 57: 2020-04-18 gp_rt    194  8764   472  2410 2325.5 10700.431 117032.59709
#>           date  type bottom   top lower upper median      mean           sd
```

A summary table is returned for rapidly understanding the results and
for reporting purposes.

``` r
estimates$summary
#>                                  measure              estimate numeric_estimate
#> 1: New confirmed cases by infection date     1948 (13 -- 9023)     <data.table>
#> 2:        Expected change in daily cases                Unsure             0.71
#> 3:            Effective reproduction no.      0.8 (0.1 -- 1.5)     <data.table>
#> 4:                        Rate of growth -0.07 (-0.26 -- 0.15)     <data.table>
#> 5:          Doubling/halving time (days)   -10.2 (4.7 -- -2.6)     <data.table>
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
#> 1: realland                      2008 (1 -- 8384)
#> 2: testland                      2003 (4 -- 9382)
#>    Expected change in daily cases Effective reproduction no.
#> 1:                         Unsure           0.8 (0.1 -- 1.5)
#> 2:                         Unsure           0.8 (0.1 -- 1.5)
#>           Rate of growth Doubling/halving time (days)
#> 1: -0.07 (-0.27 -- 0.14)          -10.5 (5.1 -- -2.5)
#> 2: -0.06 (-0.25 -- 0.16)          -11.2 (4.2 -- -2.7)
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
