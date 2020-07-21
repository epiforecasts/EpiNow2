
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
feedback and is under active development. It estimates the time-varying
reproduction number on cases by date of infection (using a similar
approach to that implemented in the
[`{EpiEstim}`](https://github.com/annecori/EpiEstim)). Imputed
infections are then mapped to observed data (for example cases by date
of report) via a series of uncertain delay distributions (in this
example an incubation period and a reporting delay) and a reporting
model that can include weekly periodicity. The default model uses a
non-stationary Gaussian process to estimate the time-varying
reproduction number but optionally a stationary Gaussian process may be
used (faster to estimate but reduced performance for real time
estimates) and arbitrary breakpoints can be defined. Propagating
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
#> [1] 1.527408
#> 
#> $mean_sd
#> [1] 0.1274648
#> 
#> $sd
#> [1] 1.086828
#> 
#> $sd_sd
#> [1] 0.1282467
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
#> [[1]]
#> Stan model 'estimate_infections' does not contain samples.

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
#>      1:       infections imputed_infections    1 2020-02-09      1    1.000
#>      2:       infections imputed_infections    2 2020-02-10      1   17.000
#>      3:       infections imputed_infections    3 2020-02-11      1   23.000
#>      4:       infections imputed_infections    4 2020-02-12      1   32.000
#>      5:       infections imputed_infections    5 2020-02-13      1   41.000
#>     ---                                                                    
#> 190566: prior_infections   prior_infections   66 2020-04-14      1 2714.255
#> 190567: prior_infections   prior_infections   67 2020-04-15      1 2647.130
#> 190568: prior_infections   prior_infections   68 2020-04-16      1 2581.666
#> 190569: prior_infections   prior_infections   69 2020-04-17      1 2517.820
#> 190570: prior_infections   prior_infections   70 2020-04-18      1 2455.553
#>         strat     type
#>      1:  <NA> estimate
#>      2:  <NA> estimate
#>      3:  <NA> estimate
#>      4:  <NA> estimate
#>      5:  <NA> estimate
#>     ---               
#> 190566:  <NA> forecast
#> 190567:  <NA> forecast
#> 190568:  <NA> forecast
#> 190569:  <NA> forecast
#> 190570:  <NA> forecast
#> 
#> $summarised
#>            date       variable strat     type     bottom         top
#>   1: 2020-02-22              R  <NA> estimate   1.031456    1.643384
#>   2: 2020-02-23              R  <NA> estimate   1.130377    1.640762
#>   3: 2020-02-24              R  <NA> estimate   1.244211    1.659314
#>   4: 2020-02-25              R  <NA> estimate   1.325784    1.647996
#>   5: 2020-02-26              R  <NA> estimate   1.419506    1.677153
#>  ---                                                                
#> 320: 2020-04-14 reported_cases  <NA> forecast 817.000000 5661.000000
#> 321: 2020-04-15 reported_cases  <NA> forecast 626.000000 5082.000000
#> 322: 2020-04-16 reported_cases  <NA> forecast 470.000000 6586.000000
#> 323: 2020-04-17 reported_cases  <NA> forecast 684.000000 8159.000000
#> 324: 2020-04-18 reported_cases  <NA> forecast 406.000000 6775.000000
#>            lower       upper      median        mean           sd
#>   1:    1.215955    1.453638    1.329942    1.335355 1.900852e-01
#>   2:    1.295350    1.482316    1.373187    1.380484 1.551254e-01
#>   3:    1.354248    1.505699    1.427582    1.431017 1.236163e-01
#>   4:    1.406273    1.527438    1.483581    1.485570 9.758853e-02
#>   5:    1.479097    1.585354    1.541611    1.542410 8.025973e-02
#>  ---                                                             
#> 320: 1340.000000 3153.000000 2759.000000 3257.640000 1.896348e+03
#> 321: 1190.000000 2777.000000 2371.000000 2895.214667 2.009576e+03
#> 322:  866.000000 2951.000000 2826.500000 3783.304000 4.119070e+03
#> 323: 1067.000000 3326.000000 3213.000000 4458.073333 5.356846e+03
#> 324:  994.000000 2903.000000 2572.500000 4558.794667 1.949937e+04
```

Reported cases are returned separately in order to ease reporting of
forecasts and model evaluation.

``` r
estimates$estimated_reported_cases
#> $samples
#>              date sample cases  type
#>     1: 2020-02-22      1    78 gp_rt
#>     2: 2020-02-23      1   130 gp_rt
#>     3: 2020-02-24      1   127 gp_rt
#>     4: 2020-02-25      1   178 gp_rt
#>     5: 2020-02-26      1   302 gp_rt
#>    ---                              
#> 42746: 2020-04-14    750  4928 gp_rt
#> 42747: 2020-04-15    750  3190 gp_rt
#> 42748: 2020-04-16    750  5068 gp_rt
#> 42749: 2020-04-17    750  2330 gp_rt
#> 42750: 2020-04-18    750   886 gp_rt
#> 
#> $summarised
#>           date  type bottom   top lower upper median       mean          sd
#>  1: 2020-02-22 gp_rt     29   144    61   105   81.5   88.34133    39.73774
#>  2: 2020-02-23 gp_rt     45   203    71   130  115.0  122.58400    50.69082
#>  3: 2020-02-24 gp_rt     48   238    85   157  137.0  144.65067    59.99840
#>  4: 2020-02-25 gp_rt     51   236    83   156  139.0  147.69200    63.50802
#>  5: 2020-02-26 gp_rt     51   236    92   164  143.0  149.01600    63.64729
#>  6: 2020-02-27 gp_rt     58   319   113   216  192.0  205.03733    87.61811
#>  7: 2020-02-28 gp_rt    108   488   177   312  262.0  285.08000   124.13407
#>  8: 2020-02-29 gp_rt     63   420   148   280  252.0  268.76133   117.52521
#>  9: 2020-03-01 gp_rt    128   574   185   357  320.0  348.63733   146.66506
#> 10: 2020-03-02 gp_rt    122   621   232   415  369.0  392.01467   162.76795
#> 11: 2020-03-03 gp_rt     95   664   229   446  375.0  409.15467   192.00358
#> 12: 2020-03-04 gp_rt    131   712   252   476  412.0  435.62400   192.47774
#> 13: 2020-03-05 gp_rt    162   998   370   674  550.5  601.92533   290.37822
#> 14: 2020-03-06 gp_rt    325  1381   442   881  796.0  853.70267   378.74638
#> 15: 2020-03-07 gp_rt    331  1369   548   969  811.5  860.34667   355.97659
#> 16: 2020-03-08 gp_rt    415  1831   599  1161 1050.5 1128.63867   469.11368
#> 17: 2020-03-09 gp_rt    489  2339   746  1435 1238.5 1364.30000   631.82231
#> 18: 2020-03-10 gp_rt    448  2311   960  1655 1319.5 1409.01733   609.48195
#> 19: 2020-03-11 gp_rt    514  2438   864  1639 1369.0 1484.02000   656.28348
#> 20: 2020-03-12 gp_rt    580  3237  1141  2184 1876.5 2035.95733   914.36416
#> 21: 2020-03-13 gp_rt    894  4431  1376  2784 2586.5 2753.69600  1201.86701
#> 22: 2020-03-14 gp_rt    843  4140  1726  2959 2451.0 2611.85200  1101.61667
#> 23: 2020-03-15 gp_rt   1303  5966  2005  3832 3393.5 3602.95733  1588.99724
#> 24: 2020-03-16 gp_rt   1296  6134  2492  4376 3547.5 3871.68667  1711.36168
#> 25: 2020-03-17 gp_rt   1291  6128  2243  4164 3451.5 3679.53867  1610.40241
#> 26: 2020-03-18 gp_rt   1292  5781  2042  3844 3239.0 3503.53733  1512.48106
#> 27: 2020-03-19 gp_rt   1330  7340  2900  5100 4246.5 4592.70133  1991.00905
#> 28: 2020-03-20 gp_rt   1508  9117  3775  6928 5417.5 5676.22000  2493.51734
#> 29: 2020-03-21 gp_rt   1714  8111  2959  5335 4595.5 4899.81467  2085.27548
#> 30: 2020-03-22 gp_rt   2131  9422  3575  6653 5508.0 5965.88533  2553.30391
#> 31: 2020-03-23 gp_rt   2048  9860  3110  6274 5653.0 6052.94533  2776.83507
#> 32: 2020-03-24 gp_rt   1771  8587  3028  5856 5035.0 5387.12933  2389.35927
#> 33: 2020-03-25 gp_rt   1601  7883  2703  4936 4338.0 4723.21733  2137.51451
#> 34: 2020-03-26 gp_rt   2202  9784  3291  6089 5175.0 5578.57867  2421.90847
#> 35: 2020-03-27 gp_rt   2080 10745  3178  6717 6292.5 6762.29600  2885.33647
#> 36: 2020-03-28 gp_rt   1690  8793  3554  6182 5087.0 5417.26267  2354.17336
#> 37: 2020-03-29 gp_rt   2162  9975  3267  6205 5554.0 6017.21867  2532.60931
#> 38: 2020-03-30 gp_rt   1729  9292  3837  6877 5535.0 5870.34800  2656.91216
#> 39: 2020-03-31 gp_rt   1814  7972  2641  5052 4667.0 4983.34133  2141.01872
#> 40: 2020-04-01 gp_rt   1504  7079  2103  4250 3878.0 4228.43600  1898.33112
#> 41: 2020-04-02 gp_rt   1611  8219  2829  5257 4442.0 4877.50400  2229.40326
#> 42: 2020-04-03 gp_rt   1903  9883  3737  6640 5544.0 5988.91467  2737.11117
#> 43: 2020-04-04 gp_rt   1649  7369  3014  5280 4302.5 4604.32800  1956.88003
#> 44: 2020-04-05 gp_rt   1755  8233  2940  5420 4578.5 4995.66000  2213.54995
#> 45: 2020-04-06 gp_rt   1576  8066  3046  5604 4390.5 4810.41467  2303.15856
#> 46: 2020-04-07 gp_rt   1457  6938  2269  4416 3872.5 4090.59067  1778.42554
#> 47: 2020-04-08 gp_rt   1146  5626  2091  3843 3211.0 3450.28933  1576.87726
#> 48: 2020-04-09 gp_rt   1411  6404  2399  4370 3608.0 3923.92400  1718.23288
#> 49: 2020-04-10 gp_rt   1601  7974  2731  5136 4142.5 4511.35600  2123.64436
#> 50: 2020-04-11 gp_rt   1065  5654  2213  3994 3285.5 3531.29467  1600.48263
#> 51: 2020-04-12 gp_rt   1402  7536  1951  4114 3620.5 4134.67067  2243.13357
#> 52: 2020-04-13 gp_rt    829  6930  1595  3728 3400.5 3862.61600  2110.01246
#> 53: 2020-04-14 gp_rt    817  5661  1340  3153 2759.0 3257.64000  1896.34758
#> 54: 2020-04-15 gp_rt    626  5082  1190  2777 2371.0 2895.21467  2009.57645
#> 55: 2020-04-16 gp_rt    470  6586   866  2951 2826.5 3783.30400  4119.07026
#> 56: 2020-04-17 gp_rt    684  8159  1067  3326 3213.0 4458.07333  5356.84640
#> 57: 2020-04-18 gp_rt    406  6775   994  2903 2572.5 4558.79467 19499.37256
#>           date  type bottom   top lower upper median       mean          sd
```

A summary table is returned for rapidly understanding the results and
for reporting purposes.

``` r
estimates$summary
#>                                  measure             estimate numeric_estimate
#> 1: New confirmed cases by infection date   2556 (139 -- 7447)     <data.table>
#> 2:        Expected change in daily cases               Unsure              0.7
#> 3:            Effective reproduction no.     0.8 (0.3 -- 1.5)     <data.table>
#> 4:                        Rate of growth -0.05 (-0.2 -- 0.12)     <data.table>
#> 5:          Doubling/halving time (days)  -15.2 (5.7 -- -3.5)     <data.table>
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
#> [[1]]
#> Stan model 'estimate_infections' does not contain samples.
#> 
#> [[2]]
#> Stan model 'estimate_infections' does not contain samples.
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
#> 1: realland                     2384 (64 -- 6945)
#> 2: testland                    2437 (211 -- 6520)
#>    Expected change in daily cases Effective reproduction no.
#> 1:                         Unsure           0.8 (0.3 -- 1.4)
#> 2:                         Unsure           0.8 (0.4 -- 1.4)
#>          Rate of growth Doubling/halving time (days)
#> 1: -0.05 (-0.22 -- 0.1)          -13.2 (6.7 -- -3.1)
#> 2: -0.05 (-0.18 -- 0.1)          -13.1 (7.2 -- -3.8)
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
