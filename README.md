
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

It estimates the time-varying reproduction number on cases
by date of infection (using a similar approach to that implemented in
the [`{EpiEstim}`](https://github.com/annecori/EpiEstim)). Imputed
infections are then mapped to observed data (for example cases by date
of report) via a series of uncertain delay distributions (in the
examples in the package documentation these are an incubation period and
a reporting delay) and a reporting model that can include weekly
periodicity. 

Uncertainty is propagated from all inputs into the final parameter estimates, helping
to mitigate spurious findings. This is handled internally. The time-varying
reproduction estimates and the uncertain generation time also give time-varying
estimates of the rate of growth.

The default model uses a non-stationary Gaussian process to
estimate the time-varying reproduction number. Optionally,
- a stationary Gaussian process may be used (faster to estimate but reduced
performance for real time estimates)
- arbitrary breakpoints can be defined
- a fixed reproduction number is also supported. When combined
with user defined breakpoints this gives the option of modelling the
time-varying reproduction number as piecewise linear

The documentation
for
[`estimate_infections`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.html)
provides examples of the different options available. 

Forecasting is supported. The time-varying reproduction number can be forecast 
forwards in time using an integration with the
[`{EpiSoon}`](https://epiforecasts.io/EpiSoon) package, and converted to
a case forecast using the renewal equation. Alternatively, the
time-varying reproduction number and cases can be forecast using a
Gaussian process. 

As a standalone tool, non-parametric back-calculation
is also supported. This uses a novel formulation based on a smoothed mean
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
incubation period followed by a reporting
delay.

``` r
reporting_delay <- EpiNow2::bootstrapped_dist_fit(rlnorm(100, log(6), 1))
## Set max allowed delay to 30 days to truncate computation
reporting_delay$max <- 30

reporting_delay
#> $mean
#> [1] 1.526777
#> 
#> $mean_sd
#> [1] 0.1187874
#> 
#> $sd
#> [1] 1.018014
#> 
#> $sd_sd
#> [1] 0.1281687
#> 
#> $max
#> [1] 30
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
#>                 variable        parameter time       date sample       value
#>      1:       infections       infections    1 2020-02-09      1    1.750261
#>      2:       infections       infections    2 2020-02-10      1   12.116275
#>      3:       infections       infections    3 2020-02-11      1   20.973398
#>      4:       infections       infections    4 2020-02-12      1   33.235345
#>      5:       infections       infections    5 2020-02-13      1   50.846664
#>     ---                                                                     
#> 254066: prior_infections prior_infections   66 2020-04-14      1 2714.255263
#> 254067: prior_infections prior_infections   67 2020-04-15      1 2647.130393
#> 254068: prior_infections prior_infections   68 2020-04-16      1 2581.665553
#> 254069: prior_infections prior_infections   69 2020-04-17      1 2517.819692
#> 254070: prior_infections prior_infections   70 2020-04-18      1 2455.552770
#>         strat     type
#>      1:  <NA> estimate
#>      2:  <NA> estimate
#>      3:  <NA> estimate
#>      4:  <NA> estimate
#>      5:  <NA> estimate
#>     ---               
#> 254066:  <NA> forecast
#> 254067:  <NA> forecast
#> 254068:  <NA> forecast
#> 254069:  <NA> forecast
#> 254070:  <NA> forecast
#> 
#> $summarised
#>            date       variable strat     type     bottom         top
#>   1: 2020-02-22              R  <NA> estimate   1.013607    1.634806
#>   2: 2020-02-23              R  <NA> estimate   1.126032    1.628409
#>   3: 2020-02-24              R  <NA> estimate   1.239503    1.632852
#>   4: 2020-02-25              R  <NA> estimate   1.302263    1.627336
#>   5: 2020-02-26              R  <NA> estimate   1.405565    1.675343
#>  ---                                                                
#> 320: 2020-04-14 reported_cases  <NA> forecast 625.000000 6101.000000
#> 321: 2020-04-15 reported_cases  <NA> forecast 575.000000 5209.000000
#> 322: 2020-04-16 reported_cases  <NA> forecast 544.000000 7177.000000
#> 323: 2020-04-17 reported_cases  <NA> forecast 652.000000 8107.000000
#> 324: 2020-04-18 reported_cases  <NA> forecast 454.000000 7528.000000
#>            lower       upper      median        mean           sd
#>   1:    1.178451    1.421593    1.339590    1.330223 1.850986e-01
#>   2:    1.259853    1.462121    1.377084    1.375055 1.527293e-01
#>   3:    1.334916    1.494135    1.422152    1.425275 1.231917e-01
#>   4:    1.402532    1.528534    1.477543    1.479545 9.865963e-02
#>   5:    1.470801    1.575832    1.530933    1.536156 8.252993e-02
#>  ---                                                             
#> 320: 1366.000000 3220.000000 2829.500000 3393.952000 2.299540e+03
#> 321: 1101.000000 2620.000000 2367.500000 2973.355000 2.680636e+03
#> 322: 1129.000000 2921.000000 2640.500000 3642.206000 3.842833e+03
#> 323: 1473.000000 3729.000000 3170.500000 5226.641000 1.567632e+04
#> 324: 1019.000000 2762.000000 2443.500000 5387.408000 2.755609e+04
```

Reported cases are returned separately in order to ease reporting of
forecasts and model evaluation.

``` r
estimates$estimated_reported_cases
#> $samples
#>              date sample cases  type
#>     1: 2020-02-22      1    57 gp_rt
#>     2: 2020-02-23      1    88 gp_rt
#>     3: 2020-02-24      1   138 gp_rt
#>     4: 2020-02-25      1   117 gp_rt
#>     5: 2020-02-26      1   141 gp_rt
#>    ---                              
#> 56996: 2020-04-14   1000  1206 gp_rt
#> 56997: 2020-04-15   1000   907 gp_rt
#> 56998: 2020-04-16   1000  1143 gp_rt
#> 56999: 2020-04-17   1000  2382 gp_rt
#> 57000: 2020-04-18   1000  1847 gp_rt
#> 
#> $summarised
#>           date  type bottom   top lower upper median     mean          sd
#>  1: 2020-02-22 gp_rt     34   153    43    88   81.0   88.586    39.65505
#>  2: 2020-02-23 gp_rt     40   203    72   134  116.0  126.097    55.10023
#>  3: 2020-02-24 gp_rt     45   244    88   162  139.5  150.251    66.05259
#>  4: 2020-02-25 gp_rt     54   264    89   172  146.0  155.772    70.09574
#>  5: 2020-02-26 gp_rt     43   253    83   161  140.0  152.781    69.60690
#>    ---    
#> 53: 2020-04-14 gp_rt    625  6101  1366  3220 2829.5 3393.952  2299.53973
#> 54: 2020-04-15 gp_rt    575  5209  1101  2620 2367.5 2973.355  2680.63628
#> 55: 2020-04-16 gp_rt    544  7177  1129  2921 2640.5 3642.206  3842.83278
#> 56: 2020-04-17 gp_rt    652  8107  1473  3729 3170.5 5226.641 15676.32300
#> 57: 2020-04-18 gp_rt    454  7528  1019  2762 2443.5 5387.408 27556.09256
```

A summary table is returned for rapidly understanding the results and
for reporting purposes.

``` r
estimates$summary
#>                                  measure             estimate  numeric_estimate
#> 1: New confirmed cases by infection date   2407 (121 -- 7474) <data.table[1x5]>
#> 2:        Expected change in daily cases               Unsure               0.7
#> 3:            Effective reproduction no.     0.8 (0.3 -- 1.4) <data.table[1x5]>
#> 4:                        Rate of growth -0.05 (-0.2 -- 0.11) <data.table[1x5]>
#> 5:          Doubling/halving time (days)  -13.5 (6.2 -- -3.5) <data.table[1x3]>
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
#> 1: realland                     2402 (97 -- 7486)
#> 2: testland                    2474 (114 -- 7527)
#>    Expected change in daily cases Effective reproduction no.
#> 1:                         Unsure           0.8 (0.3 -- 1.4)
#> 2:                         Unsure           0.8 (0.3 -- 1.4)
#>           Rate of growth Doubling/halving time (days)
#> 1: -0.05 (-0.21 -- 0.12)          -13.4 (5.7 -- -3.3)
#> 2: -0.05 (-0.23 -- 0.11)            -14.3 (6.4 -- -3)
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
