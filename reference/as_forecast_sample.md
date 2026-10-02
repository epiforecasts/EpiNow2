# Convert EpiNow2 model output to a `forecast_sample` object

**\[experimental\]** Convert outputs of EpiNow2 fitting and forecasting
functions to `forecast_sample` objects via
[`scoringutils::as_forecast_sample()`](https://epiforecasts.io/scoringutils/reference/as_forecast_sample.html)
for evaluating predictive performance. Methods are provided for objects
returned by
[`epinow()`](https://epiforecasts.io/EpiNow2/reference/epinow.md),
[`estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.md),
[`forecast_secondary()`](https://epiforecasts.io/EpiNow2/reference/forecast_secondary.md),
and
[`estimate_truncation()`](https://epiforecasts.io/EpiNow2/reference/estimate_truncation.md).

These methods extract sample-level posterior predictions via
[`get_predictions()`](https://epiforecasts.io/EpiNow2/reference/get_predictions.md)
with `format = "sample"`, merge them with the supplied observations on
`date`, and pass the result to
[`scoringutils::as_forecast_sample()`](https://epiforecasts.io/scoringutils/reference/as_forecast_sample.html).

[scoringutils](https://epiforecasts.io/scoringutils/reference/scoringutils-package.html)
is an optional dependency; calling these methods without it installed
gives an informative error.

## Usage

``` r
# S3 method for class 'estimate_infections'
as_forecast_sample(data, observations, horizon = 0, ...)

# S3 method for class 'epinow'
as_forecast_sample(data, observations, horizon = 0, ...)

# S3 method for class 'forecast_secondary'
as_forecast_sample(data, observations, horizon = 0, ...)

# S3 method for class 'estimate_truncation'
as_forecast_sample(data, observations, horizon = -Inf, ...)
```

## Arguments

- data:

  Output of
  [`epinow()`](https://epiforecasts.io/EpiNow2/reference/epinow.md),
  [`estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.md),
  [`forecast_secondary()`](https://epiforecasts.io/EpiNow2/reference/forecast_secondary.md),
  or
  [`estimate_truncation()`](https://epiforecasts.io/EpiNow2/reference/estimate_truncation.md).

- observations:

  A `<data.frame>` of observed values to score against. Must contain a
  `date` column. For
  [`epinow()`](https://epiforecasts.io/EpiNow2/reference/epinow.md) and
  [`estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.md)
  objects must also contain a `confirm` column; for
  [`forecast_secondary()`](https://epiforecasts.io/EpiNow2/reference/forecast_secondary.md)
  objects a `secondary` column; for
  [`estimate_truncation()`](https://epiforecasts.io/EpiNow2/reference/estimate_truncation.md)
  objects a `confirm` column representing the latest, least-truncated
  observations.

- horizon:

  Numeric scalar lower bound on the `horizon` column of
  [`get_predictions()`](https://epiforecasts.io/EpiNow2/reference/get_predictions.md)
  output. Predictions with a `horizon` value at or above this bound are
  retained. Defaults to `0` for
  [`epinow()`](https://epiforecasts.io/EpiNow2/reference/epinow.md),
  [`estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.md)
  and
  [`forecast_secondary()`](https://epiforecasts.io/EpiNow2/reference/forecast_secondary.md)
  (i.e. forecast period only) and to `-Inf` for
  [`estimate_truncation()`](https://epiforecasts.io/EpiNow2/reference/estimate_truncation.md)
  (keep all reconstructed horizons). Pass `horizon = -Inf` to disable
  filtering.

- ...:

  Additional arguments passed to
  [`scoringutils::as_forecast_sample()`](https://epiforecasts.io/scoringutils/reference/as_forecast_sample.html).
  `forecast_unit` is set automatically from the object class
  (`forecast_date`, `date`, `horizon`, plus `dataset` for
  [`estimate_truncation()`](https://epiforecasts.io/EpiNow2/reference/estimate_truncation.md))
  and cannot be overridden.

## Value

A `forecast_sample` object as returned by
[`scoringutils::as_forecast_sample()`](https://epiforecasts.io/scoringutils/reference/as_forecast_sample.html).
Rows for which `observations` does not provide a value on the
corresponding `date` are dropped.

## See also

[`get_predictions()`](https://epiforecasts.io/EpiNow2/reference/get_predictions.md)
for the underlying sample extraction.

## Examples

``` r
# \donttest{
library(scoringutils)

# samples and calculation time have been reduced for this example
# for real analyses, use at least samples = 2000
fit <- estimate_infections(example_confirmed[1:40],
  generation_time = gt_opts(example_generation_time),
  delays = delay_opts(example_incubation_period + example_reporting_delay),
  rt = rt_opts(prior = LogNormal(mean = 2, sd = 0.2)),
  stan = stan_opts(samples = 100, warmup = 200)
)
#> Warning: There were 25 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 12. See
#> https://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded
#> Warning: There were 1 chains where the estimated Bayesian Fraction of Missing Information was low. See
#> https://mc-stan.org/misc/warnings.html#bfmi-low
#> Warning: Examine the pairs() plot to diagnose sampling problems
#> Warning: The largest R-hat is NA, indicating chains have not mixed.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#r-hat
#> Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess
#> Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess

forecast_obj <- as_forecast_sample(fit, observations = example_confirmed)
score(forecast_obj)
#> Warning: Predictions appear to be integer-valued.
#> ! The log score uses kernel density estimation, which may not be appropriate
#>   for integer-valued forecasts.
#> ℹ See the scoringRules package for alternatives for discrete probability
#>   distributions.
#>    forecast_date       date horizon  bias      dss     crps overprediction
#>           <Date>     <Date>   <num> <num>    <num>    <num>          <num>
#> 1:    2020-04-01 2020-04-01       0 -0.16 15.48619 542.3078              0
#> 2:    2020-04-01 2020-04-02       1 -0.66 15.93876 911.9622              0
#> 3:    2020-04-01 2020-04-03       2 -0.12 15.89720 686.1540              0
#> 4:    2020-04-01 2020-04-04       3 -0.48 15.78714 782.8611              0
#> 5:    2020-04-01 2020-04-05       4 -0.46 16.05229 846.0529              0
#> 6:    2020-04-01 2020-04-06       5 -0.32 15.75334 689.9109              0
#> 7:    2020-04-01 2020-04-07       6 -0.46 15.36180 624.9233              0
#> 8:    2020-04-01 2020-04-08       7 -0.42 15.09101 573.9314              0
#>    underprediction dispersion log_score      mad ae_median   se_mean
#>              <num>      <num>     <num>    <num>     <num>     <num>
#> 1:           24.82   517.4878  8.501628 1698.318     303.0  681582.3
#> 2:          441.06   470.9022  8.675020 1552.282    1332.0 2667995.6
#> 3:           17.76   668.3940  8.807259 2564.898     324.5  534010.2
#> 4:          266.16   516.7011  8.604099 1963.704    1070.5 2227287.6
#> 5:          257.96   588.0929  8.839321 2249.845    1138.0 1579722.2
#> 6:          116.46   573.4509  8.712436 2136.427     690.0 1006189.5
#> 7:          169.52   455.4033  8.534194 1708.697     810.5 1012378.1
#> 8:          164.66   409.2714  8.492926 1693.129     831.0  693289.4
# }
```
