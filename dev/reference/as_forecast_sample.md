# Convert EpiNow2 model output to a `forecast_sample` object

**\[experimental\]** Convert outputs of EpiNow2 fitting and forecasting
functions to `forecast_sample` objects via
[`scoringutils::as_forecast_sample()`](https://epiforecasts.io/scoringutils/reference/as_forecast_sample.html)
for evaluating predictive performance. Methods are provided for objects
returned by
[`epinow()`](https://epiforecasts.io/EpiNow2/dev/reference/epinow.md),
[`estimate_infections()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_infections.md),
[`forecast_secondary()`](https://epiforecasts.io/EpiNow2/dev/reference/forecast_secondary.md),
and
[`estimate_truncation()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_truncation.md).

These methods extract sample-level posterior predictions via
[`get_predictions()`](https://epiforecasts.io/EpiNow2/dev/reference/get_predictions.md)
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
  [`epinow()`](https://epiforecasts.io/EpiNow2/dev/reference/epinow.md),
  [`estimate_infections()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_infections.md),
  [`forecast_secondary()`](https://epiforecasts.io/EpiNow2/dev/reference/forecast_secondary.md),
  or
  [`estimate_truncation()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_truncation.md).

- observations:

  A `<data.frame>` of observed values to score against. Must contain a
  `date` column. For
  [`epinow()`](https://epiforecasts.io/EpiNow2/dev/reference/epinow.md)
  and
  [`estimate_infections()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_infections.md)
  objects must also contain a `confirm` column; for
  [`forecast_secondary()`](https://epiforecasts.io/EpiNow2/dev/reference/forecast_secondary.md)
  objects a `secondary` column; for
  [`estimate_truncation()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_truncation.md)
  objects a `confirm` column representing the latest, least-truncated
  observations.

- horizon:

  Numeric scalar lower bound on the `horizon` column of
  [`get_predictions()`](https://epiforecasts.io/EpiNow2/dev/reference/get_predictions.md)
  output. Predictions with a `horizon` value at or above this bound are
  retained. Defaults to `0` for
  [`epinow()`](https://epiforecasts.io/EpiNow2/dev/reference/epinow.md),
  [`estimate_infections()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_infections.md)
  and
  [`forecast_secondary()`](https://epiforecasts.io/EpiNow2/dev/reference/forecast_secondary.md)
  (i.e. forecast period only) and to `-Inf` for
  [`estimate_truncation()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_truncation.md)
  (keep all reconstructed horizons). Pass `horizon = -Inf` to disable
  filtering.

- ...:

  Additional arguments passed to
  [`scoringutils::as_forecast_sample()`](https://epiforecasts.io/scoringutils/reference/as_forecast_sample.html).
  `forecast_unit` is set automatically from the object class
  (`forecast_date`, `date`, `horizon`, plus `dataset` for
  [`estimate_truncation()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_truncation.md))
  and cannot be overridden.

## Value

A `forecast_sample` object as returned by
[`scoringutils::as_forecast_sample()`](https://epiforecasts.io/scoringutils/reference/as_forecast_sample.html).
Rows for which `observations` does not provide a value on the
corresponding `date` are dropped.

## See also

[`get_predictions()`](https://epiforecasts.io/EpiNow2/dev/reference/get_predictions.md)
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
#> 1:    2020-04-01 2020-04-01       0  0.01 14.06060 251.1830           0.00
#> 2:    2020-04-01 2020-04-02       1 -0.40 14.32093 437.4718           0.00
#> 3:    2020-04-01 2020-04-03       2  0.28 14.94566 433.4061          55.78
#> 4:    2020-04-01 2020-04-04       3 -0.50 14.48915 477.0277           0.00
#> 5:    2020-04-01 2020-04-05       4 -0.24 14.54468 373.1789           0.00
#> 6:    2020-04-01 2020-04-06       5 -0.32 14.33418 366.6046           0.00
#> 7:    2020-04-01 2020-04-07       6 -0.28 14.27314 309.6615           0.00
#> 8:    2020-04-01 2020-04-08       7 -0.26 14.07310 297.7760           0.00
#>    underprediction dispersion log_score       mad ae_median    se_mean
#>              <num>      <num>     <num>     <num>     <num>      <num>
#> 1:            0.00   251.1830  7.965912 1177.9257       6.0   1304.654
#> 2:          183.96   253.5118  8.304566 1093.4175     806.5 307159.808
#> 3:            0.00   377.6261  8.295182 1442.5698     452.0 576187.265
#> 4:          206.80   270.2277  8.348622 1106.0196     769.0 354203.523
#> 5:           47.00   326.1789  8.329726 1313.5836     400.5  48739.393
#> 6:           98.34   268.2646  8.201649 1044.4917     627.5  99111.632
#> 7:           25.60   284.0615  8.129084 1295.7924     243.0  22653.260
#> 8:           75.76   222.0160  7.983677  879.9231     472.0  74507.162
# }
```
