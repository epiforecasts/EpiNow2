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
#> Returning NA: this distribution has uncertain parameters.
#> ℹ Resolve the uncertainty first with `fix_parameters()`.
#> This message is displayed once every 8 hours.
#> Warning: The largest R-hat is 1.17, indicating chains have not mixed.
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
#> 1:    2020-04-01 2020-04-01       0 -0.14 13.70748 217.7092           0.00
#> 2:    2020-04-01 2020-04-02       1 -0.40 14.25039 439.9640           0.00
#> 3:    2020-04-01 2020-04-03       2  0.42 15.03750 525.1318         190.62
#> 4:    2020-04-01 2020-04-04       3 -0.08 14.48573 337.5262           0.00
#> 5:    2020-04-01 2020-04-05       4 -0.08 14.60299 328.1803           0.00
#> 6:    2020-04-01 2020-04-06       5 -0.14 14.47275 328.9017           0.00
#> 7:    2020-04-01 2020-04-07       6 -0.08 14.25124 264.9818           0.00
#> 8:    2020-04-01 2020-04-08       7 -0.26 13.93948 268.7900           0.00
#>    underprediction dispersion log_score       mad ae_median    se_mean
#>              <num>      <num>     <num>     <num>     <num>      <num>
#> 1:           10.10   207.6092  7.758018  765.0216     157.5    767.290
#> 2:          167.86   272.1040  8.458758 1263.1752     670.5 221389.070
#> 3:            0.00   334.5118  8.373207 1468.5153     876.0 887703.152
#> 4:           11.20   326.3262  8.255625 1363.9920     199.5  20477.610
#> 5:            4.60   323.5803  8.229767 1411.4352     106.0   1280.924
#> 6:           19.12   309.7817  8.231758 1171.2540     270.0   2531.096
#> 7:            1.62   263.3618  8.002279 1097.1240      51.0  14844.986
#> 8:           53.96   214.8300  7.964603  919.9533     346.5  20039.234
# }
```
