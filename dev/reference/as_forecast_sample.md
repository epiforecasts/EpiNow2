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
#> Warning: The largest R-hat is 1.26, indicating chains have not mixed.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#r-hat
#> Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess
#> Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess
#> Warning: `extract_samples()` was deprecated in EpiNow2 1.10.0.
#> ℹ Please use the `format` argument of `get_samples()` instead.
#> ℹ The deprecated feature was likely used in the EpiNow2 package.
#>   Please report the issue at <https://github.com/epiforecasts/EpiNow2/issues>.

forecast_obj <- as_forecast_sample(fit, observations = example_confirmed)
score(forecast_obj)
#> Warning: Predictions appear to be integer-valued.
#> ! The log score uses kernel density estimation, which may not be appropriate
#>   for integer-valued forecasts.
#> ℹ See the scoringRules package for alternatives for discrete probability
#>   distributions.
#>    forecast_date       date horizon  bias      dss     crps overprediction
#>           <Date>     <Date>   <num> <num>    <num>    <num>          <num>
#> 1:    2020-04-01 2020-04-01       0 -0.08 14.11575 271.3038           0.00
#> 2:    2020-04-01 2020-04-02       1 -0.40 14.27200 346.0416           0.00
#> 3:    2020-04-01 2020-04-03       2  0.30 15.10833 534.0278         118.98
#> 4:    2020-04-01 2020-04-04       3 -0.46 14.52016 461.7966           0.00
#> 5:    2020-04-01 2020-04-05       4 -0.34 14.78641 470.1862           0.00
#> 6:    2020-04-01 2020-04-06       5 -0.02 14.61191 314.5333           0.00
#> 7:    2020-04-01 2020-04-07       6 -0.12 14.37581 286.5581           0.00
#> 8:    2020-04-01 2020-04-08       7 -0.38 14.26208 334.4703           0.00
#>    underprediction dispersion log_score       mad ae_median     se_mean
#>              <num>      <num>     <num>     <num>     <num>       <num>
#> 1:            6.62   264.6838  8.023377  985.1877     166.0  17886.3876
#> 2:          110.64   235.4016  8.077006 1108.9848     518.5 197242.5744
#> 3:            0.00   415.0478  8.490579 1661.9946     890.5 769058.8416
#> 4:          196.44   265.3566  8.406167 1012.6158     797.5 312548.0836
#> 5:          144.14   326.0462  8.526687 1328.4096     729.5  88446.7600
#> 6:            1.54   312.9933  8.167688 1241.6775      83.5    161.5441
#> 7:           10.86   275.6981  8.042263 1062.2829     168.5    819.6769
#> 8:           93.44   241.0303  8.140284 1014.8397     465.5  51089.5609
# }
```
