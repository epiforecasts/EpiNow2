# Format quantile predictions

Helper function to format posterior samples into quantiles in the
structure expected by
[`scoringutils::as_forecast_quantile()`](https://epiforecasts.io/scoringutils/reference/as_forecast_quantile.html).

## Usage

``` r
format_quantile_predictions(samples, quantiles, forecast_date)
```

## Arguments

- samples:

  Data.table with date and value columns

- quantiles:

  Numeric vector of quantile levels

- forecast_date:

  Date when the forecast was made

## Value

Data.table with columns: forecast_date, date, horizon, quantile_level,
predicted
