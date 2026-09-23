# Format sample predictions

Helper function to format posterior samples into the structure expected
by
[`scoringutils::as_forecast_sample()`](https://epiforecasts.io/scoringutils/reference/as_forecast_sample.html).

## Usage

``` r
format_sample_predictions(samples, forecast_date)
```

## Arguments

- samples:

  Data.table with date, sample, and value columns

- forecast_date:

  Date when the forecast was made

## Value

Data.table with columns: forecast_date, date, horizon, sample, predicted
