# Updates Forecast Horizon Based on Input Data and Target

**\[stable\]** Makes sure that a forecast is returned for the user
specified time period beyond the target date.

## Usage

``` r
update_horizon(horizon, target_date, data)
```

## Arguments

- horizon:

  Numeric, defaults to 7. Number of days into the future to forecast.

- target_date:

  Date, defaults to maximum found in the data if not specified.

- data:

  A `<data.frame>` of disease reports (confirm) by date (date).
  `confirm` must be numeric and `date` must be in date format.
  Optionally, `data` can also have a logical `accumulate` column which
  indicates whether data should be added to the next data point. This is
  useful when modelling e.g. weekly incidence data. See also the
  [`fill_missing()`](https://epiforecasts.io/EpiNow2/reference/fill_missing.md)
  function which helps add the `accumulate` column with the desired
  properties when dealing with non-daily data. If any accumulation is
  done this happens after truncation as specified by the `truncation`
  argument. If all entries of `confirm` are missing (`NA`) the returned
  estimates will represent the prior distributions.

## Value

Numeric forecast horizon adjusted for the users intention
