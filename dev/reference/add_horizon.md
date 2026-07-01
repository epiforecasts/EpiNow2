# Add missing values for future dates

Add missing values for future dates

## Usage

``` r
add_horizon(data, horizon, accumulate = 1L, obs_column = "confirm", by = NULL)
```

## Arguments

- data:

  Data frame with a `date` column. The other columns depend on the model
  that the data are to be used, e.g.
  [`estimate_infections()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_infections.md)
  or
  [`estimate_secondary()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_secondary.md).
  See the documentation there for the expected format.

- accumulate:

  The number of days to accumulate when generating posterior prediction,
  e.g. 7 for weekly accumulated forecasts. If this is not set an attempt
  will be made to detect the accumulation frequency in the data.

- obs_column:

  Character (default: "confirm"). If given, only the column specified
  here will be used for checking missingness. This is useful if using a
  data set that has multiple columns of hwich one of them corresponds to
  observations that are to be processed here.

- by:

  Character vector. Name(s) of any additional column(s) where data
  processing should be done separately for each value in the column.
  This is useful when using data representing e.g. multiple geographies.
  If NULL (default) no such grouping is done.

## Value

A data.table with missing values for future dates
