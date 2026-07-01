# Convert zero case counts to `NA` (missing) if the 7-day average is above a threshold.

This function aims to detect spurious zeroes by comparing the 7-day
average of the case counts to a threshold. If the 7-day average is above
the threshold, the zero case count is replaced with `NA`.

## Usage

``` r
apply_zero_threshold(data, threshold = Inf, obs_column = "confirm")
```

## Arguments

- data:

  A `<data.frame>` of disease reports (confirm) by date (date).
  `confirm` must be numeric and `date` must be in date format.
  Optionally, `data` can also have a logical `accumulate` column which
  indicates whether data should be added to the next data point. This is
  useful when modelling e.g. weekly incidence data. See also the
  [`fill_missing()`](https://epiforecasts.io/EpiNow2/dev/reference/fill_missing.md)
  function which helps add the `accumulate` column with the desired
  properties when dealing with non-daily data. If any accumulation is
  done this happens after truncation as specified by the `truncation`
  argument. If all entries of `confirm` are missing (`NA`) the returned
  estimates will represent the prior distributions.

- threshold:

  Numeric, defaults to `Inf`. Indicates if detected zero cases are
  meaningful by using a threshold number of cases based on the 7-day
  average. If the average is above this threshold at the time of a zero
  observation count then the zero is replaced with a missing (`NA`)
  count and thus ignored in the likelihood.

- obs_column:

  Character (default: "confirm"). If given, only the column specified
  here will be used for checking missingness. This is useful if using a
  data set that has multiple columns of hwich one of them corresponds to
  observations that are to be processed here.

## Value

A data.table with the zero threshold applied.
