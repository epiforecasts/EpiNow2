# Filter leading zeros from a data set.

Filter leading zeros from a data set.

## Usage

``` r
filter_leading_zeros(data, obs_column = "confirm", by = NULL)
```

## Arguments

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

A data.table with leading zeros removed.

## Examples

``` r
cases <- data.frame(
  date = as.Date("2020-01-01") + 0:10,
  confirm = c(0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
)
filter_leading_zeros(cases)
#>          date confirm
#>        <Date>   <num>
#> 1: 2020-01-03       1
#> 2: 2020-01-04       2
#> 3: 2020-01-05       3
#> 4: 2020-01-06       4
#> 5: 2020-01-07       5
#> 6: 2020-01-08       6
#> 7: 2020-01-09       7
#> 8: 2020-01-10       8
#> 9: 2020-01-11       9
```
