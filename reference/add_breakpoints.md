# Add breakpoints to certain dates in a data set.

Add breakpoints to certain dates in a data set.

## Usage

``` r
add_breakpoints(data, dates = as.Date(character(0)))
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

- dates:

  A vector of dates to use as breakpoints.

## Value

A data.table with `breakpoint` set to 1 on each of the specified dates.

## Examples

``` r
reported_cases <- add_breakpoints(example_confirmed, as.Date("2020-03-26"))
```
