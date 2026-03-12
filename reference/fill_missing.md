# Fill missing data in a data set to prepare it for use within the package

**\[experimental\]** This function ensures that all days between the
first and last date in the data are present. It adds an `accumulate`
column that indicates whether modelled observations should be
accumulated onto a later data point. point. This is useful for modelling
data that is reported less frequently than daily, e.g. weekly incidence
data, as well as other reporting artifacts such as delayed weekedn
reporting. The function can also be used to fill in missing observations
with zeros.

## Usage

``` r
fill_missing(
  data,
  missing_dates = c("ignore", "accumulate", "zero"),
  missing_obs = c("ignore", "accumulate", "zero"),
  initial_accumulate,
  obs_column = "confirm",
  by = NULL
)
```

## Arguments

- data:

  Data frame with a `date` column. The other columns depend on the model
  that the data are to be used, e.g.
  [`estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.md)
  or
  [`estimate_secondary()`](https://epiforecasts.io/EpiNow2/reference/estimate_secondary.md).
  See the documentation there for the expected format. The data must not
  already have an `accumulate` function, otherwise the function will
  fail with an error.

- missing_dates:

  Character. Options are "ignore" (the default), "accumulate" and
  "zero". This determines how missing dates in the data are interpreted.
  If set to "ignore", any missing dates in the observation data will be
  interpreted as missing and skipped in the likelihood. If set to
  "accumulate", modelled observations on dates that are missing in the
  data will be accumulated and added to the next non-missing data point.
  This can be used to model incidence data that is reported less
  frequently than daily. In that case, the first data point is not
  included in the likelihood (unless `initial_accumulate` is set to a
  value greater than one) but used only to reset modelled observations
  to zero. If "zero" then all observations on missing dates will be
  assumed to be of value 0.

- missing_obs:

  Character. How to process dates that exist in the data but have
  observations with NA values. The options available are the same ones
  as for the `missing_dates` argument.

- initial_accumulate:

  Integer. The number of initial dates to accumulate if `missing_dates`
  or `missing_obs` is set to `"accumulate"`. This argument needs ot have
  a minimum of 1. If it is set to 1 then no accumulation is happening on
  the first data point. If it is greater than 1 then dates are added to
  the beginning of the data set to get be able to have a sufficient
  number of modelled observations accumulated onto the first data point.
  For modelling weekly incidence data this should be set to 7. If
  accumulating and the first data point is not NA and this is argument
  is not set, then if all dates in the data have the same gap this will
  be taken as initial accumulation and a warning given to inform the
  user. If not all gaps are the same the first data point will be
  removed with a warning.

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

a data.table with an `accumulate` column that indicates whether values
are accumulated (see the documentation of the `data` argument in
[`estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.md))

## Examples

``` r
cases <- data.table::copy(example_confirmed)
## calculate weekly sum
cases[, confirm := data.table::frollsum(confirm, 7)]
#>            date confirm
#>          <Date>   <num>
#>   1: 2020-02-22      NA
#>   2: 2020-02-23      NA
#>   3: 2020-02-24      NA
#>   4: 2020-02-25      NA
#>   5: 2020-02-26      NA
#>  ---                   
#> 126: 2020-06-26    1695
#> 127: 2020-06-27    1950
#> 128: 2020-06-28    1861
#> 129: 2020-06-29    1811
#> 130: 2020-06-30    1716
## limit to dates once a week
cases <- cases[seq(7, nrow(cases), 7)]
## set the second observation to missing
cases[2, confirm := NA]
#>           date confirm
#>         <Date>   <num>
#>  1: 2020-02-28     647
#>  2: 2020-03-06      NA
#>  3: 2020-03-13   11255
#>  4: 2020-03-20   25922
#>  5: 2020-03-27   39504
#>  6: 2020-04-03   34703
#>  7: 2020-04-10   28384
#>  8: 2020-04-17   25315
#>  9: 2020-04-24   21032
#> 10: 2020-05-01   15490
#> 11: 2020-05-08   10395
#> 12: 2020-05-15    7238
#> 13: 2020-05-22    4910
#> 14: 2020-05-29    3726
#> 15: 2020-06-05    2281
#> 16: 2020-06-12    2129
#> 17: 2020-06-19    2017
#> 18: 2020-06-26    1695
## fill missing data
fill_missing(cases, missing_dates = "accumulate", initial_accumulate = 7)
#> Key: <date>
#>            date confirm accumulate
#>          <Date>   <num>     <lgcl>
#>   1: 2020-02-22      NA       TRUE
#>   2: 2020-02-23      NA       TRUE
#>   3: 2020-02-24      NA       TRUE
#>   4: 2020-02-25      NA       TRUE
#>   5: 2020-02-26      NA       TRUE
#>  ---                              
#> 122: 2020-06-22      NA       TRUE
#> 123: 2020-06-23      NA       TRUE
#> 124: 2020-06-24      NA       TRUE
#> 125: 2020-06-25      NA       TRUE
#> 126: 2020-06-26    1695      FALSE
```
