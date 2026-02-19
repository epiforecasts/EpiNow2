# Calculate Credible Interval

**\[stable\]** Adds symmetric a credible interval based on quantiles.

## Usage

``` r
calc_CrI(samples, summarise_by = NULL, CrI = 0.9)
```

## Arguments

- samples:

  A data.table containing at least a value variable

- summarise_by:

  A character vector of variables to group by.

- CrI:

  Numeric between 0 and 1. The credible interval for which to return
  values. Defaults to 0.9.

## Value

A data.table containing the upper and lower bounds for the specified
credible interval.

## Examples

``` r
samples <- data.frame(value = 1:10, type = "car")
# add 90% credible interval
calc_CrI(samples)
#>    value      CrI
#>    <num>   <char>
#> 1:  1.45 lower_90
#> 2:  9.55 upper_90
# add 90% credible interval grouped by type
calc_CrI(samples, summarise_by = "type")
#>      type value      CrI
#>    <char> <num>   <char>
#> 1:    car  1.45 lower_90
#> 2:    car  9.55 upper_90
```
