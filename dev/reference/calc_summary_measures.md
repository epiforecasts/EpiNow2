# Calculate All Summary Measures

**\[stable\]** Calculate summary statistics and credible intervals from
a `<data.frame>` by group.

## Usage

``` r
calc_summary_measures(
  samples,
  summarise_by = NULL,
  order_by = NULL,
  CrIs = c(0.2, 0.5, 0.9)
)
```

## Arguments

- samples:

  A data.table containing at least a value variable

- summarise_by:

  A character vector of variables to group by.

- order_by:

  A character vector of parameters to order by, defaults to all
  `summarise_by` variables.

- CrIs:

  Numeric vector of credible intervals to calculate.

## Value

A data.table containing summary statistics by group.

## Examples

``` r
samples <- data.frame(value = 1:10, type = "car")
# default
calc_summary_measures(samples)
#>      type median  mean      sd lower_90 lower_50 lower_20 upper_20 upper_50
#>    <char>  <num> <num>   <num>    <num>    <num>    <num>    <num>    <num>
#> 1:    car    5.5   5.5 3.02765     1.45     3.25      4.6      6.4     7.75
#>    upper_90
#>       <num>
#> 1:     9.55
#  by type
calc_summary_measures(samples, summarise_by = "type")
#>      type median  mean      sd lower_90 lower_50 lower_20 upper_20 upper_50
#>    <char>  <num> <num>   <num>    <num>    <num>    <num>    <num>    <num>
#> 1:    car    5.5   5.5 3.02765     1.45     3.25      4.6      6.4     7.75
#>    upper_90
#>       <num>
#> 1:     9.55
```
