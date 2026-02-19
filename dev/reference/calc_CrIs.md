# Calculate Credible Intervals

**\[stable\]** Adds symmetric credible intervals based on quantiles.

## Usage

``` r
calc_CrIs(samples, summarise_by = NULL, CrIs = c(0.2, 0.5, 0.9))
```

## Arguments

- samples:

  A data.table containing at least a value variable

- summarise_by:

  A character vector of variables to group by.

- CrIs:

  Numeric vector of credible intervals to calculate.

## Value

A data.table containing the `summarise_by` variables and the specified
lower and upper credible intervals.

## Examples

``` r
samples <- data.frame(value = 1:10, type = "car")
# add credible intervals
calc_CrIs(samples)
#> Key: <.>
#>         . lower_90 lower_50 lower_20 upper_20 upper_50 upper_90
#>    <char>    <num>    <num>    <num>    <num>    <num>    <num>
#> 1:      .     1.45     3.25      4.6      6.4     7.75     9.55
# add 90% credible interval grouped by type
calc_CrIs(samples, summarise_by = "type")
#> Key: <type>
#>      type lower_90 lower_50 lower_20 upper_20 upper_50 upper_90
#>    <char>    <num>    <num>    <num>    <num>    <num>    <num>
#> 1:    car     1.45     3.25      4.6      6.4     7.75     9.55
```
