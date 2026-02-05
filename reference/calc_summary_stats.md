# Calculate Summary Statistics

**\[stable\]** Calculate summary statistics from a `<data.frame>` by
group. Currently supports the mean, median and standard deviation.

## Usage

``` r
calc_summary_stats(samples, summarise_by = NULL)
```

## Arguments

- samples:

  A data.table containing at least a value variable

- summarise_by:

  A character vector of variables to group by.

## Value

A data.table containing the upper and lower bounds for the specified
credible interval

## Examples

``` r
samples <- data.frame(value = 1:10, type = "car")
# default
calc_summary_stats(samples)
#>    median  mean      sd
#>     <num> <num>   <num>
#> 1:    5.5   5.5 3.02765
#  by type
calc_summary_stats(samples, summarise_by = "type")
#>      type median  mean      sd
#>    <char>  <num> <num>   <num>
#> 1:    car    5.5   5.5 3.02765
```
