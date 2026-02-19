# Format Credible Intervals

**\[stable\]** Combines a list of values into formatted credible
intervals.

## Usage

``` r
make_conf(value, CrI = 90, reverse = FALSE)
```

## Arguments

- value:

  List of value to map into a string. Requires, `point`, `lower`, and
  `upper.`

- CrI:

  Numeric, credible interval to report. Defaults to 90.

- reverse:

  Logical, defaults to FALSE. Should the reported credible interval be
  switched.

## Value

A character vector formatted for reporting

## Examples

``` r
value <- list(median = 2, lower_90 = 1, upper_90 = 3)
make_conf(value)
#> [1] "2 (1 -- 3)"
```
