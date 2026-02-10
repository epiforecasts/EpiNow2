# Clean Regions

**\[stable\]** Removes regions with insufficient time points, and
provides logging information on the input.

## Usage

``` r
clean_regions(data, non_zero_points)
```

## Arguments

- data:

  A `<data.frame>` of disease reports (confirm) by date (date), and
  region (`region`).

- non_zero_points:

  Numeric, the minimum number of time points with non-zero cases in a
  region required for that region to be evaluated. Defaults to 7.

## Value

A dataframe of cleaned regional data

## See also

[`regional_epinow()`](https://epiforecasts.io/EpiNow2/dev/reference/regional_epinow.md)
