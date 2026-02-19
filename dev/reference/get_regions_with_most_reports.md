# Get Regions with Most Reported Cases

**\[stable\]** Extract a vector of regions with the most reported cases
in a set time window.

## Usage

``` r
get_regions_with_most_reports(data, time_window = 7, no_regions = 6)
```

## Arguments

- data:

  A `<data.frame>` of disease reports (confirm) by date (date), and
  region (`region`).

- time_window:

  Numeric, number of days to include from latest date in data. Defaults
  to 7 days.

- no_regions:

  Numeric, number of regions to return. Defaults to 6.

## Value

A character vector of regions with the highest reported cases
