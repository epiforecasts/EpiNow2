# Format raw Stan samples with dates and metadata

Internal helper that extracts Stan parameters, adds dates to
time-varying parameters, and combines into a single long-format
data.table.

## Usage

``` r
format_samples_with_dates(raw_samples, args, observations)
```

## Arguments

- raw_samples:

  Raw samples from extract_samples()

- args:

  Model arguments (from object\$args)

- observations:

  Observation data with dates

## Value

A `data.table` in long format with dates and metadata
