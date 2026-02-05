# Validate data input

**\[stable\]** `check_reports_valid()` checks that the supplied data is
a `<data.frame>`, and that it has the right column names and types. In
particular, it checks that the date column is in date format and does
not contain NAs, and that the other columns are numeric.

## Usage

``` r
check_reports_valid(
  data,
  model = c("estimate_infections", "estimate_secondary")
)
```

## Arguments

- data:

  A data frame with either:

  - a minimum of two columns: `date` and `confirm`, if to be used by
    [`estimate_infections()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_infections.md)
    or
    [`estimate_truncation()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_truncation.md),
    or

  - a minimum of three columns: `date`, `primary`, and `secondary`, if
    to be used by
    [`estimate_secondary()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_secondary.md).

- model:

  The EpiNow2 model to be used. Either "estimate_infections",
  "estimate_truncation", or "estimate_secondary". This is used to
  determine which checks to perform on the data input.

## Value

Called for its side effects.
