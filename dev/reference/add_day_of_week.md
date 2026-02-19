# Adds a day of the week vector

Adds a day of the week vector

## Usage

``` r
add_day_of_week(dates, week_effect = 7)
```

## Arguments

- dates:

  Vector of dates

- week_effect:

  Numeric from 1 to 7 defaults to 7

## Value

A numeric vector containing the period day of the week index

## Examples

``` r
if (FALSE) { # \dontrun{
dates <- seq(as.Date("2020-03-15"), by = "days", length.out = 15)
# Add date based day of week
add_day_of_week(dates, 7)

# Add shorter week
add_day_of_week(dates, 4)
} # }
```
