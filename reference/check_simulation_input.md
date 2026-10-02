# Validate simulation input data frame

Checks that a data frame intended for simulation has the required `date`
column and a named numeric value column, that the `date` column is in
date format, and that the value column contains non-negative numeric
values with no missing entries.

## Usage

``` r
check_simulation_input(data, value_col)
```

## Arguments

- data:

  A data frame with at least a `date` column and a numeric value column
  named `value_col`.

- value_col:

  Character; name of the numeric value column to check.

## Value

Called for its side effects.
