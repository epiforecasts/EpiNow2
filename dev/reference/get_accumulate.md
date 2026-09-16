# Get accumulation flags from data

Returns the `accumulate` column if present, otherwise a vector of
`FALSE` (no accumulation).

## Usage

``` r
get_accumulate(data)
```

## Arguments

- data:

  A data.table that may contain an `accumulate` column.

## Value

A logical vector of length `nrow(data)`.
