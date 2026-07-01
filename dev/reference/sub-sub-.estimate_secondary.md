# Extract elements from estimate_secondary objects with bracket notation

**\[deprecated\]** Provides backward compatibility for bracket-based
access to deprecated elements. See
[\$.estimate_secondary](https://epiforecasts.io/EpiNow2/dev/reference/cash-.estimate_secondary.md)
for details on the deprecation.

## Usage

``` r
# S3 method for class 'estimate_secondary'
x[[i]]
```

## Arguments

- x:

  An `estimate_secondary` object

- i:

  The name or index of the element to extract

## Value

The requested element. Errors for deprecated element names.
