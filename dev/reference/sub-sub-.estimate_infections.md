# Extract elements from estimate_infections objects with bracket notation

**\[deprecated\]** Provides backward compatibility for bracket-based
access to deprecated elements. See
[\$.estimate_infections](https://epiforecasts.io/EpiNow2/dev/reference/cash-.estimate_infections.md)
for details on the deprecation.

## Usage

``` r
# S3 method for class 'estimate_infections'
x[[i]]
```

## Arguments

- x:

  An `estimate_infections` object

- i:

  The name or index of the element to extract

## Value

The requested element. Errors for deprecated element names.
