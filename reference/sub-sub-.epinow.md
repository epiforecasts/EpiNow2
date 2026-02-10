# Extract elements from epinow objects with bracket notation

**\[deprecated\]** Provides backward compatibility for bracket-based
access to deprecated elements. See
[\$.epinow](https://epiforecasts.io/EpiNow2/reference/cash-.epinow.md)
for details on the deprecation.

## Usage

``` r
# S3 method for class 'epinow'
x[[i]]
```

## Arguments

- x:

  An `epinow` object

- i:

  The name or index of the element to extract

## Value

The requested element with a deprecation warning for deprecated elements
