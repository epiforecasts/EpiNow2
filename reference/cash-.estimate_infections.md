# Extract elements from estimate_infections objects with deprecation errors

**\[deprecated\]** Provides backward compatibility for the old return
structure. The previous structure with `samples` and `summarised`
elements is deprecated. Use the accessor methods instead:

- `samples` - use `get_samples(object)`

- `summarised` - use `summary(object, type = "parameters")`

## Usage

``` r
# S3 method for class 'estimate_infections'
x$name
```

## Arguments

- x:

  An `estimate_infections` object

- name:

  The name of the element to extract

## Value

The requested element. Errors for deprecated element names.
