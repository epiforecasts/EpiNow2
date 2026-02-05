# Extract elements from estimate_secondary objects with deprecated warnings

**\[deprecated\]** Provides backward compatibility for the old return
structure. The previous structure with `predictions`, `posterior`, and
`data` elements is deprecated. Use the accessor methods instead:

- `predictions` - use `get_predictions(object)`

- `posterior` - use `get_samples(object)`

- `data` - use `object$observations`

## Usage

``` r
# S3 method for class 'estimate_secondary'
x$name
```

## Arguments

- x:

  An `estimate_secondary` object

- name:

  The name of the element to extract

## Value

The requested element with a deprecation warning
