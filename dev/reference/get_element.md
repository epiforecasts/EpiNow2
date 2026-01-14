# Extracts an element of a `<dist_spec>`

Extracts an element of a `<dist_spec>`

## Usage

``` r
get_element(x, id = NULL, element)
```

## Arguments

- x:

  A `<dist_spec>`.

- id:

  Integer; the id of the distribution to use (if x is a composite
  distribution). If `x` is a single distribution this is ignored and can
  be left at its default value of `NULL`.

- element:

  The element, i.e. "parameters", "pmf" or "distribution".

## Value

The id to use.
