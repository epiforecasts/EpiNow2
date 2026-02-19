# Extract elements from epinow objects with deprecated warnings

**\[deprecated\]** Provides backward compatibility for the old return
structure. The previous structure with `estimates`,
`estimated_reported_cases`, `summary`, `plots`, and
`estimate_infections` elements is deprecated. Use the standard S3
methods instead:

- `estimates$samples` - use `get_samples(object)`

- `estimates$summarised` - use `summary(object, type = "parameters")`

- `estimated_reported_cases` - use `estimates_by_report_date(object)`

- `summary` - use `summary(object)`

- `plots` - use `plot(object)`

- `estimate_infections` - use the object directly (it now inherits from
  `estimate_infections`)

## Usage

``` r
# S3 method for class 'epinow'
x$name
```

## Arguments

- x:

  An `epinow` object

- name:

  The name of the element to extract

## Value

The requested element with a deprecation warning for deprecated elements
