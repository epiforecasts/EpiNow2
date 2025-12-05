# Summary output from epinow

**\[stable\]** `summary` method for class "epinow".

## Usage

``` r
# S3 method for class 'epinow'
summary(
  object,
  output = c("estimates", "forecast", "estimated_reported_cases"),
  target_date = NULL,
  params = NULL,
  ...
)
```

## Arguments

- object:

  A list of output as produced by "epinow".

- output:

  A character string of output to summarise. Defaults to "estimates" but
  also supports "forecast", and "estimated_reported_cases".

- target_date:

  Date, defaults to maximum found in the data if not specified.

- params:

  A character vector of parameters to filter for.

- ...:

  Pass additional summary arguments to lower level methods

## Value

Returns a `<data.frame>` of summary output

## See also

[`summary.estimate_infections()`](https://epiforecasts.io/EpiNow2/dev/reference/summary.estimate_infections.md)
[`epinow()`](https://epiforecasts.io/EpiNow2/dev/reference/epinow.md)
