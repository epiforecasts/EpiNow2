# Get delay distributions from a fitted model

**\[experimental\]** Extracts the delay distributions used in a fitted
model. For
[`estimate_infections()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_infections.md)
this includes generation time, reporting delays, and truncation delays.
For
[`estimate_secondary()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_secondary.md)
this includes any delays specified in the model. For
[`estimate_truncation()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_truncation.md)
this returns the estimated truncation distribution.

## Usage

``` r
get_delays(object, ...)

# S3 method for class 'epinowfit'
get_delays(object, ...)
```

## Arguments

- object:

  A fitted model object (e.g., from
  [`estimate_infections()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_infections.md),
  [`estimate_secondary()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_secondary.md),
  or
  [`estimate_truncation()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_truncation.md))

- ...:

  Additional arguments passed to methods

## Value

A named list of `dist_spec` objects representing the delay
distributions. The list may be empty if no delays were specified. Use
[`names()`](https://rdrr.io/r/base/names.html) to see available delays.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get all delays from a fitted model
delays <- get_delays(fit)
names(delays)
# Access specific delay
delays$generation_time
} # }
```
