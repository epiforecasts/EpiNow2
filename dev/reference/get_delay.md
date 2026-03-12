# Get a single delay distribution from a fitted model

**\[experimental\]** Convenience function to extract a single delay
distribution by name. This is equivalent to `get_delays(object)[[type]]`
but provides a cleaner interface.

## Usage

``` r
get_delay(object, type, ...)

# Default S3 method
get_delay(object, type, ...)
```

## Arguments

- object:

  A fitted model object (e.g., from
  [`estimate_infections()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_infections.md),
  [`estimate_secondary()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_secondary.md),
  or
  [`estimate_truncation()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_truncation.md))

- type:

  Character string specifying the delay type to extract. Common values
  include `"generation_time"`, `"reporting"`, and `"truncation"`. Use
  `names(get_delays(object))` to see available types.

- ...:

  Additional arguments passed to methods

## Value

A `dist_spec` object representing the requested delay distribution, or
`NULL` if the specified type is not found.

## See also

[`get_delays()`](https://epiforecasts.io/EpiNow2/dev/reference/get_delays.md)
to retrieve all delay distributions as a list

## Examples

``` r
if (FALSE) { # \dontrun{
# Get truncation delay from estimate_truncation
trunc_dist <- get_delay(fit, "truncation")

# Get generation time from estimate_infections
gt <- get_delay(fit, "generation_time")
} # }
```
