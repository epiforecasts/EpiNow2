# Get posterior samples from a fitted model

**\[stable\]** Extracts posterior samples from a fitted model, combining
all parameters into a single data.table with dates and metadata.

## Usage

``` r
get_samples(object, ...)

# S3 method for class 'estimate_infections'
get_samples(object, ...)

# S3 method for class 'epinow'
get_samples(object, ...)

# S3 method for class 'forecast_infections'
get_samples(object, ...)

# S3 method for class 'estimate_secondary'
get_samples(object, ...)

# S3 method for class 'forecast_secondary'
get_samples(object, ...)

# S3 method for class 'estimate_truncation'
get_samples(object, ...)
```

## Arguments

- object:

  A fitted model object (e.g., from
  [`estimate_infections()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_infections.md))

- ...:

  Additional arguments (currently unused)

## Value

A `data.table` with columns: date, variable, strat, sample, time, value,
type. Contains all posterior samples for all parameters.

## Examples

``` r
if (FALSE) { # \dontrun{
# After fitting a model
samples <- get_samples(fit)
# Filter to specific parameters
R_samples <- samples[variable == "R"]
} # }
```
