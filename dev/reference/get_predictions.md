# Get predictions from a fitted model

**\[stable\]** Extracts predictions from a fitted model, combining
observations with model estimates. For
[`estimate_infections()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_infections.md)
returns predicted reported cases, for
[`estimate_secondary()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_secondary.md)
returns predicted secondary observations. For
[`estimate_truncation()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_truncation.md)
returns reconstructed observations adjusted for truncation.

## Usage

``` r
get_predictions(object, ...)

# S3 method for class 'estimate_infections'
get_predictions(object, CrIs = c(0.2, 0.5, 0.9), ...)

# S3 method for class 'estimate_secondary'
get_predictions(object, CrIs = c(0.2, 0.5, 0.9), ...)

# S3 method for class 'forecast_infections'
get_predictions(object, ...)

# S3 method for class 'forecast_secondary'
get_predictions(object, ...)

# S3 method for class 'estimate_truncation'
get_predictions(object, CrIs = c(0.2, 0.5, 0.9), ...)
```

## Arguments

- object:

  A fitted model object (e.g., from
  [`estimate_infections()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_infections.md),
  [`estimate_secondary()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_secondary.md),
  or
  [`estimate_truncation()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_truncation.md))

- ...:

  Additional arguments (currently unused)

- CrIs:

  Numeric vector of credible intervals to return. Defaults to c(0.2,
  0.5, 0.9).

## Value

A `data.table` with columns including date and summary statistics (mean,
sd, credible intervals) for the model predictions.

## Examples

``` r
if (FALSE) { # \dontrun{
# After fitting a model
predictions <- get_predictions(fit)
} # }
```
