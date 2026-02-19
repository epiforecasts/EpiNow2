# Get predictions from a fitted model

**\[stable\]** Extracts predictions from a fitted model. For
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
get_predictions(
  object,
  format = c("summary", "sample", "quantile"),
  CrIs = c(0.2, 0.5, 0.9),
  quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95),
  ...
)

# S3 method for class 'estimate_secondary'
get_predictions(
  object,
  format = c("summary", "sample", "quantile"),
  CrIs = c(0.2, 0.5, 0.9),
  quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95),
  ...
)

# S3 method for class 'forecast_infections'
get_predictions(
  object,
  format = c("summary", "sample", "quantile"),
  CrIs = c(0.2, 0.5, 0.9),
  quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95),
  ...
)

# S3 method for class 'forecast_secondary'
get_predictions(
  object,
  format = c("summary", "sample", "quantile"),
  CrIs = c(0.2, 0.5, 0.9),
  quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95),
  ...
)

# S3 method for class 'estimate_truncation'
get_predictions(
  object,
  format = c("summary", "sample", "quantile"),
  CrIs = c(0.2, 0.5, 0.9),
  quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95),
  ...
)
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

- format:

  Character string specifying the output format:

  - `"summary"` (default): summary statistics (mean, sd, median, CrIs)

  - `"sample"`: raw posterior samples for
    [`scoringutils::as_forecast_sample()`](https://epiforecasts.io/scoringutils/reference/as_forecast_sample.html)

  - `"quantile"`: quantile predictions for
    [`scoringutils::as_forecast_quantile()`](https://epiforecasts.io/scoringutils/reference/as_forecast_quantile.html)

- CrIs:

  Numeric vector of credible intervals to return. Defaults to c(0.2,
  0.5, 0.9). Only used when `format = "summary"`.

- quantiles:

  Numeric vector of quantile levels to return. Defaults to c(0.05, 0.25,
  0.5, 0.75, 0.95). Only used when `format = "quantile"`.

## Value

A `data.table` with columns depending on `format`:

- `format = "summary"`: date, mean, sd, median, and credible intervals

- `format = "sample"`: forecast_date, date, horizon, sample, predicted

- `format = "quantile"`: forecast_date, date, horizon, quantile_level,
  predicted

## Examples

``` r
if (FALSE) { # \dontrun{
# After fitting a model
# Get summary predictions (default)
predictions <- get_predictions(fit)

# Get sample-level predictions for scoringutils
samples <- get_predictions(fit, format = "sample")

# Get quantile predictions for scoringutils
quantiles <- get_predictions(fit, format = "quantile")
} # }
```
