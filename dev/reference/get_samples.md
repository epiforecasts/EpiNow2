# Get posterior samples from a fitted model

Extracts posterior samples from a fitted model. By default, combines all
parameters into a single `<data.table>` with dates and metadata added.

## Usage

``` r
get_samples(object, ...)

# S3 method for class 'estimate_infections'
get_samples(object, format = c("data.table", "list"), ...)

# S3 method for class 'epinow'
get_samples(object, format = c("data.table", "list"), ...)

# S3 method for class 'forecast_infections'
get_samples(object, ...)

# S3 method for class 'estimate_secondary'
get_samples(object, format = c("data.table", "list"), ...)

# S3 method for class 'forecast_secondary'
get_samples(object, ...)

# S3 method for class 'estimate_truncation'
get_samples(object, format = c("data.table", "list"), ...)
```

## Arguments

- object:

  A fitted model object (e.g., from
  [`estimate_infections()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_infections.md))

- ...:

  Additional arguments (currently unused)

- format:

  Character string specifying the output format. For model classes
  backed by a Stan fit (`estimate_infections`, `epinow`,
  `estimate_secondary`, `estimate_truncation`):

  - `"data.table"` (default): a long-format `<data.table>` with dates
    and other metadata added.

  - `"list"`: the raw named list of arrays as returned by
    [`rstan::extract()`](https://mc-stan.org/rstan/reference/stanfit-method-extract.html),
    with no dates or metadata added.

## Value

If `format = "data.table"`, a `data.table` with columns: date, variable,
strat, sample, time, value, type. Contains all posterior samples for all
parameters. If `format = "list"`, a named list of arrays, one per
parameter.

## Examples

``` r
if (FALSE) { # \dontrun{
# After fitting a model
samples <- get_samples(fit)
# Filter to specific parameters
R_samples <- samples[variable == "R"]

# Get the raw list of arrays instead
raw_samples <- get_samples(fit, format = "list")
} # }
```
