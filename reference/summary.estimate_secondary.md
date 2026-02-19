# Summarise results from estimate_secondary

**\[stable\]** Returns a summary of the fitted secondary model including
posterior parameter estimates with credible intervals.

## Usage

``` r
# S3 method for class 'estimate_secondary'
summary(
  object,
  type = c("compact", "parameters"),
  params = NULL,
  CrIs = c(0.2, 0.5, 0.9),
  ...
)
```

## Arguments

- object:

  A fitted model object from
  [`estimate_secondary()`](https://epiforecasts.io/EpiNow2/reference/estimate_secondary.md)

- type:

  Character string indicating the type of summary to return. Options are
  "compact" (default) which returns delay distribution parameters and
  scaling factors, or "parameters" for all parameters or a filtered set.

- params:

  Character vector of parameter names to include. Only used when
  `type = "parameters"`. If NULL (default), returns all parameters.

- CrIs:

  Numeric vector of credible intervals to calculate.

- ...:

  Additional arguments (currently unused)

## Value

A `<data.table>` with summary statistics (mean, sd, median, credible
intervals) for model parameters. When `type = "compact"`, returns only
key parameters (delay distribution parameters and scaling factors). When
`type = "parameters"`, returns all or filtered parameters.
