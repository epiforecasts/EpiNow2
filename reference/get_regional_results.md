# Get Combined Regional Results

**\[stable\]** Summarises results across regions either from input or
from disk. See the examples for details.

## Usage

``` r
get_regional_results(
  regional_output,
  results_dir,
  date,
  samples = TRUE,
  forecast = FALSE
)
```

## Arguments

- regional_output:

  A list of output as produced by
  [`regional_epinow()`](https://epiforecasts.io/EpiNow2/reference/regional_epinow.md)
  and stored in the `regional` list.

- results_dir:

  A character string indicating the folder containing the `{EpiNow2}`
  results to extract.

- date:

  A Character string (in the format "yyyy-mm-dd") indicating the date to
  extract data for. Defaults to "latest" which finds the latest results
  available.

- samples:

  Logical, defaults to `TRUE`. Should samples be returned.

- forecast:

  Logical, defaults to `FALSE`. Should forecast results be returned.

## Value

A list of estimates, forecasts and estimated cases by date of report.

## Examples

``` r
# get example multiregion estimates
regional_out <- readRDS(system.file(
  package = "EpiNow2", "extdata", "example_regional_epinow.rds"
))

# from output
results <- get_regional_results(regional_out$regional, samples = FALSE)
```
