# Summarise Regional Runtimes

**\[maturing\]** Used internally by `regional_epinow` to summarise
region run times.

## Usage

``` r
regional_runtimes(
  regional_output = NULL,
  target_folder = NULL,
  target_date = NULL,
  return_output = FALSE
)
```

## Arguments

- regional_output:

  A list of output as produced by
  [`regional_epinow()`](https://epiforecasts.io/EpiNow2/dev/reference/regional_epinow.md)
  and stored in the `regional` list.

- target_folder:

  Character string specifying where to save results (will create if not
  present).

- target_date:

  A character string giving the target date for which to extract results
  (in the format "yyyy-mm-dd"). Defaults to latest available estimates.

- return_output:

  Logical, defaults to FALSE. Should output be returned, this
  automatically updates to TRUE if no directory for saving is specified.

## Value

A data.table of region run times

## See also

[`regional_summary()`](https://epiforecasts.io/EpiNow2/dev/reference/regional_summary.md)
[`regional_epinow()`](https://epiforecasts.io/EpiNow2/dev/reference/regional_epinow.md)

## Examples

``` r
regional_out <- readRDS(system.file(
  package = "EpiNow2", "extdata", "example_regional_epinow.rds"
))
regional_runtimes(regional_output = regional_out$regional)
#> INFO [2026-02-19 19:01:37] No target directory specified so returning timings
#>      region          time
#>      <char>    <difftime>
#> 1: testland 18.27078 secs
#> 2: realland 21.34412 secs
```
