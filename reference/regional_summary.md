# Regional Summary Output

**\[maturing\]** Used to produce summary output either internally in
`regional_epinow` or externally.

## Usage

``` r
regional_summary(
  regional_output = NULL,
  data,
  results_dir = NULL,
  summary_dir = NULL,
  target_date = NULL,
  region_scale = "Region",
  all_regions = TRUE,
  return_output = is.null(summary_dir),
  plot = TRUE,
  max_plot = 10,
  ...
)
```

## Arguments

- regional_output:

  A list of output as produced by
  [`regional_epinow()`](https://epiforecasts.io/EpiNow2/reference/regional_epinow.md)
  and stored in the `regional` list.

- data:

  A `<data.frame>` of disease reports (confirm) by date (date), and
  region (`region`).

- results_dir:

  An optional character string indicating the location of the results
  directory to extract results from.

- summary_dir:

  A character string giving the directory in which to store summary of
  results.

- target_date:

  A character string giving the target date for which to extract results
  (in the format "yyyy-mm-dd"). Defaults to latest available estimates.

- region_scale:

  A character string indicating the name to give the regions being
  summarised.

- all_regions:

  Logical, defaults to `TRUE`. Should summary plots for all regions be
  returned rather than just regions of interest.

- return_output:

  Logical, defaults to FALSE. Should output be returned, this
  automatically updates to TRUE if no directory for saving is specified.

- plot:

  Logical, defaults to `TRUE`. Should regional summary plots be
  produced.

- max_plot:

  Numeric, defaults to 10. A multiplicative upper bound on the\\ number
  of cases shown on the plot. Based on the maximum number of reported
  cases.

- ...:

  Additional arguments passed to `report_plots`.

## Value

A list of summary measures and plots

## See also

[`regional_epinow()`](https://epiforecasts.io/EpiNow2/reference/regional_epinow.md)

## Examples

``` r
# get example output from regional_epinow model
regional_out <- readRDS(system.file(
  package = "EpiNow2", "extdata", "example_regional_epinow.rds"
))

summary <- regional_summary(
  regional_output = regional_out$regional,
  data = regional_out$summary$reported_cases
)
#> INFO [2026-02-09 09:45:59] No summary directory specified so returning summary output
names(summary)
#> [1] "latest_date"         "results"             "summarised_results" 
#> [4] "summary_plot"        "summarised_measures" "reported_cases"     
#> [7] "high_plots"          "plots"              
```
