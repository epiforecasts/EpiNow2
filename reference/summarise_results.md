# Summarise Real-time Results

**\[questioning\]** Used internally by `regional_summary` to produce a
summary table of results. May be streamlined in later releases.

## Usage

``` r
summarise_results(
  regions,
  summaries = NULL,
  results_dir = NULL,
  target_date = "latest",
  region_scale = "Region"
)
```

## Arguments

- regions:

  An character string containing the list of regions to extract results
  for (must all have results for the same target date).

- summaries:

  A list of summary `<data.frame>`s as output by `epinow`

- results_dir:

  An optional character string indicating the location of the results
  directory to extract results from.

- target_date:

  A character string indicating the target date to extract results for.
  All regions must have results for this date.

- region_scale:

  A character string indicating the name to give the regions being
  summarised.

## Value

A list of summary data
