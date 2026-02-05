# Summarise rt and cases

**\[maturing\]** Produces summarised `<data.frame>`s of output across
regions. Used internally by `regional_summary`.

## Usage

``` r
summarise_key_measures(
  regional_results = NULL,
  results_dir = NULL,
  summary_dir = NULL,
  type = "region",
  date = "latest"
)
```

## Arguments

- regional_results:

  A list of dataframes as produced by `get_regional_results`

- results_dir:

  Character string indicating the directory from which to extract
  results.

- summary_dir:

  Character string the directory into which to save results as a csv.

- type:

  Character string, the region identifier to apply (defaults to region).

- date:

  A Character string (in the format "yyyy-mm-dd") indicating the date to
  extract data for. Defaults to "latest" which finds the latest results
  available.

## Value

A list of summarised Rt, cases by date of infection and cases by date of
report

## See also

[`regional_summary()`](https://epiforecasts.io/EpiNow2/reference/regional_summary.md)
