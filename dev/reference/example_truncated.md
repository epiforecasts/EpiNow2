# Example Case Data Set with Truncation

**\[stable\]** An example dataset of observed cases with truncation
applied. This data is generated internally for use in the example of
[`estimate_truncation()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_truncation.md).
For details on how the data is generated, see
<https://github.com/epiforecasts/EpiNow2/blob/main/data-raw/truncated.R>
\#nolint

## Usage

``` r
example_truncated
```

## Format

A list of `data.table`s containing cases reported on each date until a
point of truncation. Each element of the list is a `data.table` with the
following columns:

- date:

  Date of case report.

- confirm:

  Number of confirmed cases.
