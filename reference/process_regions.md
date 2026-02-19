# Process all Region Estimates

**\[stable\]** Internal function that processes the output from multiple
[`epinow()`](https://epiforecasts.io/EpiNow2/reference/epinow.md) runs,
adds summary logging information.

## Usage

``` r
process_regions(regional_out, regions)
```

## Arguments

- regional_out:

  A list of output from multiple runs of
  [`regional_epinow()`](https://epiforecasts.io/EpiNow2/reference/regional_epinow.md)

- regions:

  A character vector identifying the regions that have been run

## Value

A list of all regional estimates and successful regional estimates

## See also

[`regional_epinow()`](https://epiforecasts.io/EpiNow2/reference/regional_epinow.md)
[`epinow()`](https://epiforecasts.io/EpiNow2/reference/epinow.md)
