# Process regional estimate

**\[maturing\]** Internal function that removes output that is not
required, and returns logging information.

## Usage

``` r
process_region(
  out,
  target_region,
  timing,
  return_output = TRUE,
  return_timing = TRUE,
  complete_logger = "EpiNow2.epinow"
)
```

## Arguments

- out:

  List of output returned by
  [`epinow()`](https://epiforecasts.io/EpiNow2/dev/reference/epinow.md)

- target_region:

  Character string indicating the region being evaluated

- timing:

  Output from [`Sys.time()`](https://rdrr.io/r/base/Sys.time.html)

- return_output:

  Logical, defaults to FALSE. Should output be returned, this
  automatically updates to TRUE if no directory for saving is specified.

- return_timing:

  Logical, should runtime be returned

- complete_logger:

  Character string indicating the logger to output the completion of
  estimation to.

## Value

A list of processed output

## See also

[`regional_epinow()`](https://epiforecasts.io/EpiNow2/dev/reference/regional_epinow.md)
