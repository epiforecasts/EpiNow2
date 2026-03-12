# Provide Summary Statistics for Estimated Infections and Rt

**\[questioning\]** Creates a snapshot summary of estimates. May be
removed in later releases as S3 methods are enhanced.

## Usage

``` r
report_summary(
  summarised_estimates,
  rt_samples,
  target_folder = NULL,
  return_numeric = FALSE
)
```

## Arguments

- summarised_estimates:

  A data.table of summarised estimates containing the following
  variables: variable, median, bottom, and top. It should contain the
  following estimates: R, infections, and r (rate of growth).

- rt_samples:

  A data.table containing Rt samples with the following variables:
  sample and value.

- target_folder:

  Character string specifying where to save results (will create if not
  present).

- return_numeric:

  Should numeric summary information be returned.

## Value

A data.table containing formatted and numeric summary measures
