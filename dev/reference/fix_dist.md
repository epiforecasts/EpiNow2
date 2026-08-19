# Remove uncertainty in the parameters of a `<dist_spec>`

**\[deprecated\]** This function has been renamed to
[`fix_parameters()`](https://epiforecasts.io/EpiNow2/dev/reference/fix_parameters.md)
as a more appropriate name.

## Usage

``` r
fix_dist(x, strategy = c("mean", "sample"))
```

## Arguments

- x:

  A `<dist_spec>`

- strategy:

  Character; either "mean" (use the mean estimates of the mean and
  standard deviation) or "sample" (randomly sample mean and standard
  deviation from uncertainty given in the `<dist_spec>`

## Value

A `<dist_spec>` object without uncertainty
