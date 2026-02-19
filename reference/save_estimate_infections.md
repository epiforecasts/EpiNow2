# Save Estimated Infections

**\[stable\]** Saves output from `estimate_infections` to a target
directory.

## Usage

``` r
save_estimate_infections(
  estimates,
  target_folder = NULL,
  samples = TRUE,
  return_fit = TRUE,
  CrIs = c(0.2, 0.5, 0.9)
)
```

## Arguments

- estimates:

  List of data frames as output by `estimate_infections`

- target_folder:

  Character string specifying where to save results (will create if not
  present).

- samples:

  Logical, defaults to TRUE. Should samples be saved

- return_fit:

  Logical, defaults to TRUE. Should the fit stan object be returned.

- CrIs:

  Numeric vector of credible intervals to calculate.

## Value

No return value, called for side effects

## See also

[`estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.md)
