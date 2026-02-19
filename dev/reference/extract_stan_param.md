# Extract a parameter summary from a Stan object

**\[stable\]** Extracts summarised parameter posteriors from a `stanfit`
object using `rstan::summary()` in a format consistent with other
summary functions in `{EpiNow2}`.

## Usage

``` r
extract_stan_param(
  fit,
  params = NULL,
  CrIs = c(0.2, 0.5, 0.9),
  var_names = FALSE
)
```

## Arguments

- fit:

  A `<stanfit>` objec.

- params:

  A character vector of parameters to extract. Defaults to all
  parameters.

- CrIs:

  Numeric vector of credible intervals to calculate.

- var_names:

  Logical defaults to `FALSE`. Should variables be named. Automatically
  set to TRUE if multiple parameters are to be extracted.

## Value

A `<data.table>` summarising parameter posteriors. Contains a following
variables: `variable`, `mean`, `mean_se`, `sd`, `median`, and `lower_`,
`upper_` followed by credible interval labels indicating the credible
intervals present.
