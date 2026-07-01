# Merge truncation predictions with observations for display

Internal function to prepare data for plotting or returning merged
predictions and observations. Combines predictions with observed data
from each snapshot, including the latest observations as reference.

## Usage

``` r
merge_truncation_predictions_obs(observations, predictions)
```

## Arguments

- observations:

  A list of `<data.frame>`s containing date and confirm columns, as
  stored in an `estimate_truncation` object.

- predictions:

  A `<data.table>` of predictions from
  [`get_predictions()`](https://epiforecasts.io/EpiNow2/dev/reference/get_predictions.md).

## Value

A `<data.table>` with columns: date, report_date, confirm (observed),
last_confirm (from latest snapshot), and prediction columns (median,
CrIs).
