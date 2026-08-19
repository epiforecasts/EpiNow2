# Prepare linelist data for delay estimation

Converts date-based or numeric delay data into an aggregated format
suitable for the primarycensored Stan model. Handles date-to-numeric
conversion, computation of derived columns, an obs-time-to-Inf
heuristic, and aggregation by unique delay/censoring combinations.

## Usage

``` r
.prepare_linelist_data(data, obs_time_threshold = 2, verbose = FALSE)
```

## Arguments

- data:

  A data frame with date columns (`pdate_lwr`, `sdate_lwr`, and
  optionally `pdate_upr`, `sdate_upr`, `obs_date`, `n`).

- obs_time_threshold:

  Numeric multiplier for the obs-time-to-Inf heuristic. Observations
  where `relative_obs_time > max(delay_upr) * obs_time_threshold` are
  treated as untruncated. Set to `Inf` to disable.

- verbose:

  Logical, print progress messages?

## Value

A data frame with columns: `delay_lwr`, `delay_upr`, `pwindow`,
`relative_obs_time`, `n`.
