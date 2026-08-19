# Prepare truncation observations for Stan

Internal function to process a list of observation snapshots into the
matrix format required by the truncation Stan model.

## Usage

``` r
prepare_truncation_obs(data, trunc_max)
```

## Arguments

- data:

  A list of `<data.frame>`s each containing date and confirm columns.
  Each data set should be a snapshot of reported data.

- trunc_max:

  Integer, the maximum truncation delay to consider.

## Value

A list containing:

- `obs`: Matrix of observations (time x datasets)

- `obs_dist`: Vector of NA counts per dataset (used to determine
  truncation)

- `t`: Number of time points

- `obs_sets`: Number of observation datasets

- `dirty_obs`: The processed data.tables (ordered by nrow)
