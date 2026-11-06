# Build the stan-side init-prior data block from a list of priors

Bundles the data items consumed by the init-prior dispatch in
`inst/stan/estimate_infections.stan` (`n_init_priors`, `init_param_ids`,
`init_dists`, `init_lower`, `init_upper`, `init_dist_params_length`,
`init_dist_params`). With no priors, returns empty arrays so callers
(such as forward simulation) satisfy the shared data block.

## Usage

``` r
make_init_priors(priors = list())
```

## Arguments

- priors:

  A list of `list(param_id, dist, lower_bound)` entries, one per init
  prior; defaults to an empty list.

## Value

A named list of stan_data fields to be merged in via
[`c()`](https://rdrr.io/r/base/c.html).
