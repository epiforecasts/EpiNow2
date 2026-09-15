# Pack a dist_spec into stan-side init-prior fields

Converts a `<dist_spec>` into the integer/vector representation consumed
by the init-prior loop in `inst/stan/estimate_infections.stan` (data
items `init_dists`, `init_dist_params`, `init_lower`, `init_upper`).

## Usage

``` r
pack_init_prior(dist, lower_bound = 0)
```

## Arguments

- dist:

  A `<dist_spec>` (LogNormal, Gamma, or Normal).

- lower_bound:

  Numeric, lower bound on the parameter's support.

## Value

A list with elements `dist_type` (integer code: 0 = lognormal, 1 =
gamma, 2 = normal), `params` (numeric, the distribution parameters),
`lower` and `upper` (numeric scalars).
