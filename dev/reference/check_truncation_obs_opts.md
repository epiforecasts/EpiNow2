# Check that obs_opts settings unused by estimate_truncation are at defaults

Internal check that warns when
[`obs_opts()`](https://epiforecasts.io/EpiNow2/dev/reference/obs_opts.md)
settings that are not consumed by the truncation model (week effect,
scale, weight) have been set to non-default values. These settings are
silently ignored by
[`estimate_truncation()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_truncation.md)
which only uses `family`, `dispersion`, `likelihood` and
`return_likelihood`.

## Usage

``` r
check_truncation_obs_opts(obs)
```

## Arguments

- obs:

  An `<obs_opts>` object.

## Value

Called for its side effects.
