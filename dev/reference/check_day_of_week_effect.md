# Validate day of week effect against the observation model week length

`check_day_of_week_effect()` checks that a user-supplied day of week
effect has as many elements as the reporting week length implied by the
observation model (see
[`obs_opts()`](https://epiforecasts.io/EpiNow2/dev/reference/obs_opts.md)),
i.e. `week_length` if `week_effect` is `TRUE` and 1 otherwise. The two
are required to match because the day of week effect is passed to stan
as a simplex of that length; a mismatch is otherwise silently resolved
by recycling, which reassigns the weights to different days than the
user intended.

## Usage

``` r
check_day_of_week_effect(day_of_week_effect, week_effect)
```

## Arguments

- day_of_week_effect:

  Either `NULL` (no day of the week effect specified by the user) or a
  numeric vector.

- week_effect:

  Integer; the reporting week length expected by the observation model,
  as returned in the `week_effect` element of
  [`create_obs_model()`](https://epiforecasts.io/EpiNow2/dev/reference/create_obs_model.md).

## Value

Called for its side effects.
