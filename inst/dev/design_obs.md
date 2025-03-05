# Combining multiple observation models

The overall premise is that there is a single latent process (modelled as GP or with the renewal equation) that informs multiple observation processes. Each of these can have:

- a delay or set of delays
- truncation
- scaling
- accumulation
- week(end) effects
- different weights
- overdispersion

The delays need to be identified in case they are common across different observations, e.g.

- incubation period + reporting delay for symptomatic cases
- incubation period + hospital admission + another reporting delay for hospitalisations

We're assuming that delays can be equal but the observations are independent (not e.g. hospitalisation conditional on case report)

This means that the current options specified in `delay_opts()`, `trunc_opts()`, `obs_opts()`, the `accumulation` argument in `forecast_opts()` are all specific to the particular observation type.

## Implementation

### Stan

Observations are converted from an array to a ragged array.
The main required changes here will be lookups so that observations are correctly associated with different observation models.
Some variables e.g. for day of the week effects will have to become 2D arrays.

### R

The observation options mentioned above will have to be combined in a single interface.
The proposed solution is that the current `data`, `delays`, `truncation` and `future_accumulation` become folded into a new `obs_model()` (based on `obs_opts()` and these can be passed as `...`, e.g.

```r
estimate_infections(
  cases = obs_model(
    data = reported_cases,
    delays = incubation_period + reporting_delay,
    truncation = trunc_opts(Lognormal(1, 2)),
    family = "poisson",
    accumulation = 7
  ),
  hospitalisations = obs_model(
    data = admissions,
    delays = incubation_period + admission_delay,
    truncation = trunc_opts(Gamma(1, 1)),
    family = "negbin",
    weight = 0.5
  )
  generation_time = generation_time_opts(generation_time)
)
```

The main work will have to be on the interface, e.g. redefining seeding times., accessing predictions for the different observation types, merging dates, etc.
