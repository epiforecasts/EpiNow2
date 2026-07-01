# Model features

This vignette provides a quick reference to the modelling features
available in EpiNow2. For an overview of how these models connect see
the [model
overview](https://epiforecasts.io/EpiNow2/articles/model_overview.md).
For mathematical details see the model definition vignettes ([infection
model](https://epiforecasts.io/EpiNow2/articles/estimate_infections.md),
[secondary
model](https://epiforecasts.io/EpiNow2/articles/estimate_secondary.md),
[truncation
model](https://epiforecasts.io/EpiNow2/articles/estimate_truncation.md),
[distribution
model](https://epiforecasts.io/EpiNow2/articles/estimate_dist.md)). For
applied examples see the [options
vignette](https://epiforecasts.io/EpiNow2/articles/estimate_infections_options.md).
For prior guidance see the [prior choice
guide](https://epiforecasts.io/EpiNow2/articles/prior_choice_guide.md).

## Component overview

The package’s modelling features fall into four groups.

**Estimation models** fit a Stan model to data:

- [Infection model](#infection-model) —
  [`estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.md)
- [Secondary model](#secondary-model) —
  [`estimate_secondary()`](https://epiforecasts.io/EpiNow2/reference/estimate_secondary.md)
- [Truncation / nowcasting](#truncation-nowcasting) —
  [`estimate_truncation()`](https://epiforecasts.io/EpiNow2/reference/estimate_truncation.md)
- [Delay distribution fitting](#delay-distribution-fitting) —
  [`estimate_dist()`](https://epiforecasts.io/EpiNow2/reference/estimate_dist.md)

**Model configuration** controls how the estimation models behave:

- [Reproduction number](#reproduction-number) —
  [`rt_opts()`](https://epiforecasts.io/EpiNow2/reference/rt_opts.md)
- [Gaussian process](#gaussian-process) —
  [`gp_opts()`](https://epiforecasts.io/EpiNow2/reference/gp_opts.md)
- [Delay distributions](#delay-distributions) —
  [`gt_opts()`](https://epiforecasts.io/EpiNow2/reference/generation_time_opts.md),
  [`delay_opts()`](https://epiforecasts.io/EpiNow2/reference/delay_opts.md),
  [`trunc_opts()`](https://epiforecasts.io/EpiNow2/reference/trunc_opts.md)
- [Observation model](#observation-model) —
  [`obs_opts()`](https://epiforecasts.io/EpiNow2/reference/obs_opts.md)
- [Options summary](#options-summary) — defaults and which models use
  each

**Forward simulation and forecasting** generate observations:

- [Simulation](#simulation) —
  [`simulate_infections()`](https://epiforecasts.io/EpiNow2/reference/simulate_infections.md),
  [`simulate_secondary()`](https://epiforecasts.io/EpiNow2/reference/simulate_secondary.md)
- [Forecasting](#forecasting) —
  [`forecast_infections()`](https://epiforecasts.io/EpiNow2/reference/forecast_infections.md),
  [`forecast_secondary()`](https://epiforecasts.io/EpiNow2/reference/forecast_secondary.md)

**Supporting utilities**:

- [Data preprocessing](#data-preprocessing) —
  [`fill_missing()`](https://epiforecasts.io/EpiNow2/reference/fill_missing.md)
- [Workflow wrappers](#workflow-wrappers) —
  [`epinow()`](https://epiforecasts.io/EpiNow2/reference/epinow.md),
  [`regional_epinow()`](https://epiforecasts.io/EpiNow2/reference/regional_epinow.md)
- [Stan backend](#stan-backend) —
  [`stan_opts()`](https://epiforecasts.io/EpiNow2/reference/stan_opts.md)

## Estimation models

### Infection model

[`estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.md)
reconstructs infections from a count time series (e.g. reported cases)
using either a generative renewal model or non-parametric
back-calculation.

| Feature | Argument | Description | See also |
|----|----|----|----|
| Renewal equation (default) | `rt = rt_opts(...)` | Generative model using the reproduction number and generation time | [Model definition](https://epiforecasts.io/EpiNow2/articles/estimate_infections.md) |
| Back-calculation | `rt = NULL`, `backcalc = backcalc_opts(...)` | Non-parametric deconvolution of reported cases | [Model definition](https://epiforecasts.io/EpiNow2/articles/estimate_infections.md) |

See also: [estimate_infections
workflow](https://epiforecasts.io/EpiNow2/articles/estimate_infections_workflow.md)

### Secondary model

[`estimate_secondary()`](https://epiforecasts.io/EpiNow2/reference/estimate_secondary.md)
estimates the relationship between a primary and secondary observation
(e.g. cases and deaths, admissions and bed occupancy).

| Feature | Argument | Description | See also |
|----|----|----|----|
| Incidence model | `secondary_opts(type = "incidence")` | Secondary reports as a convolution of primary cases | [Secondary model definition](https://epiforecasts.io/EpiNow2/articles/estimate_secondary.md) |
| Prevalence model | `secondary_opts(type = "prevalence")` | Cumulative secondary reports (e.g. bed occupancy) | [Secondary model definition](https://epiforecasts.io/EpiNow2/articles/estimate_secondary.md) |

See also: [estimate_secondary model
definition](https://epiforecasts.io/EpiNow2/articles/estimate_secondary.md)

### Truncation / nowcasting

Recent observations are typically incomplete due to reporting delays
(right truncation).
[`estimate_truncation()`](https://epiforecasts.io/EpiNow2/reference/estimate_truncation.md)
estimates a truncation distribution from multiple snapshots of the same
data source over time and produces a nowcast (predicted complete
observations). The reconstructed observations can be obtained from the
fit with
[`get_predictions()`](https://epiforecasts.io/EpiNow2/reference/get_predictions.md).
The estimated distribution can be passed to
[`estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.md)
via
[`trunc_opts()`](https://epiforecasts.io/EpiNow2/reference/trunc_opts.md)
for truncation-adjusted inference.

| Feature | Argument | Description | See also |
|----|----|----|----|
| Estimate truncation and nowcast | `estimate_truncation(data = list_of_snapshots)` | Fit a truncation distribution and produce a nowcast from data snapshots | [Truncation model definition](https://epiforecasts.io/EpiNow2/articles/estimate_truncation.md) |
| Truncation-adjusted inference | `estimate_infections(truncation = trunc_opts(dist = ...))` | Adjust for right-truncation of recent data in the observation model | [Truncation model definition](https://epiforecasts.io/EpiNow2/articles/estimate_truncation.md) |

See also: [estimate_truncation model
definition](https://epiforecasts.io/EpiNow2/articles/estimate_truncation.md)

### Delay distribution fitting

[`estimate_dist()`](https://epiforecasts.io/EpiNow2/reference/estimate_dist.md)
fits delay distributions from linelist data using likelihood functions
vendored from
[primarycensored](https://primarycensored.epinowcast.org/).

| Feature | Argument | Description | See also |
|----|----|----|----|
| Double interval censoring | Date columns in linelist | Accounts for censoring of both primary and secondary events | [Distribution model](https://epiforecasts.io/EpiNow2/articles/estimate_dist.md) |
| Variable censoring windows | `pdate_upr`, `sdate_upr` columns | Per-observation primary and secondary censoring window widths | [Distribution model](https://epiforecasts.io/EpiNow2/articles/estimate_dist.md) |
| Right truncation | `obs_date` column | Per-observation truncation times from reporting delays | [Distribution model](https://epiforecasts.io/EpiNow2/articles/estimate_dist.md) |
| Primary event distribution | `estimate_dist(primary = "expgrowth")` | Account for exponential growth in primary events | [Distribution model](https://epiforecasts.io/EpiNow2/articles/estimate_dist.md) |
| Distribution families | `estimate_dist(dist = ...)` | `"lognormal"` (default), `"gamma"`, `"normal"`, `"exp"`, `"weibull"` | [Distribution model](https://epiforecasts.io/EpiNow2/articles/estimate_dist.md) |
| Untruncated approximation | `estimate_dist(obs_time_threshold = ...)` | Skip the right-truncation renormalisation when observation times are far beyond the largest observed delay | [Distribution model](https://epiforecasts.io/EpiNow2/articles/estimate_dist.md) |
| Observation aggregation | Automatic | Identical delay-censoring-truncation strata are aggregated to speed up fitting | [Distribution model](https://epiforecasts.io/EpiNow2/articles/estimate_dist.md) |

See also: [distribution
model](https://epiforecasts.io/EpiNow2/articles/estimate_dist.md),
[worked
example](https://epiforecasts.io/EpiNow2/articles/estimate_dist_workflow.md)

## Model configuration

### Reproduction number

When using the renewal equation, several options control how the
time-varying reproduction number is modelled.

| Feature | Argument | Description | See also |
|----|----|----|----|
| GP on differences of log Rt (default) | `rt_opts(gp_on = "R_t-1")` | Gaussian process applied to successive Rt values | [Options examples](https://epiforecasts.io/EpiNow2/articles/estimate_infections_options.md) |
| GP on deviations from R0 | `rt_opts(gp_on = "R0")` | Gaussian process applied as deviations from a global mean | [Options examples](https://epiforecasts.io/EpiNow2/articles/estimate_infections_options.md) |
| Random walk | `rt_opts(rw = 7)` with `gp = NULL` | Piecewise-constant Rt with specified step size | [Options examples](https://epiforecasts.io/EpiNow2/articles/estimate_infections_options.md) |
| Breakpoints | Add a `breakpoint` column to the input data | Step changes in Rt at user-specified dates; on by default in [`rt_opts()`](https://epiforecasts.io/EpiNow2/reference/rt_opts.md), set `use_breakpoints = FALSE` to disable | [Options examples](https://epiforecasts.io/EpiNow2/articles/estimate_infections_options.md) |
| Fixed Rt | `gp = NULL` (and default [`rt_opts()`](https://epiforecasts.io/EpiNow2/reference/rt_opts.md)) | Constant reproduction number sampled from the prior; no GP, no random walk | [Options examples](https://epiforecasts.io/EpiNow2/articles/estimate_infections_options.md) |
| Population adjustment | `rt_opts(pop = Fixed(N))` | Adjust Rt for susceptible depletion; `pop` accepts any `<dist_spec>` | [Options examples](https://epiforecasts.io/EpiNow2/articles/estimate_infections_options.md) |
| Growth rate method | `rt_opts(growth_method = "infectiousness")` | Alternative growth rate calculation via infectiousness | [Model definition](https://epiforecasts.io/EpiNow2/articles/estimate_infections.md) |

See also: [estimate_infections model
definition](https://epiforecasts.io/EpiNow2/articles/estimate_infections.md),
[options
examples](https://epiforecasts.io/EpiNow2/articles/estimate_infections_options.md)

### Gaussian process

The Gaussian process controls the flexibility of the time-varying
reproduction number or infection trajectory. Configured via
[`gp_opts()`](https://epiforecasts.io/EpiNow2/reference/gp_opts.md). For
kernel mathematics and the Hilbert space spectral approximation see the
[GP implementation
details](https://epiforecasts.io/EpiNow2/articles/gaussian_process_implementation_details.md)
vignette.

| Feature | Argument | Description |
|----|----|----|
| Kernel choice | `gp_opts(kernel = ...)` | `"matern"` (default), `"se"`, `"ou"`, or `"periodic"` |
| Disable GP | `gp = NULL` in [`estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.md) | Remove the Gaussian process entirely |
| Accuracy/speed tradeoff | `gp_opts(basis_prop = ...)` | Higher values increase accuracy; lower values are faster (default 0.2) |

See also: [GP implementation
details](https://epiforecasts.io/EpiNow2/articles/gaussian_process_implementation_details.md),
[options
examples](https://epiforecasts.io/EpiNow2/articles/estimate_infections_options.md),
[prior choice
guide](https://epiforecasts.io/EpiNow2/articles/prior_choice_guide.md)

### Delay distributions

Delay distributions map latent infections to observed quantities. See
the [workflow
vignette](https://epiforecasts.io/EpiNow2/articles/estimate_infections_workflow.md)
for a practical guide and the [prior choice
guide](https://epiforecasts.io/EpiNow2/articles/prior_choice_guide.md)
for default priors.

| Feature | Argument | Description |
|----|----|----|
| Generation time | `gt_opts(dist = ...)` | Time between successive infections |
| Reporting delay | `delay_opts(dist = ...)` | Delay from infection to report |
| Composite delays | `delay_opts(dist = dist1 + dist2)` | Sum of independent delay distributions |
| Non-parametric delays | `dist = NonParametric(...)` | Fixed PMF rather than a parametric family |
| Uncertain parameters | e.g. `LogNormal(meanlog = Normal(...), ...)` | Parameters drawn from prior distributions |
| Truncation correction | `trunc_opts(dist = ...)` | Adjust for right-truncation of recent data |

See also: [workflow
vignette](https://epiforecasts.io/EpiNow2/articles/estimate_infections_workflow.md),
[prior choice
guide](https://epiforecasts.io/EpiNow2/articles/prior_choice_guide.md),
[fitting delay
distributions](https://epiforecasts.io/EpiNow2/articles/estimate_dist_workflow.md)

### Observation model

The observation model links latent expected cases to reported data.
Configured via
[`obs_opts()`](https://epiforecasts.io/EpiNow2/reference/obs_opts.md).
For default prior values see the [prior choice
guide](https://epiforecasts.io/EpiNow2/articles/prior_choice_guide.md).

| Feature | Argument | Description |
|----|----|----|
| Negative binomial (default) | `obs_opts(family = "negbin")` | Overdispersed count model |
| Poisson | `obs_opts(family = "poisson")` | No overdispersion |
| Day-of-week effect | `obs_opts(week_effect = TRUE)` | Separate reporting rate per day of the week (default on) |
| Scaling / ascertainment | `obs_opts(scale = Normal(...))` | Fraction of infections observed; accepts any `<dist_spec>` |
| Likelihood weighting | `obs_opts(weight = ...)` | Re-weight observations in the log density |
| Aggregated data | `fill_missing(missing_dates = "accumulate")` | Latent daily expectations are accumulated in the model before likelihood evaluation |
| Missing observations | NA values in data | Time points with NA observations are excluded from the likelihood |

Aggregation and missing data are handled at the model level in Stan, not
just in preprocessing.
[`fill_missing()`](https://epiforecasts.io/EpiNow2/reference/fill_missing.md)
constructs the flags that the Stan model uses; the model itself
accumulates latent expected reports and drops missing observations from
the likelihood. Both
[`estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.md)
and
[`estimate_secondary()`](https://epiforecasts.io/EpiNow2/reference/estimate_secondary.md)
support these features.

See also: [workflow
vignette](https://epiforecasts.io/EpiNow2/articles/estimate_infections_workflow.md),
[prior choice
guide](https://epiforecasts.io/EpiNow2/articles/prior_choice_guide.md)

### Options summary

The table below summarises the options functions, what they configure,
their defaults, and which estimation functions use them.

| Function | Configures | Default | Used by |
|----|----|----|----|
| [`rt_opts()`](https://epiforecasts.io/EpiNow2/reference/rt_opts.md) | Reproduction number model | GP on log Rt differences | [`estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.md) |
| [`gp_opts()`](https://epiforecasts.io/EpiNow2/reference/gp_opts.md) | Gaussian process prior | Matern 3/2 kernel | [`estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.md) |
| [`gt_opts()`](https://epiforecasts.io/EpiNow2/reference/generation_time_opts.md) | Generation time distribution | `Fixed(1)` (degenerate 1-day generation interval; users normally override) | [`estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.md) |
| [`delay_opts()`](https://epiforecasts.io/EpiNow2/reference/delay_opts.md) | Reporting delay distributions | `Fixed(0)` (no delay) | [`estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.md), [`estimate_secondary()`](https://epiforecasts.io/EpiNow2/reference/estimate_secondary.md) |
| [`obs_opts()`](https://epiforecasts.io/EpiNow2/reference/obs_opts.md) | Observation model | Negative binomial with day-of-week effect | [`estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.md), [`estimate_secondary()`](https://epiforecasts.io/EpiNow2/reference/estimate_secondary.md) |
| [`trunc_opts()`](https://epiforecasts.io/EpiNow2/reference/trunc_opts.md) | Truncation distribution | `Fixed(0)` (no truncation) | [`estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.md) |
| [`backcalc_opts()`](https://epiforecasts.io/EpiNow2/reference/backcalc_opts.md) | Back-calculation settings | Smoothed reports as prior | [`estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.md) (when `rt = NULL`) |
| [`secondary_opts()`](https://epiforecasts.io/EpiNow2/reference/secondary_opts.md) | Secondary model type | Incidence | [`estimate_secondary()`](https://epiforecasts.io/EpiNow2/reference/estimate_secondary.md) |

See the [prior choice
guide](https://epiforecasts.io/EpiNow2/articles/prior_choice_guide.md)
for default values and guidance.

## Forward simulation and forecasting

The estimation models already produce forecasts as part of their fit,
projecting forward over a horizon set by the `horizon` argument. The
functions below are for the cases where you want to simulate or forecast
separately from a fit: generating synthetic observations from known
parameters, or extending a fitted model with new inputs (e.g. a new Rt
trajectory or new primary data).

### Simulation

Simulation functions generate observations from known or fixed
parameters, useful for model checking and scenario analysis.

| Feature | Argument | Description | See also |
|----|----|----|----|
| Simulate infections | `simulate_infections(R, initial_infections)` | Forward-simulate from a given Rt trajectory via the renewal equation | [Workflow](https://epiforecasts.io/EpiNow2/articles/estimate_infections_workflow.md) |
| Simulate secondary | `simulate_secondary(primary, ...)` | Simulate secondary observations from primary data | [Secondary model definition](https://epiforecasts.io/EpiNow2/articles/estimate_secondary.md) |
| Convolve and scale | `convolve_and_scale(data, ...)` | R-based convolution with time-varying parameters | [Secondary model definition](https://epiforecasts.io/EpiNow2/articles/estimate_secondary.md) |

See also: [workflow
vignette](https://epiforecasts.io/EpiNow2/articles/estimate_infections_workflow.md)

### Forecasting

Forecasts can be generated from fitted models by projecting forward with
specified or estimated parameters.

| Feature | Argument | Description | See also |
|----|----|----|----|
| Infection forecast | `forecast_infections(estimates, R)` | Simulate future infections from a fitted model with a new Rt trajectory | [Workflow](https://epiforecasts.io/EpiNow2/articles/estimate_infections_workflow.md) |
| Secondary forecast | `forecast_secondary(estimate, primary)` | Forecast secondary observations from new primary data | [Secondary model definition](https://epiforecasts.io/EpiNow2/articles/estimate_secondary.md) |

See also: [workflow
vignette](https://epiforecasts.io/EpiNow2/articles/estimate_infections_workflow.md)

## Supporting utilities

### Data preprocessing

[`fill_missing()`](https://epiforecasts.io/EpiNow2/reference/fill_missing.md)
is an R-side utility that prepares data for the estimation functions by
constructing the accumulation and missingness flags that the Stan models
require.

| Feature | Argument | Description |
|----|----|----|
| Accumulate missing dates | `fill_missing(missing_dates = "accumulate")` | Mark missing dates for model-level accumulation |
| Zero-fill missing dates | `fill_missing(missing_dates = "zero")` | Insert zero observations for missing dates |
| Ignore missing dates (default) | `fill_missing(missing_dates = "ignore")` | Skip missing dates in the likelihood |
| Handle NA observations | `fill_missing(missing_obs = ...)` | Accumulate or zero-fill NA values |
| Weekly reporting | `fill_missing(initial_accumulate = 7)` | Set accumulation window for weekly data |

See also: [workflow
vignette](https://epiforecasts.io/EpiNow2/articles/estimate_infections_workflow.md)

### Workflow wrappers

Convenience functions wrap the core estimation and reporting steps.

| Feature | Argument | Description | See also |
|----|----|----|----|
| Production wrapper | [`epinow()`](https://epiforecasts.io/EpiNow2/reference/epinow.md) | Wraps [`estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.md) with logging and formatted output | [epinow vignette](https://epiforecasts.io/EpiNow2/articles/epinow.md) |
| Multi-region | [`regional_epinow()`](https://epiforecasts.io/EpiNow2/reference/regional_epinow.md) | Runs [`epinow()`](https://epiforecasts.io/EpiNow2/reference/epinow.md) across regions in parallel | [Getting started](https://epiforecasts.io/EpiNow2/articles/EpiNow2.md) |
| Region-specific options | [`opts_list()`](https://epiforecasts.io/EpiNow2/reference/opts_list.md) | Generate per-region configuration lists | [Getting started](https://epiforecasts.io/EpiNow2/articles/EpiNow2.md) |

See also: [epinow
vignette](https://epiforecasts.io/EpiNow2/articles/epinow.md), [getting
started](https://epiforecasts.io/EpiNow2/articles/EpiNow2.md)

### Stan backend

Models are implemented in Stan. Users can switch between MCMC sampling
(default, via `cmdstanr` or `rstan`), variational inference, the Laplace
approximation, or pathfinder using `stan_opts(method = ...)`. See the
[workflow
vignette](https://epiforecasts.io/EpiNow2/articles/estimate_infections_workflow.md)
for details on configuring the backend.
