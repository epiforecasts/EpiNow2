# Model overview

## Introduction

EpiNow2 provides several estimation models that can be combined for
end-to-end epidemiological inference and forecasting. This vignette
gives an overview of how these models connect. For a reference of what
each model can do see the [model
features](https://epiforecasts.io/EpiNow2/articles/model_features.md)
vignette.

## Architecture

The diagram below shows the main functions and how they relate to one
another.

![Diagram showing how the main EpiNow2 models
connect.](model-overview-architecture-1.png)

Data flows from top to bottom. Solid arrows show direct dependencies;
dashed arrows show optional connections. Green boxes are options
functions that configure the estimation models. See the [model
features](https://epiforecasts.io/EpiNow2/articles/model_features.md)
vignette for what each function and option does.

## Relationship between models

[`estimate_dist()`](https://epiforecasts.io/EpiNow2/reference/estimate_dist.md)
fits delay distributions from linelist data, accounting for double
interval censoring and right truncation. Its output can define priors
for the other models via
[`delay_opts()`](https://epiforecasts.io/EpiNow2/reference/delay_opts.md),
[`gt_opts()`](https://epiforecasts.io/EpiNow2/reference/generation_time_opts.md),
or
[`trunc_opts()`](https://epiforecasts.io/EpiNow2/reference/trunc_opts.md).

[`estimate_truncation()`](https://epiforecasts.io/EpiNow2/reference/estimate_truncation.md)
produces both a nowcast and a truncation distribution from multiple
snapshots of the same data. The distribution is typically passed to
[`estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.md)
via
[`trunc_opts()`](https://epiforecasts.io/EpiNow2/reference/trunc_opts.md)
for truncation-adjusted inference, but can also be used via
[`delay_opts()`](https://epiforecasts.io/EpiNow2/reference/delay_opts.md)
or
[`gt_opts()`](https://epiforecasts.io/EpiNow2/reference/generation_time_opts.md)
where appropriate.

[`estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.md)
is the core model, estimating latent infections and the time-varying
reproduction number from a count time series. Its posterior feeds into
[`forecast_infections()`](https://epiforecasts.io/EpiNow2/reference/forecast_infections.md)
for projections and can inform
[`simulate_infections()`](https://epiforecasts.io/EpiNow2/reference/simulate_infections.md)
for scenario analysis.

Estimated primary observations from
[`estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.md)
are used as input to
[`estimate_secondary()`](https://epiforecasts.io/EpiNow2/reference/estimate_secondary.md),
which estimates secondary outcomes (e.g. deaths, hospitalisations).
[`forecast_secondary()`](https://epiforecasts.io/EpiNow2/reference/forecast_secondary.md)
extends a fitted secondary model with new primary data;
[`simulate_secondary()`](https://epiforecasts.io/EpiNow2/reference/simulate_secondary.md)
generates synthetic secondary observations.

[`epinow()`](https://epiforecasts.io/EpiNow2/reference/epinow.md) wraps
[`estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.md)
with logging and formatted output.
[`regional_epinow()`](https://epiforecasts.io/EpiNow2/reference/regional_epinow.md)
runs [`epinow()`](https://epiforecasts.io/EpiNow2/reference/epinow.md)
across regions in parallel.

## Where to look next

**Start here**

- [Getting started](https://epiforecasts.io/EpiNow2/articles/EpiNow2.md)
  — quick introduction and basic usage
- [Model
  features](https://epiforecasts.io/EpiNow2/articles/model_features.md)
  — feature reference for arguments and options

**Model definitions** (mathematical detail)

- [Infection
  model](https://epiforecasts.io/EpiNow2/articles/estimate_infections.md)
  —
  [`estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.md)
  - [Gaussian process
    implementation](https://epiforecasts.io/EpiNow2/articles/gaussian_process_implementation_details.md)
    — shared component used inside the renewal and back-calculation
    models
- [Secondary
  model](https://epiforecasts.io/EpiNow2/articles/estimate_secondary.md)
  —
  [`estimate_secondary()`](https://epiforecasts.io/EpiNow2/reference/estimate_secondary.md)
- [Truncation
  model](https://epiforecasts.io/EpiNow2/articles/estimate_truncation.md)
  —
  [`estimate_truncation()`](https://epiforecasts.io/EpiNow2/reference/estimate_truncation.md)
- [Distribution
  model](https://epiforecasts.io/EpiNow2/articles/estimate_dist.md) —
  [`estimate_dist()`](https://epiforecasts.io/EpiNow2/reference/estimate_dist.md)
  - [Understanding delay
    distributions](https://epiforecasts.io/EpiNow2/articles/delays.md) —
    conceptual background on delays in *EpiNow2*

**Estimating the reproduction number**

- [Workflow](https://epiforecasts.io/EpiNow2/articles/estimate_infections_workflow.md)
  — end-to-end estimation and forecasting
- [Configuration
  examples](https://epiforecasts.io/EpiNow2/articles/estimate_infections_options.md)
  — different model configurations with results
- [Prior choice
  guide](https://epiforecasts.io/EpiNow2/articles/prior_choice_guide.md)
  — default priors and how to modify them

**Auxiliary models**

- [Fitting delay
  distributions](https://epiforecasts.io/EpiNow2/articles/estimate_dist_workflow.md)
  — worked example for
  [`estimate_dist()`](https://epiforecasts.io/EpiNow2/reference/estimate_dist.md)
- [Forecasting multiple data
  streams](https://epiforecasts.io/EpiNow2/articles/forecasting_multiple_data_streams.md)
  — combining
  [`estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.md)
  with
  [`forecast_secondary()`](https://epiforecasts.io/EpiNow2/reference/forecast_secondary.md)

**Production use**

- [epinow() and
  regional_epinow()](https://epiforecasts.io/EpiNow2/articles/epinow.md)
  — wrappers for production runs

**Case studies**

- [External case
  studies](https://epiforecasts.io/EpiNow2/articles/case-studies.md) —
  applications in the literature
