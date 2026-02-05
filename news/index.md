# Changelog

## EpiNow2 1.8.0

CRAN release: 2026-02-04

### Breaking changes

- Changed
  [`discretise()`](https://epiforecasts.io/EpiNow2/reference/discretise.md)
  to use the `primarycensored` package for double censored PMF
  calculations, replacing the previous CDF difference approximation.
  This provides more accurate discretisation but will change the exact
  numerical values returned every time a distribution without
  uncertainty is discretised. Code that depends on the specific
  numerical output of
  [`discretise()`](https://epiforecasts.io/EpiNow2/reference/discretise.md)
  may produce different results, though the differences should be small
  and represent improvements in accuracy. The function interface remains
  unchanged.
- Changed population-adjusted Rt estimates (via `pop` in
  [`rt_opts()`](https://epiforecasts.io/EpiNow2/reference/rt_opts.md))
  to account for susceptible depletion. Adjusted Rt represents the
  effective reproduction number given the current susceptible
  population, whilst unadjusted Rt represents transmission in a fully
  susceptible population. If you previously used `pop` in
  [`rt_opts()`](https://epiforecasts.io/EpiNow2/reference/rt_opts.md)
  for forecasting only, the returned `R` estimates now reflect the
  adjusted values rather than unadjusted. Unadjusted estimates are
  provided in a separate `R_unadjusted` output variable.
- Removed deprecated functions and arguments that have been erroring
  since v1.5.0/v1.6.0: `dist_skel()`, `apply_tolerance()`, `fix_dist()`,
  `gp_opts(matern_type)`, and
  `estimate_truncation(obs, model, weigh_delay_priors)`.
- The `variable` column in
  [`get_samples()`](https://epiforecasts.io/EpiNow2/reference/get_samples.md)
  and
  [`summary()`](https://epiforecasts.io/EpiNow2/reference/summary.epinow.md)
  output now contains semantic parameter names (e.g.,
  `"generation_time[1]"`, `"fraction_observed"`) instead of generic
  category names like `"delay_params"` or `"params"`. Existing code
  filtering by `variable == "R"` or similar continues to work unchanged.

### Package changes

- Updated DESCRIPTION: removed “EpiForecasts” from authors, revised
  package description, and added `lintr` and `styler` to development
  dependencies.
- Fixed integration tests for
  [`estimate_truncation()`](https://epiforecasts.io/EpiNow2/reference/estimate_truncation.md)
  and
  [`estimate_secondary()`](https://epiforecasts.io/EpiNow2/reference/estimate_secondary.md)
  parameter recovery, and updated `example_truncated` data generation to
  use the Stan `discretised_pmf` function directly.
- Updated
  [`setup_future()`](https://epiforecasts.io/EpiNow2/reference/setup_future.md)
  to use
  [`parallelly::availableCores()`](https://parallelly.futureverse.org/reference/availableCores.html)
  instead of the re-exported
  [`future::availableCores()`](https://parallelly.futureverse.org/reference/availableCores.html),
  and removed the deprecated `earlySignal` argument from
  [`future::plan()`](https://future.futureverse.org/reference/plan.html)
  calls.
- The test suite has been reorganised into core tests (fast, always run)
  and integration tests (slow, run weekly), improving local development
  speed by 77% (from 9 minutes to 2 minutes) whilst maintaining test
  coverage.
- Added comprehensive unit tests for Stan functions including
  `discretised_pmf`, `get_delay_rev_pmf`, `convolve_to_report`, and
  `deconvolve_infections`.
- Development-only dependencies (`covr`, `here`, `hexSticker`, `magick`,
  `pkgdown`, `precommit`, `usethis`) have been moved from `Suggests` to
  `Config/Needs/dev`. This reduces the dependency burden for end users
  while maintaining full functionality for package developers.
  Developers should use `pak::pak(".", dependencies = TRUE)` to install
  all dependencies including dev tools.
- The package now has a hex logo.
- Parameter IDs are now prefixed with `param_id_parameter_name` to make
  them easier to discover. If you were previously extracting parameter
  posteriors with the pattern `[parameter_name]_id`, you now have to do
  `param_id_[parameter_name]`, for example, `fraction_observed_id` is
  now `param_id_fraction_observed`.
- [`estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.md)
  now returns an S3 object of class
  `c("epinowfit", "estimate_infections", "list")` with elements `fit`,
  `args`, and `observations`.
  - Use `get_samples(object)` to extract formatted posterior samples
    (replaces `summary(object, type = "samples")`).
  - Use `get_predictions(object)` to get predicted reported cases.
    Supports three output formats: `"summary"` (default) for summary
    statistics, `"sample"` for raw posterior samples compatible with
    \[scoringutils::as_forecast_sample()\], and `"quantile"` for
    quantile predictions compatible with
    \[scoringutils::as_forecast_quantile()\].
  - Use `summary(object)` to get summarised estimates (same as before,
    but `type = "samples"` is now deprecated).
  - Access the Stan fit directly via `object$fit`, model arguments via
    `object$args`, and observations via `object$observations`.
  - **Deprecated**: `summary(object, type = "samples")` now issues a
    deprecation warning. Use `get_samples(object)` instead.
  - **Deprecated**: `$samples` and `$summarised` accessors now issue
    deprecation warnings. Use
    [`get_samples()`](https://epiforecasts.io/EpiNow2/reference/get_samples.md)
    and
    [`summary()`](https://epiforecasts.io/EpiNow2/reference/summary.epinow.md)
    instead.
  - **Deprecated**: Internal function
    [`extract_parameter_samples()`](https://epiforecasts.io/EpiNow2/reference/extract_parameter_samples.md)
    renamed to
    [`format_simulation_output()`](https://epiforecasts.io/EpiNow2/reference/format_simulation_output.md)
    for clarity.
- [`forecast_infections()`](https://epiforecasts.io/EpiNow2/reference/forecast_infections.md)
  now returns an independent S3 class `"forecast_infections"` instead of
  inheriting from `"estimate_infections"`. This clarifies the
  distinction between fitted models (which contain a Stan fit for
  diagnostics) and forecast simulations (which contain pre-computed
  samples). Dedicated
  [`summary()`](https://epiforecasts.io/EpiNow2/reference/summary.epinow.md),
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html), and
  [`get_samples()`](https://epiforecasts.io/EpiNow2/reference/get_samples.md)
  methods are provided.
- [`estimate_secondary()`](https://epiforecasts.io/EpiNow2/reference/estimate_secondary.md)
  now returns an S3 object of class
  `c("epinowfit", "estimate_secondary", "list")` with elements `fit`,
  `args`, and `observations`, matching the structure of
  [`estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.md).
  - Use `get_samples(object)` to extract formatted posterior samples for
    delay and scaling parameters.
  - Use `get_predictions(object)` to get predicted secondary
    observations. Supports three output formats: `"summary"` (default),
    `"sample"`, and `"quantile"` for
    [scoringutils](https://epiforecasts.io/scoringutils/) integration.
  - Use `summary(object)` to get summarised parameter estimates. Use
    `type = "compact"` for key parameters only, or `type = "parameters"`
    with a `params` argument to select specific parameters.
  - Access the Stan fit directly via `object$fit`, model arguments via
    `object$args`, and observations via `object$observations`.
  - **Deprecated**: The previous return structure with `predictions`,
    `posterior`, and `data` elements is deprecated and will be removed
    in a future release. Backward compatibility is provided with
    deprecation warnings when accessing these elements via `$` or `[[`.
- [`forecast_secondary()`](https://epiforecasts.io/EpiNow2/reference/forecast_secondary.md)
  now returns an independent S3 class `"forecast_secondary"` instead of
  inheriting from `"estimate_secondary"`, with dedicated
  [`get_samples()`](https://epiforecasts.io/EpiNow2/reference/get_samples.md),
  [`get_predictions()`](https://epiforecasts.io/EpiNow2/reference/get_predictions.md),
  and [`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods.
- [`epinow()`](https://epiforecasts.io/EpiNow2/reference/epinow.md) now
  returns an S3 object that inherits from `estimate_infections`, with
  class `c("epinow", "epinowfit", "estimate_infections", "list")`. This
  provides a consistent interface with
  [`estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.md)
  whilst adding epinow-specific computed elements.
  - Use `get_samples(object)` to extract formatted posterior samples.
  - Use `summary(object)` to get a snapshot summary or
    `summary(object, type = "parameters")` for detailed parameter
    estimates.
  - Use `plot(object)` to generate visualisations.
  - Access the Stan fit via `object$fit`, model arguments via
    `object$args`, and observations via `object$observations`.
  - The `output` argument now controls what is saved to disk, not the
    return structure.
  - **Deprecated**: The previous return structure with `$estimates`,
    `$estimated_reported_cases`, `$summary`, `$plots`, and
    `$estimate_infections` elements is deprecated. Backward
    compatibility is provided via `$` and `[[` operators with
    deprecation warnings.
- [`plot.estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/plot.estimate_infections.md)
  and
  [`plot.forecast_infections()`](https://epiforecasts.io/EpiNow2/reference/plot.forecast_infections.md)
  now accept a `CrIs` argument to control which credible intervals are
  displayed.
- Refactored
  [`estimate_truncation()`](https://epiforecasts.io/EpiNow2/reference/estimate_truncation.md)
  to return a proper S3 object with a simplified structure.
  - Renamed return elements: `obs` → `observations`, `data` → `args`.
  - Removed elements: `last_obs` (now included in `observations`), `cmf`
    and `dist`.
  - Use `get_parameters(object)[["truncation"]]` to extract the
    estimated truncation distribution for use in
    [`epinow()`](https://epiforecasts.io/EpiNow2/reference/epinow.md) or
    [`estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.md).
  - Use `get_predictions(object)` to extract truncation-adjusted
    (nowcast) estimates. Supports `"summary"`, `"sample"`, and
    `"quantile"` formats for
    [scoringutils](https://epiforecasts.io/scoringutils/) integration.
  - Use `get_samples(object)` to extract posterior samples.
  - Use `summary(object)` to get parameter estimates as a data.table.
  - **Deprecated**: Accessing `$dist` via `$` or `[[` triggers
    deprecation warnings. Use `get_parameters(x)[["truncation"]]`
    instead.
- Updated
  [`get_parameters()`](https://epiforecasts.io/EpiNow2/reference/get_parameters.md)
  to an S3 generic that works with both `dist_spec` objects (to extract
  fixed parameter values) and fitted model objects from
  [`estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.md),
  [`estimate_secondary()`](https://epiforecasts.io/EpiNow2/reference/estimate_secondary.md),
  and
  [`estimate_truncation()`](https://epiforecasts.io/EpiNow2/reference/estimate_truncation.md).
  For fitted models, it returns posterior distributions as `dist_spec`
  objects, allowing estimated parameters to be used directly as priors
  in subsequent model fits. Use `get_parameters(fit)` to extract all
  parameters as a named list, or `get_parameters(fit)[["truncation"]]`
  for a single parameter.
- Added a `style` argument to
  [`plot_estimates()`](https://epiforecasts.io/EpiNow2/reference/plot_estimates.md)
  and related plot methods to display credible intervals as error bars
  (`"linerange"`) instead of the default ribbons (`"ribbon"`). Error
  bars can be clearer for weekly or aggregated data.
- **Internal**: Stan model delay identifiers have been renamed for
  semantic clarity (`delay_id` → `delay_id_reporting`, `gt_id` →
  `delay_id_generation_time`, `trunc_id` → `delay_id_truncation`). This
  may affect users who access Stan models directly.
- **Internal**: Stan model parameter names have been renamed for clarity
  (`dispersion` → `reporting_overdispersion`, `frac_obs` →
  `fraction_observed`). This simplifies internal code by removing
  post-hoc parameter renaming. This may affect users who access Stan
  models directly or use custom priors with the old parameter names.

### Model changes

- MCMC runs are now initialised with parameter values drawn from a
  distribution that approximates their prior distributions.
- Added an option to compute growth rates using an estimator by Parag et
  al. (2022) based on total infectiousness rather than new infections,
  see `growth_method` argument in
  [`rt_opts()`](https://epiforecasts.io/EpiNow2/reference/rt_opts.md).
- Added support for fitting the susceptible population size.
- Replaced custom rounding function in generated quantities by native
  Stan version.

### Bug fixes

- Fixed an incorrect Rt prior in vignettes (`sd = 0.1` instead of
  `sd = 1`) that caused divergent transitions after changing to
  prior-based MCMC initialisation.
- Fixed `report_log_lik` Stan function using the raw overdispersion
  parameter instead of the transformed phi value, producing incorrect
  pointwise log-likelihood values for model comparison (LOO, WAIC).
- Fixed truncation PMF vector in `estimate_secondary.stan` being
  declared with incorrect dimension, causing a dimension mismatch with
  the `get_delay_rev_pmf()` function call.
- Fixed `CrIs` parameter in
  [`epinow()`](https://epiforecasts.io/EpiNow2/reference/epinow.md) not
  being passed through to internal functions, causing user-specified
  credible intervals to be ignored in saved files and output.
- Fixed
  [`forecast_infections()`](https://epiforecasts.io/EpiNow2/reference/forecast_infections.md)
  failing with `samples = 1`.
- Fixed
  [`opts_list()`](https://epiforecasts.io/EpiNow2/reference/opts_list.md)
  incorrectly recursing lists.
- Fixed shifted cases for the deconvolution model not reflecting
  accumulation settings.
- Fixed
  [`estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.md)
  throwing an error with too many consecutive `NA` observations.
- Fixed error when convolving delay distributions with very small
  values.
- Fixed various log variables (time steps, horizon, samples, chains, and
  iterations) not being reported correctly, especially when using the
  `cmdstanr` backend and `estimate_delays()` function.
- Fixed broken cli warning due to bad syntax.
- Fixed intermediate data being bound together by column position
  instead of column name, leading to erroneous results.
- Fixed Matern 5/2 Gaussian process kernel spectral density
  implementation.

### Documentation

- Updated vignettes to use the current API
  ([`gt_opts()`](https://epiforecasts.io/EpiNow2/reference/generation_time_opts.md)
  instead of
  [`generation_time_opts()`](https://epiforecasts.io/EpiNow2/reference/generation_time_opts.md),
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) instead of
  `$plots$R`,
  [`estimates_by_report_date()`](https://epiforecasts.io/EpiNow2/reference/estimates_by_report_date.md)
  instead of deprecated `output` argument) and disabled logging output
  with `logs = NULL` to prevent output changing between runs.
- Added guidelines for AI-assisted contributions to the CONTRIBUTING
  guide, including transparency requirements, contributor
  responsibilities, and AI-assisted code reviews.
- Added a comprehensive prior choice and specification guide vignette
  covering all three main modelling functions with practical guidance on
  when and how to modify priors.
- Fixed broken `@seealso` links in roxygen2 documentation by converting
  plain text function references to proper link syntax.
- Added documentation about doing prior predictive checks.
- Added full documentation for the Stan code, accessible on the website
  under the Reference tab.
- Fixed an issue with the pkgdown website where the Reference tab was
  not appearing as a dropdown menu for the R and Stan Reference tabs.
- Enhanced the Stan documentation with a doxygen-awesome theme and added
  a licence badge.
- Clarified when the population adjustment is done when `pop` is
  specified.
- Updated workflow vignette to reference `{epidist}` instead of the
  outdated `{dynamicaltruncation}` reference.

## EpiNow2 1.7.1

CRAN release: 2025-02-19

This is a patch release in response to an upstream issue in `rstan`, as
flagged in CRAN checks.

## EpiNow2 1.7.0

CRAN release: 2025-02-05

This release introduces the new accumulation feature, where models can
fitted to data reported at regular or irregular intervals. Moreover, all
priors are now specified using the internal distribution interface.

Internal changes should improve performance, reduce the number of
failing fits, and pave the way for future model flexibility.

### Model changes

- The models now support more complex patterns of aggregating reported
  cases by accumulating them over multiple time points, as well as
  mixtures of accumulation and missingness via the new
  [`fill_missing()`](https://epiforecasts.io/EpiNow2/reference/fill_missing.md)
  function and a logical `accumulate` column that can be supplied with
  the data. If the accumulation frequency is fixed in the data this is
  detected when using
  [`fill_missing()`](https://epiforecasts.io/EpiNow2/reference/fill_missing.md).
  By [@sbfnk](https://github.com/sbfnk) in
  [\#851](https://github.com/epiforecasts/EpiNow2/issues/851) and
  [\#933](https://github.com/epiforecasts/EpiNow2/issues/933) and
  reviewed by [@seabbs](https://github.com/seabbs) and
  [@jamesmbaazam](https://github.com/jamesmbaazam).

  ``` r
  # Deprecated
  data |>
    estimate_infections(obs_opts(na = "accumulate"))

  # Recommended workflow e.g. for weekly incidence data
  data |>
    fill_missing(missing = "accumulate", initial_accumulate = 7) |>
    estimate_infections()
  ```

- A bug was fixed where the initial growth was never estimated (i.e. the
  prior mean was always zero). By [@sbfnk](https://github.com/sbfnk) in
  [\#853](https://github.com/epiforecasts/EpiNow2/issues/853) and
  reviewed by [@seabbs](https://github.com/seabbs).

- A bug was fixed where an internal function for applying a default cdf
  cutoff failed due to a difference a vector length issue. By
  [@jamesmbaazam](https://github.com/jamesmbaazam) in
  [\#858](https://github.com/epiforecasts/EpiNow2/issues/858) and
  reviewed by [@sbfnk](https://github.com/sbfnk).

- All parameters have been changed to the new parameter interface. By
  [@sbfnk](https://github.com/sbfnk) in
  [\#871](https://github.com/epiforecasts/EpiNow2/issues/871) and
  [\#890](https://github.com/epiforecasts/EpiNow2/issues/890) and
  reviewed by [@seabbs](https://github.com/seabbs).

- The Gaussian Process lengthscale is now scaled internally by half the
  length of the time series. By [@sbfnk](https://github.com/sbfnk) in
  [\#890](https://github.com/epiforecasts/EpiNow2/issues/890) and
  reviewed by [@seabbs](https://github.com/seabbs).

- A bug was fixed where
  [`plot.dist_spec()`](https://epiforecasts.io/EpiNow2/reference/plot.dist_spec.md)
  wasn’t throwing an informative error due to an incomplete check for
  the max of the specified delay. By
  [@jamesmbaazam](https://github.com/jamesmbaazam) in
  [\#858](https://github.com/epiforecasts/EpiNow2/issues/858).

- Updated the early dynamics calculation to estimate growth from the
  initial reproduction number instead of a separate linear model. Also
  changed the prior calculation for initial infections to be a scaling
  factor of early case numbers adjusted by the growth estimate, instead
  a true number of initial infections. By
  [@sbfnk](https://github.com/sbfnk) in
  [\#923](https://github.com/epiforecasts/EpiNow2/issues/923) (with
  initial exploration in
  [\#903](https://github.com/epiforecasts/EpiNow2/issues/903)) and
  reviewed by [@seabbs](https://github.com/seabbs) and
  [@SamuelBrand1](https://github.com/SamuelBrand1).

- A bug was fixed in the non-mechanistic model where the final period
  could have discontinuities. By [@sbfnk](https://github.com/sbfnk).

### Package changes

- The internal functions `create_clean_reported_cases()` has been broken
  up into several functions, with relevant ones
  [`filter_leading_zeros()`](https://epiforecasts.io/EpiNow2/reference/filter_leading_zeros.md),
  [`add_breakpoints()`](https://epiforecasts.io/EpiNow2/reference/add_breakpoints.md)
  and
  [`apply_zero_threshold()`](https://epiforecasts.io/EpiNow2/reference/apply_zero_threshold.md)
  exposed to the user. By [@sbfnk](https://github.com/sbfnk) in
  [\#884](https://github.com/epiforecasts/EpiNow2/issues/884) and
  reviewed by [@seabbs](https://github.com/seabbs) and
  [@jamesmbaazam](https://github.com/jamesmbaazam).
- The step of estimating early infections and growth in the internal
  function
  [`create_stan_data()`](https://epiforecasts.io/EpiNow2/reference/create_stan_data.md)
  has been separated into a new internal function
  `estimate_early_dynamics()`. By
  [@jamesmbaazam](https://github.com/jamesmbaazam) in
  [\#888](https://github.com/epiforecasts/EpiNow2/issues/888) and
  reviewed by [@sbfnk](https://github.com/sbfnk).
- [`estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.md)
  and [`epinow()`](https://epiforecasts.io/EpiNow2/reference/epinow.md)
  gain the `forecast` argument for setting the forecast horizon
  (`horizon`) and accumulation of forecasts. `forecast` is set with the
  [`forecast_opts()`](https://epiforecasts.io/EpiNow2/reference/forecast_opts.md)
  function similar to the other settings arguments. By
  [@sbfnk](https://github.com/sbfnk) in
  [\#901](https://github.com/epiforecasts/EpiNow2/issues/901) and
  [@jamesmbaazam](https://github.com/jamesmbaazam) in
  [\#912](https://github.com/epiforecasts/EpiNow2/issues/912) and
  reviewed by each other.

### Documentation

- Brought the docs on `alpha_sd` up to date with the code change from
  prior PR [\#853](https://github.com/epiforecasts/EpiNow2/issues/853).
  By [@zsusswein](https://github.com/zsusswein) in
  [\#862](https://github.com/epiforecasts/EpiNow2/issues/862) and
  reviewed by [@jamesmbaazam](https://github.com/jamesmbaazam).
- The `...` argument in
  [`estimate_secondary()`](https://epiforecasts.io/EpiNow2/reference/estimate_secondary.md)
  has been removed because it was not used. By
  [@jamesmbaazam](https://github.com/jamesmbaazam) in
  [\#894](https://github.com/epiforecasts/EpiNow2/issues/894).
- All examples now use the natural parameters of distributions rather
  than the mean and standard deviation when specifying uncertain
  distributions. This is to eliminate warnings and encourage best
  practice. By [@jamesmbaazam](https://github.com/jamesmbaazam) in
  [\#893](https://github.com/epiforecasts/EpiNow2/issues/893) and
  reviewed by [@sbfnk](https://github.com/sbfnk).
- Updated the methodology vignettes, By
  [@sbfnk](https://github.com/sbfnk) in
  [\#919](https://github.com/epiforecasts/EpiNow2/issues/919) and
  reviewed by [@seabbs](https://github.com/seabbs) and
  [@jamesmbaazam](https://github.com/jamesmbaazam).
- The ways that `dist_spec()` with certain/uncertain parameters can be
  constrained has been clarified. By [@sbfnk](https://github.com/sbfnk)
  in [\#940](https://github.com/epiforecasts/EpiNow2/issues/940) and
  reviewed by [@jamesmbaazam](https://github.com/jamesmbaazam).
- Language in plots and summaries was adapted to reflect potentially
  non-daily data. By [@sbfnk](https://github.com/sbfnk) in
  [\#947](https://github.com/epiforecasts/EpiNow2/issues/947) and
  reviewed by [@seabbs](https://github.com/seabbs).

## EpiNow2 1.6.1

CRAN release: 2024-10-31

### Model changes

- A bug in the spectral densities of Matern kernels of order other than
  3/2 has been fixed and the vignette updated accordingly. By
  [@sbfnk](https://github.com/sbfnk) in
  [\#835](https://github.com/epiforecasts/EpiNow2/issues/835) and
  reviewed by [@seabbs](https://github.com/seabbs).
- Changed the prior on the (square root of the) magnitude alpha of the
  Gaussian Process back to HalfNormal(0, 0.05) based on user feedback of
  unexpected results. By [@sbfnk](https://github.com/sbfnk) in
  [\#840](https://github.com/epiforecasts/EpiNow2/issues/840) and
  reviewed by [@jamesmbaazam](https://github.com/jamesmbaazam).

### Package changes

- Users are now informed that `NA` observations (if present implicitly
  or explicitly) will be treated as missing instead of zero when using
  the default
  [`obs_opts()`](https://epiforecasts.io/EpiNow2/reference/obs_opts.md).
  Options to treat `NA` as zeros or accumulate them are also provided in
  the message. By [@jamesmbaazam](https://github.com/jamesmbaazam) in
  [\#774](https://github.com/epiforecasts/EpiNow2/issues/774) and
  reviewed by [@sbfnk](https://github.com/sbfnk).

## EpiNow2 1.6.0

CRAN release: 2024-10-02

A release that introduces model improvements to the Gaussian Process
models, alongside a number of other improvements and bug fixes.

### Documentation

- The documentation of the `rt` argument has been expanded in the case
  where `rt = NULL` to make explicit the settings that are applied in
  that case. By [@jamesmbaazam](https://github.com/jamesmbaazam) in
  [\#779](https://github.com/epiforecasts/EpiNow2/issues/779) and
  reviewed by [@sbfnk](https://github.com/sbfnk).
- The README has been updated to link to the free course on nowcasting
  and forecasting. The availability of variational inference, Laplace
  approximation, and Pathfinder through `cmdstanr` has also be surfaced.
  By [@jamesmbaazam](https://github.com/jamesmbaazam) in
  [\#753](https://github.com/epiforecasts/EpiNow2/issues/753) and
  reviewed by [@seabbs](https://github.com/seabbs).
- Some implicit argument defaults have been made explicit in the
  function definition. By [@Bisaloo](https://github.com/Bisaloo) in
  [\#729](https://github.com/epiforecasts/EpiNow2/issues/729).
- The installation guide in the README has been updated to provide
  instructions for configuring the C toolchain of Windows, MacOS, and
  Linux. By [@jamesmbaazam](https://github.com/jamesmbaazam) in
  [\#707](https://github.com/epiforecasts/EpiNow2/issues/707) and
  reviewed by [@sbfnk](https://github.com/sbfnk).

### Model changes

- The interface for defining delay distributions has been generalised to
  also cater for continuous distributions
- When defining probability distributions these can now be truncated
  using the `tolerance` argument
- Ornstein-Uhlenbeck and 5 / 2 Matérn kernels have been added. By
  [@sbfnk](https://github.com/sbfnk) in
  [\#741](https://github.com/epiforecasts/EpiNow2/issues/741) and
  reviewed by [@seabbs](https://github.com/seabbs).
- Gaussian processes have been vectorised, leading to some speed gains
  🚀 , and the
  [`gp_opts()`](https://epiforecasts.io/EpiNow2/reference/gp_opts.md)
  function has gained three more options, “periodic”, “ou”, and “se”, to
  specify periodic and linear kernels respectively. By
  [@seabbs](https://github.com/seabbs) in
  [\#742](https://github.com/epiforecasts/EpiNow2/issues/742) and
  reviewed by [@jamesmbaazam](https://github.com/jamesmbaazam).
- Prior predictive checks have been used to update the following priors:
  the prior on the magnitude of the Gaussian process (from HalfNormal(0,
  0.05)^2 to HalfNormal(0, 0.01)^2), and the prior on the overdispersion
  (from 1 / HalfNormal(0, 1)^2 to 1 / HalfNormal(0, 0.25)). In the
  user-facing API, this is a change in default values of the `sd` of
  `phi` in
  [`obs_opts()`](https://epiforecasts.io/EpiNow2/reference/obs_opts.md)
  from 1 to 0.25. By [@seabbs](https://github.com/seabbs) in
  [\#742](https://github.com/epiforecasts/EpiNow2/issues/742) and
  reviewed by [@jamesmbaazam](https://github.com/jamesmbaazam).
- The default stan control options have been updated from
  `list(adapt_delta = 0.95, max_treedepth = 15)` to
  `list(adapt_delta = 0.9, max_treedepth = 12)` due to improved
  performance and to reduce the runtime of the default
  parameterisations. By [@seabbs](https://github.com/seabbs) in
  [\#742](https://github.com/epiforecasts/EpiNow2/issues/742) and
  reviewed by [@jamesmbaazam](https://github.com/jamesmbaazam).
- Initialisation has been simplified by sampling directly from the
  priors, where possible, rather than from a constrained space. By
  [@seabbs](https://github.com/seabbs) in
  [\#742](https://github.com/epiforecasts/EpiNow2/issues/742) and
  reviewed by [@jamesmbaazam](https://github.com/jamesmbaazam).
- Unnecessary normalisation of delay priors has been removed. By
  [@seabbs](https://github.com/seabbs) in
  [\#742](https://github.com/epiforecasts/EpiNow2/issues/742) and
  reviewed by [@jamesmbaazam](https://github.com/jamesmbaazam).
- Ornstein-Uhlenbeck and 5 / 2 Matérn kernels have been added. By
  [@sbfnk](https://github.com/sbfnk) in
  [\#741](https://github.com/epiforecasts/EpiNow2/issues/741) and
  reviewed by [@seabbs](https://github.com/seabbs).
- Switch to broadcasting from random walks and added unit tests. By
  [@seabbs](https://github.com/seabbs) in
  [\#747](https://github.com/epiforecasts/EpiNow2/issues/747) and
  reviewed by [@jamesmbaazam](https://github.com/jamesmbaazam).
- Optimised convolution code to take into account the relative length of
  the vectors being convolved. See
  [\#745](https://github.com/epiforecasts/EpiNow2/issues/745) by
  [@seabbs](https://github.com/seabbs) and reviewed by
  [@jamesmbaazam](https://github.com/jamesmbaazam).
- Switch to broadcasting the day of the week effect. By
  [@seabbs](https://github.com/seabbs) in
  [\#746](https://github.com/epiforecasts/EpiNow2/issues/746) and
  reviewed by [@jamesmbaazam](https://github.com/jamesmbaazam).
- A warning is now thrown if nonparametric PMFs passed to delay options
  have consecutive tail values that are below a certain low threshold as
  these lead to loss in speed with little gain in accuracy. By
  [@jamesmbaazam](https://github.com/jamesmbaazam) in
  [\#752](https://github.com/epiforecasts/EpiNow2/issues/752) and
  reviewed by [@seabbs](https://github.com/seabbs), with a subsequent
  bug fix in
  [\#802](https://github.com/epiforecasts/EpiNow2/issues/802).
- [`dist_fit()`](https://epiforecasts.io/EpiNow2/reference/dist_fit.md)
  can now accept any number of `samples` without throwing a warning when
  `samples` \< 1000 in
  [\#751](https://github.com/epiforecasts/EpiNow2/issues/751) by
  [@jamesmbaazam](https://github.com/jamesmbaazam) and reviewed by
  [@seabbs](https://github.com/seabbs) and
  [@sbfnk](https://github.com/sbfnk).
- The `phi` parameter has been renamed to `dispersion` to be in line
  with the use of phi in the Negative Binomial distribution in the stan
  documentation. By [@sbfnk](https://github.com/sbfnk) in
  [\#969](https://github.com/epiforecasts/EpiNow2/issues/969) and
  reviewed by [@seabbs](https://github.com/seabbs).

### Package changes

- [`epinow()`](https://epiforecasts.io/EpiNow2/reference/epinow.md) now
  returns the “timing” output in a “time difference”” format that is
  easier to understand and work with. By
  [@jamesmbaazam](https://github.com/jamesmbaazam) in
  [\#688](https://github.com/epiforecasts/EpiNow2/issues/688) and
  reviewed by [@sbfnk](https://github.com/sbfnk).
- All functions now use the [cli](https://cli.r-lib.org) R package to
  signal errors, warnings, and messages. By
  [@jamesmbaazam](https://github.com/jamesmbaazam) in
  [\#762](https://github.com/epiforecasts/EpiNow2/issues/762) and
  reviewed by [@seabbs](https://github.com/seabbs).
- `fix_dist()` has been renamed to
  [`fix_parameters()`](https://epiforecasts.io/EpiNow2/reference/fix_parameters.md)
  because it removes the uncertainty in a distribution’s parameters. By
  [@sbfnk](https://github.com/sbfnk) in
  [\#733](https://github.com/epiforecasts/EpiNow2/issues/733) and
  reviewed by [@jamesmbaazam](https://github.com/jamesmbaazam).
- `plot.dist_spec` now uses color instead of line types to display pmfs
  vs cmfs. By [@jamesmbaazam](https://github.com/jamesmbaazam) in
  [\#788](https://github.com/epiforecasts/EpiNow2/issues/788) and
  reviewed by [@sbfnk](https://github.com/sbfnk).
- The use of the [progressr](https://progressr.futureverse.org) package
  for displaying progress bars is now optional, as is the use of
  [future](https://future.futureverse.org) and
  [future.apply](https://future.apply.futureverse.org) for
  parallelisation. By [@sbfnk](https://github.com/sbfnk) in
  [\#798](https://github.com/epiforecasts/EpiNow2/issues/798) and
  reviewed by [@seabbs](https://github.com/seabbs).
- Specifying nonparametric generation time intervals with a nonzero
  first element (corresponding to the zero bin) is being deprecated as
  the current behaviour of setting it to zero internally was not well
  exposed. By [@sbfnk](https://github.com/sbfnk) in \#.

### Bug fixes

- a bug was fixed that caused delay option functions to report an error
  if only the CDF cutoff was specified. By
  [@sbfnk](https://github.com/sbfnk) in
  [\#716](https://github.com/epiforecasts/EpiNow2/issues/716) and
  reviewed by [@jamesmbaazam](https://github.com/jamesmbaazam).
- a bug was fixed where
  [`forecast_secondary()`](https://epiforecasts.io/EpiNow2/reference/forecast_secondary.md)
  did not work with fixed delays. By [@sbfnk](https://github.com/sbfnk)
  in [\#717](https://github.com/epiforecasts/EpiNow2/issues/717) and
  reviewed by [@seabbs](https://github.com/seabbs).
- a bug was fixed that caused delay option functions to report an error
  if only the CDF cutoff was specified. By
  [@sbfnk](https://github.com/sbfnk).
- a bug was fixed that led to the truncation PMF being shortened from
  the wrong side when the truncation PMF was longer than the supplied
  data. By [@seabbs](https://github.com/seabbs) in
  [\#736](https://github.com/epiforecasts/EpiNow2/issues/736) and
  reviewed by [@sbfnk](https://github.com/sbfnk) and
  [@jamesmbaazam](https://github.com/jamesmbaazam).
- a bug was fixed that caused internal validation checks on delay
  distributions to fail if they contained non-parametric distributions.
  By [@jamesmbaazam](https://github.com/jamesmbaazam) in
  [\#750](https://github.com/epiforecasts/EpiNow2/issues/750) and
  reviewed by [@seabbs](https://github.com/seabbs).
- a bug was fixed where combined distributions where shown in
  alphabetical order, rather than the order in which they were combined.
  By [@sbfnk](https://github.com/sbfnk) in
  [\#784](https://github.com/epiforecasts/EpiNow2/issues/784) and
  reviewed by [@jamesmbaazam](https://github.com/jamesmbaazam).

### Documentation

- Updated the documentation of the dots argument of the
  [`stan_sampling_opts()`](https://epiforecasts.io/EpiNow2/reference/stan_sampling_opts.md)
  to add that the dots are passed to
  [`cmdstanr::sample()`](https://mc-stan.org/cmdstanr/reference/model-method-sample.html).
  By [@jamesmbaazam](https://github.com/jamesmbaazam) in
  [\#699](https://github.com/epiforecasts/EpiNow2/issues/699) and
  reviewed by [@sbfnk](https://github.com/sbfnk).
- [`generation_time_opts()`](https://epiforecasts.io/EpiNow2/reference/generation_time_opts.md)
  has been shortened to
  [`gt_opts()`](https://epiforecasts.io/EpiNow2/reference/generation_time_opts.md)
  to make it easier to specify. Calls to both functions are equivalent.
  By [@jamesmbaazam](https://github.com/jamesmbaazam) in
  [\#698](https://github.com/epiforecasts/EpiNow2/issues/698) and
  reviewed by [@seabbs](https://github.com/seabbs) and
  [@sbfnk](https://github.com/sbfnk) .
- Added stan documentation for `update_rt()`. By
  [@seabbs](https://github.com/seabbs) in
  [\#747](https://github.com/epiforecasts/EpiNow2/issues/747) and
  reviewed by [@jamesmbaazam](https://github.com/jamesmbaazam).

## EpiNow2 1.5.2

CRAN release: 2024-05-16

A patch release to further fix an issue with the date in the package
citation. This has now been addressed by removing `inst/CITATION`.

## EpiNow2 1.5.1

CRAN release: 2024-05-14

A patch release to fix an issue with the date in the package citation.

## EpiNow2 1.5.0

CRAN release: 2024-05-10

This release comes with a change of maintainer, from
[@seabbs](https://github.com/seabbs) to
[@sbfnk](https://github.com/sbfnk). This is to reflect who will handle
the upcoming CRAN submission, but is not expected to lead to a change in
workflows.

### Major changes

- The interface to generating delay distributions has been completely
  overhauled. Instead of calling `dist_spec()` users now specify
  distributions using functions that represent the available
  distributions,
  i.e. [`LogNormal()`](https://epiforecasts.io/EpiNow2/reference/Distributions.md),
  [`Gamma()`](https://epiforecasts.io/EpiNow2/reference/Distributions.md)
  and
  [`Fixed()`](https://epiforecasts.io/EpiNow2/reference/Distributions.md).
  See
  [`?EpiNow2::Distributions`](https://epiforecasts.io/EpiNow2/reference/Distributions.md).
  Uncertainty is specified using calls of the same nature, to
  [`Normal()`](https://epiforecasts.io/EpiNow2/reference/Distributions.md).
  More information on the underlying design can be found in
  `inst/dev/design_dist.md` By [@sbfnk](https://github.com/sbfnk) in
  [\#504](https://github.com/epiforecasts/EpiNow2/issues/504) and
  reviewed by [@seabbs](https://github.com/seabbs).
- Delay discretisation is now based on a two-day censoring window (with
  uniform probability in between), based on recommendations in [Park et
  al, medRxiv, 2024](https://doi.org/10.1101/2024.01.12.24301247). By
  [@sbfnk](https://github.com/sbfnk) in
  [\#518](https://github.com/epiforecasts/EpiNow2/issues/518) and
  reviewed by [@jamesmbaazam](https://github.com/jamesmbaazam).

### Deprecations

- The functions `sample_approx_dist()`, `report_cases()`, and
  `adjust_infection_reports()` have been deprecated as the functionality
  they provide can now be achieved with
  [`simulate_secondary()`](https://epiforecasts.io/EpiNow2/reference/simulate_secondary.md).
  See [\#597](https://github.com/epiforecasts/EpiNow2/issues/597) by
  [@jamesmbaazam](https://github.com/jamesmbaazam) and reviewed by
  [@sbfnk](https://github.com/sbfnk).
- The utility function `update_list()` has been deprecated in favour of
  [`utils::modifyList()`](https://rdrr.io/r/utils/modifyList.html)
  because it comes with an installation of R. By
  [@jamesmbaazam](https://github.com/jamesmbaazam) in
  [\#491](https://github.com/epiforecasts/EpiNow2/issues/491) and
  reviewed by [@seabbs](https://github.com/seabbs).
- The `fixed` argument to `dist_spec` has been deprecated and replaced
  by a `fix_dist()` function. By [@sbfnk](https://github.com/sbfnk) in
  [\#503](https://github.com/epiforecasts/EpiNow2/issues/503) and
  reviewed by [@seabbs](https://github.com/seabbs).
- The functions `get_dist()`, `get_generation_time()`,
  `get_incubation_period()` have been deprecated and replaced with
  examples. By [@sbfnk](https://github.com/sbfnk) in
  [\#481](https://github.com/epiforecasts/EpiNow2/issues/481) and
  reviewed by [@seabbs](https://github.com/seabbs).
- The function `init_cumulative_fit()` has been deprecated. By
  [@jamesmbaazam](https://github.com/jamesmbaazam) in
  [\#541](https://github.com/epiforecasts/EpiNow2/issues/541) and
  reviewed by [@sbfnk](https://github.com/sbfnk).
- The model-specific `weigh_delay_priors` argument has been deprecated
  in favour of delay-specific prior weighting using `weight_priors`. See
  [`generation_time_opts()`](https://epiforecasts.io/EpiNow2/reference/generation_time_opts.md),
  [`delay_opts()`](https://epiforecasts.io/EpiNow2/reference/delay_opts.md),
  and
  [`trunc_opts()`](https://epiforecasts.io/EpiNow2/reference/trunc_opts.md).
  By [@sbfnk](https://github.com/sbfnk) in
  [\#630](https://github.com/epiforecasts/EpiNow2/issues/630) and
  reviewed by [@jamesmbaazam](https://github.com/jamesmbaazam).
- All functions now use a `data` argument to pass data. The existing
  `reported_cases`, `reports`, and `obs` arguments are deprecated and
  will be removed in v2.0.0. By
  [@jamesmbaazam](https://github.com/jamesmbaazam) in
  [\#638](https://github.com/epiforecasts/EpiNow2/issues/638) and
  reviewed by [@sbfnk](https://github.com/sbfnk).

### Other breaking changes

- Updated
  [`estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.md)
  so that rather than imputing missing data, it now skips these data
  points in the likelihood. This is a breaking change as it alters the
  behaviour of the model when dates are missing from a time series but
  are known to be zero. We recommend that users check their results when
  updating to this version but expect this to in most cases improve
  performance. By [@seabbs](https://github.com/seabbs) in
  [\#528](https://github.com/epiforecasts/EpiNow2/issues/528) and
  reviewed by [@sbfnk](https://github.com/sbfnk).
- `simulate_infections` has been renamed to `forecast_infections` in
  line with `simulate_secondary` and `forecast_secondary`. The
  terminology is: a forecast is done from a fit to existing data, a
  simulation from first principles. By
  [@sbfnk](https://github.com/sbfnk) in
  [\#544](https://github.com/epiforecasts/EpiNow2/issues/544) and
  reviewed by [@seabbs](https://github.com/seabbs).
- A new `simulate_infections` function has been added that can be used
  to simulate from the model from given initial conditions and
  parameters. By [@sbfnk](https://github.com/sbfnk) in
  [\#557](https://github.com/epiforecasts/EpiNow2/issues/557) and
  reviewed by [@jamesmbaazam](https://github.com/jamesmbaazam).
- The function `init_cumulative_fit()` has been deprecated. By
  [@jamesmbaazam](https://github.com/jamesmbaazam) in
  [\#541](https://github.com/epiforecasts/EpiNow2/issues/541) and
  reviewed by [@sbfnk](https://github.com/sbfnk).
- The interface to generating delay distributions has been completely
  overhauled. Instead of calling `dist_spec()` users now specify
  distributions using functions that represent the available
  distributions,
  i.e. [`LogNormal()`](https://epiforecasts.io/EpiNow2/reference/Distributions.md),
  [`Gamma()`](https://epiforecasts.io/EpiNow2/reference/Distributions.md)
  and
  [`Fixed()`](https://epiforecasts.io/EpiNow2/reference/Distributions.md).
  Uncertainty is specified using calls of the same nature, to
  [`Normal()`](https://epiforecasts.io/EpiNow2/reference/Distributions.md).
  More information on the underlying design can be found in
  `inst/dev/design_dist.md` By [@sbfnk](https://github.com/sbfnk) in
  [\#504](https://github.com/epiforecasts/EpiNow2/issues/504) and
  reviewed by [@seabbs](https://github.com/seabbs).
- The accessor functions
  [`get_parameters()`](https://epiforecasts.io/EpiNow2/reference/get_parameters.md),
  [`get_pmf()`](https://epiforecasts.io/EpiNow2/reference/get_pmf.md),
  and
  [`get_distribution()`](https://epiforecasts.io/EpiNow2/reference/get_distribution.md)
  have been added to extract elements of a object. By
  [@sbfnk](https://github.com/sbfnk) in
  [\#646](https://github.com/epiforecasts/EpiNow2/issues/646) and
  reviewed by [@jamesmbaazam](https://github.com/jamesmbaazam).
- The functions `sample_approx_dist()`, `report_cases()`, and
  `adjust_infection_reports()` have been deprecated as the functionality
  they provide can now be achieved with
  [`simulate_secondary()`](https://epiforecasts.io/EpiNow2/reference/simulate_secondary.md).
  See [\#597](https://github.com/epiforecasts/EpiNow2/issues/597) by
  [@jamesmbaazam](https://github.com/jamesmbaazam) and reviewed by
  [@sbfnk](https://github.com/sbfnk).

### Documentation

- Two new vignettes have been added to cover the workflow and example
  uses. By [@sbfnk](https://github.com/sbfnk) in
  [\#458](https://github.com/epiforecasts/EpiNow2/issues/458) and
  reviewed by [@jamesmbaazam](https://github.com/jamesmbaazam).
- Removed references to the no longer existing `forecast_infections`
  function. By [@sbfnk](https://github.com/sbfnk) in
  [\#460](https://github.com/epiforecasts/EpiNow2/issues/460) and
  reviewed by [@seabbs](https://github.com/seabbs).
- The contribution guide has been improved to include more detail on
  ways to contribute new features/enhancements, report bugs, and improve
  or suggest vignettes. By
  [@jamesmbaazam](https://github.com/jamesmbaazam) in
  [\#464](https://github.com/epiforecasts/EpiNow2/issues/464) and
  reviewed by [@seabbs](https://github.com/seabbs).
- Updated the code in `inst/CITATION` and added a GitHub Actions
  workflow to auto-generate `citation.cff` so that the two citation
  files are always in sync with `DESCRIPTION`. By
  [@jamesmbazam](https://github.com/jamesmbazam) in
  [\#467](https://github.com/epiforecasts/EpiNow2/issues/467), with
  contributions from [@Bisaloo](https://github.com/Bisaloo), and
  reviewed by [@seabbs](https://github.com/seabbs) and
  [@sbfnk](https://github.com/sbfnk).
- Updated the documentation of the `data` argument in
  [`estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.md)
  and `confirm` column in the `obs` argument of
  [`estimate_truncation()`](https://epiforecasts.io/EpiNow2/reference/estimate_truncation.md)
  to allow `numeric` types, not just `integer`. See
  [\#594](https://github.com/epiforecasts/EpiNow2/issues/594), by
  [@jamesmbaazam](https://github.com/jamesmbaazam), and reviewed by
  [@sbfnk](https://github.com/sbfnk).
- Removed the reporting templates that were previously provided. See
  [\#604](https://github.com/epiforecasts/EpiNow2/issues/604) by
  [@jamesmbaazam](https://github.com/jamesmbaazam), and reviewed by
  [@sbfnk](https://github.com/sbfnk).
- Clarified how estimated or specified uncertainty around data
  truncation can be passed to
  [`epinow()`](https://epiforecasts.io/EpiNow2/reference/epinow.md),
  [`regional_epinow()`](https://epiforecasts.io/EpiNow2/reference/regional_epinow.md),
  and
  [`estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.md)
  using the `truncation` argument. By
  [@jamesmbaazam](https://github.com/jamesmbaazam) in
  [\#644](https://github.com/epiforecasts/EpiNow2/issues/644) and
  reviewed by [@sbnfk](https://github.com/sbnfk).
- Internal functions have been removed from the pkgdown index. By
  [@sbfnk](https://github.com/sbfnk) in
  [\#735](https://github.com/epiforecasts/EpiNow2/issues/735).

### Package

- Replaced use of
  [`purrr::transpose()`](https://purrr.tidyverse.org/reference/transpose.html)
  with
  [`purrr::list_transpose()`](https://purrr.tidyverse.org/reference/list_transpose.html)
  because the former is superseded. By
  [@jamesmbaazam](https://github.com/jamesmbaazam) in
  [\#524](https://github.com/epiforecasts/EpiNow2/issues/524) and
  reviewed by [@seabbs](https://github.com/seabbs).
- Reduced the number of long-running examples. By
  [@sbfnk](https://github.com/sbfnk) in
  [\#459](https://github.com/epiforecasts/EpiNow2/issues/459) and
  reviewed by [@seabbs](https://github.com/seabbs).
- Changed all instances of arguments that refer to the maximum of a
  distribution to reflect the maximum. Previously this did, in some
  instance, refer to the length of the PMF. By
  [@sbfnk](https://github.com/sbfnk) in
  [\#468](https://github.com/epiforecasts/EpiNow2/issues/468).
- Fixed a bug in the bounds of delays when setting initial conditions.
  By [@sbfnk](https://github.com/sbfnk) in
  [\#474](https://github.com/epiforecasts/EpiNow2/issues/474).
- Added input checking to
  [`estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.md),
  [`estimate_secondary()`](https://epiforecasts.io/EpiNow2/reference/estimate_secondary.md),
  [`estimate_truncation()`](https://epiforecasts.io/EpiNow2/reference/estimate_truncation.md),
  [`simulate_infections()`](https://epiforecasts.io/EpiNow2/reference/simulate_infections.md),
  and [`epinow()`](https://epiforecasts.io/EpiNow2/reference/epinow.md).
  [`check_reports_valid()`](https://epiforecasts.io/EpiNow2/reference/check_reports_valid.md)
  has been added to validate the reports dataset passed to these
  functions. Tests are added to check
  [`check_reports_valid()`](https://epiforecasts.io/EpiNow2/reference/check_reports_valid.md).
  As part of input validation, the various `*_opts()` functions now
  return subclasses of the same name as the functions and are tested
  against passed arguments to ensure the right `*_opts()` is passed to
  the right argument. For example, the `obs` argument in
  [`estimate_secondary()`](https://epiforecasts.io/EpiNow2/reference/estimate_secondary.md)
  is expected to only receive arguments passed through
  [`obs_opts()`](https://epiforecasts.io/EpiNow2/reference/obs_opts.md)
  and will error otherwise. By
  [@jamesmbaazam](https://github.com/jamesmbaazam) in
  [\#476](https://github.com/epiforecasts/EpiNow2/issues/476) and
  reviewed by [@sbfnk](https://github.com/sbfnk) and
  [@seabbs](https://github.com/seabbs).
- Added the possibility of specifying a fixed observation scaling. By
  [@sbfnk](https://github.com/sbfnk) in
  [\#550](https://github.com/epiforecasts/EpiNow2/issues/550) and
  reviewed by [@seabbs](https://github.com/seabbs).
- Added the possibility of specifying fixed overdispersion. By
  [@sbfnk](https://github.com/sbfnk) in
  [\#560](https://github.com/epiforecasts/EpiNow2/issues/560) and
  reviewed by [@seabbs](https://github.com/seabbs).
- The example in
  [`estimate_truncation()`](https://epiforecasts.io/EpiNow2/reference/estimate_truncation.md)
  has been simplified. The package now ships with a dataset
  `example_truncated`, which is used in the
  [`estimate_truncation()`](https://epiforecasts.io/EpiNow2/reference/estimate_truncation.md)
  example and tests. The steps for creating the `example_truncated` is
  stored in `./data-raw/estimate-truncation.R`. By
  [@jamesmbaazam](https://github.com/jamesmbaazam) in
  [\#584](https://github.com/epiforecasts/EpiNow2/issues/584) and
  reviewed by [@seabbs](https://github.com/seabbs) and
  [@sbfnk](https://github.com/sbfnk).
- Tests have been updated to only set random seeds before snapshot tests
  involving random number generation, and unset them subsequently. By
  [@sbfnk](https://github.com/sbfnk) in
  [\#590](https://github.com/epiforecasts/EpiNow2/issues/590) and
  reviewed by [@seabbs](https://github.com/seabbs).
- A function
  [`simulate_secondary()`](https://epiforecasts.io/EpiNow2/reference/simulate_secondary.md)
  was added to simulate from parameters of the `estimate_secondary`
  model. A function of the same name that was previously based on a
  reimplementation of that model in R with potentially time-varying
  scalings and delays has been renamed to
  [`convolve_and_scale()`](https://epiforecasts.io/EpiNow2/reference/convolve_and_scale.md).
  By [@sbfnk](https://github.com/sbfnk) in
  [\#591](https://github.com/epiforecasts/EpiNow2/issues/591) and
  reviewed by [@seabbs](https://github.com/seabbs).
- Fixed broken links in the README. By
  [@jamesmbaazam](https://github.com/jamesmbaazam) in
  [\#617](https://github.com/epiforecasts/EpiNow2/issues/617) and
  reviewed by [@sbfnk](https://github.com/sbfnk).
- Replaced descriptions and plot labels to be more general and clearer.
  By [@sbfnk](https://github.com/sbfnk) in
  [\#621](https://github.com/epiforecasts/EpiNow2/issues/621) and
  reviewed by [@jamesmbaazam](https://github.com/jamesmbaazam).
- Argument choices have been moved into default arguments. By
  [@sbfnk](https://github.com/sbfnk) in
  [\#622](https://github.com/epiforecasts/EpiNow2/issues/622) and
  reviewed by [@seabbs](https://github.com/seabbs).
- [`simulate_infections()`](https://epiforecasts.io/EpiNow2/reference/simulate_infections.md)
  gained the argument `seeding_time` to change the seeding time.
  Additionally, the documentation was improved. By
  [@sbfnk](https://github.com/sbfnk) in
  [\#627](https://github.com/epiforecasts/EpiNow2/issues/627) and
  reviewed by [@jamesmbaazam](https://github.com/jamesmbaazam).
- A `cmdstanr` backend has been added. By
  [@sbfnk](https://github.com/sbfnk) in
  [\#537](https://github.com/epiforecasts/EpiNow2/issues/537) and
  [\#642](https://github.com/epiforecasts/EpiNow2/issues/642) and
  reviewed by [@seabbs](https://github.com/seabbs).

### Model changes

- Updated the parameterisation of the dispersion term `phi` to be
  `phi = 1 / sqrt_phi ^ 2` rather than the previous parameterisation
  `phi = 1 / sqrt(sqrt_phi)` based on the suggested prior
  [here](https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations#story-when-the-generic-prior-fails-the-case-of-the-negative-binomial)
  and the performance benefits seen in the `epinowcast` package (see
  [here](https://github.com/epinowcast/epinowcast/blob/8eff560d1fd8305f5fb26c21324b2bfca1f002b4/inst/stan/epinowcast.stan#L314)).
  By [@seabbs](https://github.com/seabbs) in
  [\#487](https://github.com/epiforecasts/EpiNow2/issues/487) and
  reviewed by [@sbfnk](https://github.com/sbfnk).
- Added an `na` argument to
  [`obs_opts()`](https://epiforecasts.io/EpiNow2/reference/obs_opts.md)
  that allows the user to specify whether NA values in the data should
  be interpreted as missing or accumulated in the next non-NA data
  point. By [@sbfnk](https://github.com/sbfnk) in
  [\#534](https://github.com/epiforecasts/EpiNow2/issues/534) and
  reviewed by [@seabbs](https://github.com/seabbs).
- Growth rates are now calculated directly from the infection trajectory
  as `log I(t) - log I(t - 1)`. Originally by
  [@seabbs](https://github.com/seabbs) in
  [\#213](https://github.com/epiforecasts/EpiNow2/issues/213), finished
  by [@sbfnk](https://github.com/sbfnk) in
  [\#610](https://github.com/epiforecasts/EpiNow2/issues/610) and
  reviewed by [@seabbs](https://github.com/seabbs).
- Fixed a bug when using the nonmechanistic model that could lead to
  explosive growth. By [@sbfnk](https://github.com/sbfnk) in
  [\#612](https://github.com/epiforecasts/EpiNow2/issues/612) and
  reviewed by [@jamesmbaazam](https://github.com/jamesmbaazam).
- Added the arguments `filter_leading_zeros` and `zero_threshold` to
  [`estimate_secondary()`](https://epiforecasts.io/EpiNow2/reference/estimate_secondary.md)
  and
  [`estimate_truncation()`](https://epiforecasts.io/EpiNow2/reference/estimate_truncation.md)
  to allow the user to specify whether to filter leading zeros in the
  data and the threshold for replacing zero cases. These arguments were
  already used in
  [`estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.md),
  [`epinow()`](https://epiforecasts.io/EpiNow2/reference/epinow.md), and
  [`regional_epinow()`](https://epiforecasts.io/EpiNow2/reference/regional_epinow.md).
  See
  [`?estimate_secondary`](https://epiforecasts.io/EpiNow2/reference/estimate_secondary.md)
  and
  [`?estimate_truncation`](https://epiforecasts.io/EpiNow2/reference/estimate_truncation.md)
  for more details. By [@jamesmbaazam](https://github.com/jamesmbaazam)
  in [\#608](https://github.com/epiforecasts/EpiNow2/issues/608) and
  reviewed by [@sbfnk](https://github.com/sbfnk).

## EpiNow2 1.4.0

CRAN release: 2023-09-26

This release contains some bug fixes, minor new features, and the
initial stages of some broader improvement to future handling of delay
distributions.

### Breaking changes

- The external distribution interface has been updated to use the
  `dist_spec()` function. This comes with a range of benefits, including
  optimising model fitting when static delays are used (by convolving
  when first defined vs in stan), easy printing (using
  [`print()`](https://rdrr.io/r/base/print.html)), and easy plotting
  (using [`plot()`](https://rdrr.io/r/graphics/plot.default.html)). It
  also makes it possible to use all supported distributions everywhere
  (i.e, as a generation time or reporting delay). However, while for now
  backwards compatibility has been ensured this update will break most
  users’ code eventually as the interface has changed. See the
  documentation for `dist_spec()` for more details. By
  [@sbfnk](https://github.com/sbfnk) in
  [\#363](https://github.com/epiforecasts/EpiNow2/issues/363) and
  reviewed by [@seabbs](https://github.com/seabbs).

### Package

- Model description has been expanded to include more detail. By
  [@sbfnk](https://github.com/sbfnk) in
  [\#373](https://github.com/epiforecasts/EpiNow2/issues/373) and
  reviewed by [@seabbs](https://github.com/seabbs).
- Moved to a GitHub Action to only lint changed files. By
  [@seabbs](https://github.com/seabbs) in
  [\#378](https://github.com/epiforecasts/EpiNow2/issues/378).
- Linted the package with a wider range of default linters. By
  [@seabbs](https://github.com/seabbs) in
  [\#378](https://github.com/epiforecasts/EpiNow2/issues/378).
- Added a GitHub Action to build the README when it is altered. By
  [@seabbs](https://github.com/seabbs).
- Added handling of edge case where we sample from the negative binomial
  with mean close or equal to 0. By [@sbfnk](https://github.com/sbfnk)
  in [\#366](https://github.com/epiforecasts/EpiNow2/issues/366) and
  reviewed by [@seabbs](https://github.com/seabbs).
- Replaced use of nested
  [`ifelse()`](https://rdrr.io/r/base/ifelse.html) and
  [`data.table::fifelse()`](https://rdrr.io/pkg/data.table/man/fifelse.html)
  in the code base with
  [`data.table::fcase()`](https://rdrr.io/pkg/data.table/man/fcase.html).
  By [@jamesmbaazam](https://github.com/jamesmbaazam) in
  [\#383](https://github.com/epiforecasts/EpiNow2/issues/383) and
  reviewed by [@seabbs](https://github.com/seabbs).
- Reviewed the example in `calc_backcalc_data()` to call
  `calc_backcalc_data()` instead of
  [`create_gp_data()`](https://epiforecasts.io/EpiNow2/reference/create_gp_data.md).
  By [@jamesmbaazam](https://github.com/jamesmbaazam) in
  [\#388](https://github.com/epiforecasts/EpiNow2/issues/388) and
  reviewed by [@seabbs](https://github.com/seabbs).
- Improved compilation times by reducing the number of distinct stan
  models and deprecated `tune_inv_gamma()`. By
  [@sbfnk](https://github.com/sbfnk) in
  [\#394](https://github.com/epiforecasts/EpiNow2/issues/394) and
  reviewed by [@seabbs](https://github.com/seabbs).
- Changed touchstone settings so that benchmarks are only performed if
  the stan model is changed. By [@sbfnk](https://github.com/sbfnk) in
  [\#400](https://github.com/epiforecasts/EpiNow2/issues/400) and
  reviewed by [@seabbs](https://github.com/seabbs).
- [pak](https://pak.r-lib.org/) is now suggested for installing the
  developmental version of the package. By
  [@jamesmbaazam](https://github.com/jamesmbaazam) in
  [\#407](https://github.com/epiforecasts/EpiNow2/issues/407) and
  reviewed by [@seabbs](https://github.com/seabbs). This has been
  successfully tested on MacOS Ventura, Ubuntu 20.04, and Windows 10.
  Users are advised to use
  `remotes::install_github("epiforecasts/EpiNow2")` if `pak` fails and
  if both fail, raise an issue.
- [`dist_fit()`](https://epiforecasts.io/EpiNow2/reference/dist_fit.md)’s
  `samples` argument now takes a default value of 1000 instead of NULL.
  If a supplied `samples` is less than 1000, it is changed to 1000 and a
  warning is thrown to indicate the change. By
  [@jamesmbazam](https://github.com/jamesmbazam) in
  [\#389](https://github.com/epiforecasts/EpiNow2/issues/389) and
  reviewed by [@seabbs](https://github.com/seabbs).
- The internal distribution interface has been streamlined to reduce
  code duplication. By [@sbfnk](https://github.com/sbfnk) in
  [\#363](https://github.com/epiforecasts/EpiNow2/issues/363) and
  reviewed by [@seabbs](https://github.com/seabbs).
- A small bug has been fixed where the seeding time was too long. When a
  single delay is used this shortens the seeding time by one day and
  when more delays are used it shortens the seeding time by n days where
  n is the number of delays used e.g. for two parametric delays it’s two
  days. By [@sbfnk](https://github.com/sbfnk) in
  [\#413](https://github.com/epiforecasts/EpiNow2/issues/413) and
  reviewed by [@seabbs](https://github.com/seabbs).
- Some tuning was done to speed up the renewal model. By
  [@sbfnk](https://github.com/sbfnk) in
  [\#416](https://github.com/epiforecasts/EpiNow2/issues/416) and
  reviewed by [@seabbs](https://github.com/seabbs).
- An approximation of the negative binomial by the Poisson at low levels
  of overdispersion was disabled as it led to parameter identification
  issues. By [@sbfnk](https://github.com/sbfnk) in
  [\#432](https://github.com/epiforecasts/EpiNow2/issues/432) and
  reviewed by [@seabbs](https://github.com/seabbs).
- Reduced verbosity of tests. By [@sbfnk](https://github.com/sbfnk) in
  [\#433](https://github.com/epiforecasts/EpiNow2/issues/433) and
  reviewed by [@seabbs](https://github.com/seabbs).
- Updated code style in response to lintr warnings. By
  [@sbfnk](https://github.com/sbfnk) in
  [\#437](https://github.com/epiforecasts/EpiNow2/issues/437) and
  reviewed by [@seabbs](https://github.com/seabbs).
- Fixed an edge case breaking summary output. Reported by
  [@jrcpulliam](https://github.com/jrcpulliam), fixed by
  [@sbfnk](https://github.com/sbfnk) in
  [\#436](https://github.com/epiforecasts/EpiNow2/issues/436) and
  reviewed by [@seabbs](https://github.com/seabbs).
- Added content to the vignette for the estimate_truncation model. By
  [@sbfnk](https://github.com/sbfnk) in
  [\#439](https://github.com/epiforecasts/EpiNow2/issues/439) and
  reviewed by [@seabbs](https://github.com/seabbs).
- Added a feature to the `estimate_truncation` to allow it to be applied
  to time series that are shorter than the truncation max. By
  [@sbfnk](https://github.com/sbfnk) in
  [\#438](https://github.com/epiforecasts/EpiNow2/issues/438) and
  reviewed by [@seabbs](https://github.com/seabbs).
- Changed the `estimate_truncation` to use the `dist_spec` interface,
  deprecating existing options `max_trunc` and `trunc_dist`. By
  [@sbfnk](https://github.com/sbfnk) in
  [\#448](https://github.com/epiforecasts/EpiNow2/issues/448) and
  [\#452](https://github.com/epiforecasts/EpiNow2/issues/452) and
  reviewed by [@seabbs](https://github.com/seabbs).
- Added a `weigh_delay_priors` argument to the main functions, allowing
  the users to choose whether to weigh delay priors by the number of
  data points or not. By [@sbfnk](https://github.com/sbfnk) in
  [\#450](https://github.com/epiforecasts/EpiNow2/issues/450) and
  reviewed by [@seabbs](https://github.com/seabbs).

### Documentation

- Added a link to the recent CSTE workshop on using `EpiNow2` to the
  case studies vignette. By [@seabbs](https://github.com/seabbs) in
  [\#441](https://github.com/epiforecasts/EpiNow2/issues/441) and
  reviewed by [@sbfnk](https://github.com/sbfnk).

## EpiNow2 1.3.5

CRAN release: 2023-04-27

This is a minor release to resolve issues with the recent CRAN
requirement to make use of a C++ 17 compiler which has been causing
[issues with the `rstantools`
package](https://github.com/stan-dev/rstantools/pull/100).

## EpiNow2 1.3.4

CRAN release: 2023-02-12

This release focusses on bug fixes and package infrastructure updates
along with a few quality of life improvements such as enabling the use
of fixed delays and generation times.

Thanks to [@seabbs](https://github.com/seabbs), and
[@sbfnk](https://github.com/sbfnk) and for the South African Centre for
Epidemiological Modelling and Analysis (SACEMA) for hosting
[@seabbs](https://github.com/seabbs) whilst some of the development work
on this release was being done.

### Breaking changes

- To streamline the interface with the definition of delay distributions
  [`trunc_opts()`](https://epiforecasts.io/EpiNow2/reference/trunc_opts.md)
  now takes a single argument (`dist`) which defines the truncation
  delay rather than a arbitrary list of arguments (which was previously
  used to define the distribution).
- Updated the handling of generation times in the renewal equation to be
  left truncation adjusted for the zeroth day. This more better the
  approach taken to estimate generation times but may slightly impact
  estimates vs those produced using previous versions.
- The range of the `frac_obs` parameter has restricted with an upper
  bound of 1 to reflect its name and description. This may impact a
  small number of edge case uses with the majority being models fit
  using
  [`estimate_secondary()`](https://epiforecasts.io/EpiNow2/reference/estimate_secondary.md).
  By [@sbfnk](https://github.com/sbfnk) in
  [\#340](https://github.com/epiforecasts/EpiNow2/issues/340).

### Features

- Adds a new function
  [`simulate_secondary()`](https://epiforecasts.io/EpiNow2/reference/simulate_secondary.md)
  for simulating secondary observations under the generative process
  model assumed by `estimate_secondary`. Unlike
  [`forecast_secondary()`](https://epiforecasts.io/EpiNow2/reference/forecast_secondary.md)
  which uses a `stan` model to simulate secondary cases (which shares
  code with the `estimate_secondary` model) this new function is
  implemented entirely in R and is therefore useful to sense check any
  results from the `stan` implementation.
- Adds support for fixed delays (mean only or fixed lognormal
  distributed) or truncations (fixed lognormal distributed), and for
  pre-computing these delays as well as generation times if they are
  fixed. By [@sbfnk](https://github.com/sbfnk) and
  [@seabbs](https://github.com/seabbs).
- Support for gamma distributed delays and log-normal distributed
  generation times.

### Package

- Update the GitHub Action files to new versions.
- Switched to using [`seq_along()`](https://rdrr.io/r/base/seq.html)
  rather than `1:length()` in all package code.
- Fixed a broken example in the documentation for
  [`regional_runtimes()`](https://epiforecasts.io/EpiNow2/reference/regional_runtimes.md).
- Add compatibility changes for the latest version of `rstan` and
  `rstantools`.
- Remove legacy use of `pkgnet` for package dependency visualisation.
- Restyled all code using `styler`.
- Dropped dependency on `RcppParallel`.
- Updated `report_cases` to work with the new `delay_opts` helper
  function.
- Added test coverage for `report_cases` though note this function will
  likely be deprecated in future releases.
- Switched to `linewidth` in `plot_CrIs` rather than `size` to avoid
  issues with `ggplot2` 3.4.0.
- Set up validation against synthetic data to run as a CI check.
- Added tests for internal stan convolution functions.
- Update all `get_` distribution functions to return the distribution as
  well as summary parameters.
- Added support for model fitting benchmarking using `touchstone`.

### Documentation

- Slight edits to the model outline for
  [`estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.md).
- Updated examples to make use of fixed distributions.

### Bugs

- Fixed a bug in
  [`simulate_infections()`](https://epiforecasts.io/EpiNow2/reference/simulate_infections.md)
  where passing a custom number of samples would cause the input vector
  of R values to be replicated in a column-wise fashion meaning that the
  intended R trajectory was not simulated.
- Fixed a bug in the
  [`estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.md)
  deconvolution model where the generation time was not correctly being
  reversed.

## EpiNow2 1.3.3

This release adds a range of new minor features, squashes bugs, enhances
documentation, expands unit testing, implements some minor run-time
optimisations, and removes some obsolete features.

Thanks to [@Bisaloo](https://github.com/Bisaloo),
[@hsbadr](https://github.com/hsbadr),
[@LloydChapman](https://github.com/LloydChapman),
[@medewitt](https://github.com/medewitt), and
[@sbfnk](https://github.com/sbfnk) for contributing to this release.

Thanks to [@sbfnk](https://github.com/sbfnk),
[@pearsonca](https://github.com/pearsonca), and
[@nicholasdavies](https://github.com/nicholasdavies) for regression
testing this release against `1.3.2`.

### New features

- Added supported to `simulate_infections` so that a `data.frame` of R
  samples can be passed in instead of a vector of R values. By
  [@seabbs](https://github.com/seabbs).
- Added extraction of posterior samples to the summary method for
  `estimate_infections`. By [@seabbs](https://github.com/seabbs).
- Exposed `zero_threshold` to users allowing for control over when zeros
  or NAs in count data are treated as true zeros versus as reporting
  errors that require some smoothing. By
  [@seabbs](https://github.com/seabbs).
- Added support for varying the length of the day of the week effect
  (see
  [`obs_opts()`](https://epiforecasts.io/EpiNow2/reference/obs_opts.md)).
  This allows, for example, fitting to data with cases only reported
  every 3 days. By [@seabbs](https://github.com/seabbs).
- Adds option to
  [`plot_estimates()`](https://epiforecasts.io/EpiNow2/reference/plot_estimates.md)
  and higher level functions to choose which estimate type to plot. By
  [@seabbs](https://github.com/seabbs).
- Adds support for fixed generation times (either mean only or fixed
  gamma distributed). By [@sbfnk](https://github.com/sbfnk).
- Adds support for optionally using an inverse gamma prior for the
  lengthscale of the gaussian process. This scaled prior has been tested
  for both short and long simulations where the default prior may make
  the model unstable. The new prior is more stable for long simulations
  and adaptively change the distribution based on the simulation length
  (total number of days) without relying on the user inputs or the fixed
  defaults. It can be tested by setting ls_sd = 0 in gp_opts(). By
  [@hsbadr](https://github.com/hsbadr).
- Updated the prior on the magnitude of the gaussian process to be 0.05
  vs 0.1 leading to slightly more stable estimates. By
  [@hsbadr](https://github.com/hsbadr).
- Added an argument (`plot`) to `regional_summary` to allow plotting to
  be optional. Closes
  [\#250](https://github.com/epiforecasts/EpiNow2/issues/250). By
  [@seabbs](https://github.com/seabbs) in
  [\#317](https://github.com/epiforecasts/EpiNow2/issues/317)

### Model changes

- Minor optimisations in the observation model by only using the
  `target` likelihood definition approach when required and in the use
  of `fmax` and `fmin` over using if statements. By
  [@seabbs](https://github.com/seabbs).
- Added support for users setting the overdispersion (parameterised as
  one over the square root of phi) of the reporting process. This is
  accessible via the `phi` argument of `obs_opts` with the default of a
  normal distribution with mean 0 and standard deviation of 1 truncated
  at 0 remaining unchanged. By [@seabbs](https://github.com/seabbs).
- Added additive noise term to the `estimate_truncation` model to deal
  with zeroes. By [@sbfnk](https://github.com/sbfnk).
- Switched to using optimised versions of the discretised distributions
  supported for the reporting delay and the generation time. These are
  based on an implementation in
  [`epinowcast`](https://package.epinowcast.org/) by Adrian Lison and
  Sam Abbott. By [@seabbs](https://github.com/seabbs) in
  [\#320](https://github.com/epiforecasts/EpiNow2/issues/320).

### Documentation

- Updates to all synthetic delays to reduce runtime of examples. By
  [@seabbs](https://github.com/seabbs).
- Additional statements to make it clear to users examples should be
  used for real world analysis. By [@seabbs](https://github.com/seabbs).
- Additional context in the README on package functionality. By
  [@seabbs](https://github.com/seabbs).
- Added some work in progress model definitions and a resource list for
  case studies using the package. By
  [@seabbs](https://github.com/seabbs).

### Package changes

- Added a `contributing.md` to guide contributors and added `pre-commit`
  support to check new contributions styling. By
  [@seabbs](https://github.com/seabbs).
- Better test skipping thanks to [@Bisaloo](https://github.com/Bisaloo).
- Switched from `cowplot::theme_cowplot()` to
  [`ggplot2::theme_bw()`](https://ggplot2.tidyverse.org/reference/ggtheme.html).
  This allows the removal of `cowplot` as a dependency as well making
  plots visible for users saving as pngs and using a dark theme. By
  [@seabbs](https://github.com/seabbs).
- By default `epinow` and downstream functions remove leading zeros. Now
  this is optional with the new `filter_leading_zeros` option. Thanks to
  [@LloydChapman](https://github.com/LloydChapman) in
  [\#285](https://github.com/epiforecasts/EpiNow2/issues/285).
- Basic tests have been added to cover
  [`estimate_secondary()`](https://epiforecasts.io/EpiNow2/reference/estimate_secondary.md),
  [`forecast_secondary()`](https://epiforecasts.io/EpiNow2/reference/forecast_secondary.md),
  and
  [`estimate_truncation()`](https://epiforecasts.io/EpiNow2/reference/estimate_truncation.md).
  By [@seabbs](https://github.com/seabbs) in
  [\#315](https://github.com/epiforecasts/EpiNow2/issues/315).
- Add basic snapshot tests for `adjust_infection_to_report`. By
  [@seabbs](https://github.com/seabbs) in
  [\#316](https://github.com/epiforecasts/EpiNow2/issues/316).
- Update to use `rstantools` to manage compiler flags.
- Update the Dockerfile to work better with vscode.

### Other changes

- Updated the classification of growth to use stable rather than unsure
  when Rt is approximately 1. By [@seabbs](https://github.com/seabbs).
- The default parallisation has been changed to
  [`future::multisession()`](https://future.futureverse.org/reference/multisession.html)
  from `future::multiprocess()` as the latter is being depreciated in
  the `future` package. By [@seabbs](https://github.com/seabbs) and
  [@sbfnk](https://github.com/sbfnk).
- Ensure the seeding time is at least the maximum generation time
  ([@sbfnk](https://github.com/sbfnk)).

### Deprecated features

- `simulate_cases()` and
  [`simulate_infections()`](https://epiforecasts.io/EpiNow2/reference/simulate_infections.md)
  have been deprecated and have been removed. These functions depend on
  `EpiSoon` which itself is archived and near equivalent functionality
  is available within `EpiNow2` and in other packages
  ([@seabbs](https://github.com/seabbs)).
- Functions supporting secondary forecasting using
  [`simulate_infections()`](https://epiforecasts.io/EpiNow2/reference/simulate_infections.md)
  (i.e in \`epinow()) have been removed along with the arguments that
  supported them ([@seabbs](https://github.com/seabbs)).
- `global_map()`, `country_map()`, and `theme_map()` have all been
  deprecated and have been removed. These functions were used to support
  reporting of reproduction number estimates and are considered out of
  scope for `EpiNow2`. If finding useful contacting the `EpiNow2`
  developers ([@seabbs](https://github.com/seabbs)).

### Bug fixes

- Fixed a bug in the deconvolution Rt estimation method where the mean
  of the generation time was being used as the standard deviation. For
  the default package generation time these are close and so the impact
  will be limited but in cases where the standard deviation is \<\< than
  the mean this should result in more accurate Rt estimates. By
  [@seabbs](https://github.com/seabbs).
- Fixed a bug where the number of threads used by the data.table package
  were set to one in the global environment. Now the number of threads
  used by data.table are set to whatever the used specified on exit. By
  [@medewitt](https://github.com/medewitt).
- Fixed a bug in `simulate_infections` and `forecast_secondary` which
  meant that a Poisson observation model used for estimation would lead
  to a error. By [@seabbs](https://github.com/seabbs).
- Fixed a bug where `use_rt = FALSE` did not properly cancel user
  settings. By [@sbfnk](https://github.com/sbfnk).
- Fixed a bug in `estimate_truncation` where phi was not initialised. By
  [@sbfnk](https://github.com/sbfnk).
- Fixed a bug where `zero_threshold` was being ignored and so no
  post-processing was happening. To maintain backwards compatibility the
  default has been changed to `Inf` (i.e. no zero threshold). By
  [@LloydChapman](https://github.com/LloydChapman) in
  [\#285](https://github.com/epiforecasts/EpiNow2/issues/285).
- Fixed a bug where setting `obs_opts(return_likelihood = TRUE)` fails.
  By [@sbfnk](https://github.com/sbfnk) in
  [\#333](https://github.com/epiforecasts/EpiNow2/issues/333).

## EpiNow2 1.3.2

CRAN release: 2020-12-14

In this release model run times have been reduced using a combination of
code optimisation and testing to reduce the likelihood of long running
edge cases. Model flexibility has also been increased, particularly for
the back calculation approach which now supports an increased range of
prior choices. A significant development in this release is the edition
of the experimental `estimate_secondary` model (and supporting
`forecast` and `plot` functions). This allows a downstream target to be
forecast from an observation. Example use cases include forecasting
deaths from test positive cases and hospital bed usage from hospital
admissions. This approach is intended to provide an alternative to
models in which multiple targets are estimated jointly.

### New features

- Added a new argument, `prior`, to
  [`backcalc_opts()`](https://epiforecasts.io/EpiNow2/reference/backcalc_opts.md).
  This allows the use of different priors for the underlying latent
  infections when estimating using deconvolution/back-calculation rather
  than the package default of using a generated Rt model (enable this
  option by setting `rt = NULL`). The default prior remains smoothed
  mean delay shifted reported cases but optionally no prior can now also
  be used (for scenarios when the data is very untrustworthy but likely
  to perform extremely poorly in real time).In addition, the previously
  estimated infections can be used (i.e infections\[t\] =
  infections\[t-1\] \* exp(GP)) with this being an approximate version
  of the generative Rt model that does not weight previous infections
  using the generation time.
- Updates the smoothing applied to mean shifted reported cases used as a
  prior for back calculation when `prior = "reports"` to be a partial
  centred moving average rather than a right aligned moving average.
  This choice means that increasing the `prior` window does not alter
  the location of epidemic peaks as when using a right alighted moving
  average.
- Updates the default smoothing applied to mean shifted reported cases
  to be 14 days rather than 7 as usage indicates this provided too much
  weight to small scale changes. This remains user set able.
- Adds a new argument `init_fit` to
  [`stan_opts()`](https://epiforecasts.io/EpiNow2/reference/stan_opts.md)
  that enables the user to pass in a `stanfit` to use to initialise a
  model fit in
  [`estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.md).
  Optionally `init_fit = "cumulative"` can also be passed which first
  fits to cumulative data and then uses the result to initialise the
  full fit on incidence data. This approach is based on the approach
  used in [epidemia](https://github.com/ImperialCollegeLondon/epidemia/)
  authored by James Scott. Currently `stan` warnings from this initial
  fit are broadcast to the user and may cause concern as the short run
  time and approximate settings often lead to poor convergence.
- Adds `estimate_secondary` and `forecast_secondary` along with a `plot`
  method and a new option function
  ([`secondary_opts()`](https://epiforecasts.io/EpiNow2/reference/secondary_opts.md)).
  These functions implement a generic model for forecasting a secondary
  observation (such as hospital bed usage, deaths in hospital) that
  entirely depends on a primary observation (such as hospital
  admissions) via a combination of convolving over a delay and
  adding/subtracting current observations. They share the same
  observation model and optional features used by `estimate_infections`
  and so support data truncation, scaling (between primary and secondary
  observations), multiple log normal delays, a day of the week effect,
  and various error models. `stationary_opts()` allows for easy
  specification of the most common use cases (incidence and prevalence
  variables). See the documentation and examples for model details.

### Other changes

- Updates `discretised_gamma_pmf` (discretised truncated Gamma PMF) and
  `discretised_lognormal_pmf` (discretised truncated lognormal PMF) to
  limit/clip the values of the parameters by prespecified lower and
  upper bounds.
- Tightens the initialisation of fitting in `estimate_infections` by
  reducing all standard deviations used by a scaling factor of 0.1 in
  `create_initial_conditions`.
- Adds boundary checking on `gt_mean` (the mean of the generation time)
  to reject samples with a mean greater than `gt_max` (the maximum
  allowed generation time). Adds boundary checking to reject standard
  deviations that are negative. Adds a boundary check on R values to
  reject them if 10 times greater than the mean of the initial prior. In
  some scenarios this will require users to supply a prior not is not
  completely misspecified (i.e if the prior has a mean of 1 and the
  posterior has a mean of 50).
- Refactor of `update_rt` (an internal `stan` function found in
  `inst/stan/functions/rt.stan`) to be vectorised. This change reduces
  run times by approximately 1- ~ 20% (though only tested on a small
  subset of examples) and opens the way for future model extensions
  (such as additive rather than multiplicative random walks, and
  introducing covariates).
- Switched to reporting two significant figures in all summary tables
- Reduced minimum default Gaussian process length scale to 3 days from 7
  based on experience running the model at scale.

## EpiNow2 1.3.1

CRAN release: 2020-11-22

This release focusses on model stability, with a functional rewrite of
the model implementation, finalising the interface across the package,
and introducing additional tooling. The additional tooling includes:
support for adjusting for and estimating data truncation, multiple
approaches for estimating Rt (including the default generative Rt
approach, de-convolution coupled with Rt calculation, and `EpiEstim`
like estimation on observed cases but with a robust observation model),
optional scaling of observed data, and optional adjustment of future
forecasts based on population susceptibility. The examples have also
been expanded with links out to Covid-19 specific work flows that may be
of interest to users. The implementation and model options are now
considered to be maturing with the next release planned to contain
documentation on the underlying approach, case studies, validation,
evaluation the various supported options, and tools for dealing with
secondary reports that are dependent on a primary report (i.e hospital
admissions and hospital bed usage). If interested in contributing to any
of these features please contact the package authors or submit a PR.
User contributions are warmly welcomed.

### New features

- Rewritten the interface for `estimate_infections` to be divided into
  calls to `_opts()` functions. Options are now divided by type for
  delays
  ([`delay_opts()`](https://epiforecasts.io/EpiNow2/reference/delay_opts.md)),
  Rt
  ([`rt_opts()`](https://epiforecasts.io/EpiNow2/reference/rt_opts.md)),
  backcalculation
  ([`backcalc_opts()`](https://epiforecasts.io/EpiNow2/reference/backcalc_opts.md)),
  the Gaussian process
  ([`gp_opts()`](https://epiforecasts.io/EpiNow2/reference/gp_opts.md)),
  and stan arguments
  ([`stan_opts()`](https://epiforecasts.io/EpiNow2/reference/stan_opts.md)).
  This has resulted in a larger number of the arguments from
  `estimate_infections` being folded into the related `_opts()`
  function. Please see the function documentation and examples for
  details.
- Added support for region specific settings for all arguments that take
  an `_opts()` function in `regional_epinow` using the helper functions
  `opts_list` and `update_list` or alternatively by constructing a named
  list with an entry for each region to be estimated.
- Extended the functionality of the back calculation model so that Rt
  can be produced via calculation. These estimates are potentially less
  reliable than those produced using the generative model but the model
  can be estimated in a fraction of the time. In essence this is similar
  to using a back projection method and then estimating Rt using
  [EpiEstim](https://github.com/mrc-ide/EpiEstim) (here with a default
  window of 1 but this can be updated using `backcalc_opts(rt_window))`
  but this approaches incorporates uncertainty from all inputs in a
  single estimate.
- Reduced the default maximum generation time and incubation period
  allowed in the truncated distribution (from 30 days to 15). This
  decreases the model run time substantially at a marginal accuracy
  cost. This new default is not suitable for longer generation times and
  should be modified by the user if these are used.
- Adds basic S3 plot and summary measures for `epinow` and
  `estimate_infections`.
- Updates the initialisation of the generative Rt model (the default) so
  that initial infections that occur in unobserved time (i.e before the
  first reported case) are generated using an exponential growth model
  with priors based on fitting the same model to the first week of data.
  This replaces the previous approach which was to use delay shifted
  reported cases multiplied by independent noise terms. It reduces
  degrees of freedom and fitting time at the cost of some model
  flexibility. Alternatives such as using the generative Rt model were
  considered but ultimately these approaches were not used as they
  introduced spurious variation to the gaussian process and result in
  unreliable Rt estimates due to the lack of historic infections.
- New `simulate_infections` function from
  [@sbfnk](https://github.com/sbfnk) which allows the simulation of
  different Rt traces when combined with estimates as produced by
  `estimate_infections`. This function is likely to form the basis for
  moving all forecasting out of `estimate_infections` which may improve
  model stability.
- Updates the implementation of the Gaussian process to support the
  Matern 3/2 Kernel (and set this as the default) in addition to the
  squared exponential kernel. Updates the handling of Gaussian process
  arguments so that only overridden settings need to be passed by the
  user when making changes. Settings are now defined, and documented, in
  [`gp_opts()`](https://epiforecasts.io/EpiNow2/reference/gp_opts.md).
  The length scale is now defined using a log normal truncated prior
  with a mean of 21 days and a standard deviation of 7 days truncated at
  3 days and the length of the data by default. This prior is an area of
  active research and so may change in future releases.
- Updates the over dispersion prior to be
  `1 / sqrt(half_normal(0, rho_prior))` based on
  [this](https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations)
  advice and as the over dispersion being measured is in reports and not
  infections and hence a priori there is not strong evidence for over
  dispersion (which may be the case for infections) so the previous
  prior was overly weighted towards this.
- Updates the interface for the observation model with arguments now
  passed using
  [`obs_opts()`](https://epiforecasts.io/EpiNow2/reference/obs_opts.md).
  This removes `week_effect` and `family` from the main argument list
  which will allow for future extensions. Also adds a new argument
  `scale` which controls the uncertain fraction of cases that are
  eventually observed (defined as normally distributed). Setting this
  parameter will not impact Rt estimates.
- Updates the interface to the Rt settings with all arguments passed via
  `rt`, using
  [`rt_opts()`](https://epiforecasts.io/EpiNow2/reference/rt_opts.md),
  this includes the initial prior,`use_breakpoints`, and `future`. Adds
  a new helper argument `rw` which enables easy parameterisation of a
  fixed length random walk. These changes also help make it clear that
  these arguments only impact the Rt generative model and not the back
  calculation model.
- Adds an adjustment for population susceptibility based on that used in
  [`{epidemia}`](https://github.com/ImperialCollegeLondon/epidemia) when
  Rt is fixed into the future (set by passing a population to
  `rt_opts(pop = initial susceptible population)`. Note this only
  impacts case forecasts and not output Rt estimates and only impacts
  estimates at all beyond the forecast horizon as those based on data
  already account for population susceptibility by definition. The
  impact of this assumption can be explored using `simulate_infections`
  (by updating `est$arg$pop` in the example).
- Adds `truncation` as a new argument to `estimate_infections` and
  higher level functions. This takes output from
  [`trunc_opts()`](https://epiforecasts.io/EpiNow2/reference/trunc_opts.md)
  and allows for internally adjusting observed cases for truncation. A
  new method `estimate_truncation` has also been added to support
  estimating a log normal truncation distribution from archived versions
  of the same data set though this method is currently experimental.
- Adds `estimate_delay` as a user friendly wrapper around
  `bootstrapped_dist_fit`.

### Other changes

- Recoded the core stan model to be functional with the aim of making
  the code modular and extendable.
- Added unit tests for the internal stan update_rt function.
- Reworked the package logging system to improve the reporting of issues
  both in `epinow` and in `regional_epinow` for large batch runs.
- Fix from [@hsbadr](https://github.com/hsbadr) to prevent overflow when
  overdispersion is larger (by switching to a Poisson approximation).
  Hitting this issue may indicate a bug in other model code that will
  need further work to explore.
- Moved default verbosity for all functions (excepting
  `regional_epinow`) to be based on whether or not usage is interactive.
- Removed the `burn_in` argument of `estimate_infections` as updates to
  model initialisation mean that this feature is likely no longer
  needed. Please contact the developers if you feel you have a use case
  for this argument.
- Adds utility functions to map between mean and standard deviation and
  the log mean and log standard deviation for a log normal distribution
  (`convert_to_logmean` and `convert_to_logsd`).
- Optimised all discrete probability mass functions to be as vectorised
  as possible.
- Updated the Gaussian process to be internally on the unit scale.
- Added a new function, `expose_stan_fns` that exposes the internal stan
  functions into R. The enables unit testing, exploration of the stan
  functionality and potentially within R use cases for these functions.
- Updates the default `warmup` to be 250 samples and the default
  `adapt_delta` to be 0.98.
- Adds a pooling parameter for the standard deviation of breakpoint
  effects.
- Updated all documentation and added
  [lifecycle](https://lifecycle.r-lib.org/) badges to all functions to
  indicate development stage.

## EpiNow2 1.2.1

CRAN release: 2020-10-20

This release introduces multiple breaking interface changes. Please see
the README for examples of the new interface. It adds a range of quality
of life improvements including updating the `stan` interface to support
fitting each chain independently and offering variational inference as
an alternative, experimental, fitting option. Notably it also adds
support for nesting logging and a parallel enabled progress bar via the
`progressr` package. Minor bugs have been fixed in the core model
implementation focussing on stability and several already implemented
features have been extended. Major model developments are planned for
the next release of `EpiNow2`.

### New features

- Added support for either NUTs sampling (`method = "exact"`) or
  Variational inference (`method = "approximate"`).
- Update the prior on the initial Rt estimate to be lognormal rather
  than gamma distributed. For users the interface remains unchanged but
  this parameterisation should be more numerically stable.
- Added `get_dist`, `get_generation_time`, `get_incubation_period` based
  on ideas from [@pearsonca](https://github.com/pearsonca). (This leads
  to breaking changes with the removal of `covid_generation_times` and
  `covid_incubation_periods`).
- Added `setup_logging` to enable users to specify the level and
  location of logging (wrapping functionality from `futile.logger`).
  Also added `setup_default_logging` to give users sensible defaults and
  embedded this function in `regional_epinow` and `epinow`.
- Added `setup_future` to making using nested futures easier (required
  when using `future = TRUE`).
- Implemented progress bar support using `progressr`.
- Added timeout and timing option to `regional_epinow`
- Improved logging of warnings in `regional_epinow`
- Enabled the user to specify the credible intervals desired with 20%,
  50% and 90% calculated by default. Also switched from high density
  regions to quantiles. Custom credible intervals are now supported in
  all reporting and plotting functions.
- Added mean and sd to all reporting summaries.
- Added a summary of the growth rate and doubling time.
- Added a new function `regional_runtimes` that summarises the run time
  across regions.
- Updated the `estimate_infections` interface and expanded the range of
  options for the `future_rt` argument. Users can now choose to set Rt
  from any time point referenced to the forecast date.

### Bug fixes

- Fixed y axis max for `plot_summary`.
- Fix to normalisation of delay and generation time distributions from
  [@sbfnk](https://github.com/sbfnk). This will impact nowcast
  infections but not reproduction number estimate.
- Updated `discretised_gamma_pmf` (discretised truncated Gamma PMF) to
  constrain gamma shape and (inverse) scale parameters to be positive
  and finite (`alpha > 0` and `beta > 0`).
- Fixed `readLines` incomplete final line warnings.
- Fix from [@medewitt](https://github.com/medewitt) from the internal
  `fit_chain` function where an interaction between `rstan` and timing
  out may have introduced an exception that caused whole regions to
  fail. This did not show on current unit tests or exploration using
  examples indicating a gap in testing.

### Other changes

- Updates the interface for specifying how output is returned.
- Moved all inherited from stan arguments into `create_stan_args` with
  the option to override using `stan_args`. This leads to breaking
  changes - see the examples for details of the new interface.
- Updated all example and documentation to reflect the new interface.
- Added a `samples` argument to `get_regional_results` to make loading
  in samples optional. This also allows samples to be dropped when using
  `regional_epinow` which reduces RAM usage.
- Cleaned up wrapper functions to move individual jobs into functions.
- Adds testing of high level functions and some low level unit testing.
- Adds a csv download button the interactive table in the regional
  summary table.
- Makevars updated to remove the dependency on GNU Make by
  [@hsbadr](https://github.com/hsbadr)

## EpiNow2 1.1.0

CRAN release: 2020-09-01

- Implemented reporting templates
- Bug fix of estimate reporting
- Added additional reporting of runtime errors
- Examples for `global_map` and `country_map` expanded by
  [@ellisp](https://github.com/ellisp)
- Improved ISO code matching in `global_map` from
  [@ellisp](https://github.com/ellisp)
- Improvements so that data frames and tibbles are supported as inputs.
- Updated reporting templates
- Updated reporting of estimates to clearly summarise cases by infection
  and report date.
- Made all region summary plots optional.
- Made reporting of decimal places more standardised across metrics.
- README updated by [@kathsherratt](https://github.com/kathsherratt)
- Logging added by [@joeHickson](https://github.com/joeHickson)
- Updated plotting to be limited to a scaling of reported data (prevents
  upper CIs from skewing the plot).
- Added uncertainty plot bounds to control y axis on plots for clarity
  purposes.
- `regional_summary` now saves input reported cases data
  `reported_cases.csv`.
- Added an optional no delay model where Rt is estimated directly from
  the data. This option is not supported when using backcalculation
  only.

## EpiNow2 1.0.0

- Rebased package from [EpiNow](https://epiforecasts.io/EpiNow/)
- Implemented backcalculation, estimation, forecasting, and bootstrapped
  distribution fitting.
- Added options to estimate the time-varying reproduction number using a
  Gaussian process (both stationary and non-stationary), combined with
  optional user supplied breakpoints. Alternatively a static
  reproduction number can be assumed which when combined with
  breakpoints becomes piecewise linear.
