# EpiNow2 1.3.0

## New features

* Extended the functionality of the back calculation model so that Rt can be produced via calculation. These estimates are potentially less reliable than those produced using the generative model but the model can be estimated in a fraction of the time.
* Reduced the default maximum generation time and incubation period allowed in the truncated distribution (from 30 days to 15). This decreases the model run time substantially at a marginal accuracy cost. This new default is not suitable for longer generation times and should be modified by the user if these are used.
* Adds basic S3 plot and summary measures for `epinow` and a plot method for `estimate_infections`. This functionality will likely be further built out in later releases.
* Updates the initialisation of the generative Rt model (the default) so that initial infections that occur in unobserved time (i.e before the first reported case) are generated using an exponential growth model with priors based on fitting the same model to the first week of data. This replaces the previous approach which was to use delay shifted reported cases multiplied by independent noise terms. It reduces degrees of freedom and fitting time at the cost of some model flexibility. Alternatives such as using the generative Rt model were considered but ultimately these approaches were not used as they introduced spurious variation to the gaussian process and result in unreliable Rt estimates due to the lack of historic infections.
* New `simulate_infections` function from @sbfnk which allows the simulation of different Rt traces when combined with estimates as produced by `estimate_infections`. This function is likely to form the basis for moving all forecasting out of `estimate_infections` which may improve model stability. 
* Updates the implementation of the Gaussian process to support the Matern 3/2 Kernel (and set this as the default) in addition to the squared exponential kernel. Updates the handling of Gaussian process arguments so that only overridden settings need to be passed by the user when making changes. Settings are now defined, and documented, in `gp_settings`. The length scale is now defined using a log normal truncated prior with a mean of 21 days and a standard deviation of 7 days truncated at 3 days and the length of the data by default. This prior is an area of active research and so may change in future releases.
* Updates the over dispersion prior to be `1 / sqrt(half_normal(0, rho_prior))` based on [this](https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations) advice and as the over dispersion being measured is in reports and not infections and hence a priori there is not strong evidence for over dispersion (which may be the case for infections) so the previous prior was overly weighted towards this.
* Updates the interface for the observation model with arguments now  passed using `obs_model`. This removes `week_effect` and `family` from the main argument list which will allow for future extensions. Also adds a new argument `scale` which controls the uncertain fraction of cases that are eventually observed (defined as normally distributed). Setting this parameter will not 
impact Rt estimates.
* Updates the interface to the Rt settings with all arguments passed to `rt` this includes the initial prior,`use_breakpoints`, and `future`. Adds a new helper argument `rw` which enables easy parameterisation of a fixed length random walk. These changes also help make it clear that these arguments only impact the Rt generative model and not the back calculation model.
* Updates the interface to back calculation options to be passed via `backcalc`. Currently this only impacts `prior_smoothing_window` which can now be passed using `backcalc = list(smoothing_window = 7)`

## Other changes

* Recoded the core stan model to be functional with the aim of making the code modular and extendable.
* Added unit tests for the internal stan update_rt function.
* Reworked the package logging system to improve the reporting of issues both in `epinow` and in `regional_epinow` for large batch runs.
* Fix from @hsbadr to prevent overflow when overdispersion is larger (by switching to a Poisson approximation). Hitting this issue may indicate a bug in other model code that will need further work to explore.
* Moved default verbosity for all functions (excepting `regional_epinow`) to be based on whether or not usage is interactive. 
* Removed the `burn_in` argument of `estimate_infections` as updates to model initialisation mean that this feature is likely no longer needed. Please contact the developers if you feel you have a use case for this argument.
* Adds utility functions to map between mean and standard deviation and the log mean and log standard deviation for a log normal distribution (`convert_to_logmean` and `convert_to_logsd`).
* Optimised all discrete probability mass functions to be as vectorised as possible.
* Updated the Gaussian process to be internally on the unit scale.
* Added a new function, `expose_stan_fns` that exposes the internal stan functions into R. The enables unit testing, exploration of the stan functionality and potentially within R use cases for these functions.
* Updates the default `warmup` to be 250 samples and the default `adapt_delta` to be 0.98.
* Dropped the `model` and `method` arguments from `estimate_infections` as these are now included in `stan_args`. Also clarified the documentation for `stan_args` to better indicate what the defaults are and how they can be updated.
* Adds a new `summary` method for `estimate_infections` and updates the `epinow` summary method to be based on it.
* Adds a pooling parameter for the standard deviation of breakpoint effects.

# EpiNow2 1.2.1

This release introduces multiple breaking interface changes. Please see the README for examples of the new interface. It adds a range of quality of life improvements including updating the `stan` interface to support fitting each chain independently and offering variational inference as an alternative, experimental, fitting option. Notably it also adds support for nesting logging and a parallel enabled progress bar via the `progressr` package. Minor bugs have been fixed in the core model implementation focussing on stability and several already implemented features have been extended. Major model developments are planned for the next release of `EpiNow2`.

## New features

* Added support for either NUTs sampling (`method = "exact"`) or Variational inference (`method = "approximate"`).
* Update the prior on the initial Rt estimate to be lognormal rather than gamma distributed. For users the interface remains unchanged but this parameterisation should be more numerically stable.
* Added `get_dist`, `get_generation_time`, `get_incubation_period` based on ideas from @pearsonca. (This leads to breaking changes with the removal of `covid_generation_times` and `covid_incubation_periods`).
* Added `setup_logging` to enable users to specify the level and location of logging (wrapping functionality from `futile.logger`). Also added `setup_default_logging` to give users sensible defaults and embedded this
function in `regional_epinow` and `epinow`.
* Added `setup_future` to making using nested futures easier (required when using `future = TRUE`).
* Implemented progress bar support using `progressr`.
* Added timeout and timing option to `regional_epinow`
* Improved logging of warnings in `regional_epinow`
* Enabled the user to specify the credible intervals desired with 20%, 50% and 90% calculated by default. Also switched from high density regions to quantiles. Custom credible intervals are now supported in all reporting and plotting functions.
* Added mean and sd to all reporting summaries.
* Added a summary of the growth rate and doubling time.
* Added a new function `regional_runtimes` that summarises the run time across regions.
* Updated the `estimate_infections` interface and expanded the range of options for the `future_rt` argument. Users can now choose to set Rt from any time point referenced to the forecast date.

## Bug fixes

* Fixed y axis max for `plot_summary`.
* Fix to normalisation of delay and generation time distributions from @sbfnk. This will impact nowcast infections but not reproduction number estimate.
* Updated `discretised_gamma_pmf` (discretised truncated Gamma PMF) to constrain gamma shape and (inverse) scale parameters to be positive and finite (`alpha > 0` and `beta > 0`).
* Fixed `readLines` incomplete final line warnings.
* Fix from @medewitt from the internal `fit_chain` function where an interaction between `rstan` and timing out may have introduced an exception that caused whole regions to fail. This did not show on current unit tests or exploration using examples indicating a gap in testing. 

## Other changes

* Updates the interface for specifying how output is returned.
* Moved all inherited from stan arguments into `create_stan_args` with the option to override using `stan_args`. This leads to breaking changes - see the examples for details of the new interface.
* Updated all example and documentation to reflect the new interface.
* Added a `samples` argument to `get_regional_results` to make loading in samples optional. This also allows samples to be dropped when using `regional_epinow` which reduces RAM usage.
* Cleaned up wrapper functions to move individual jobs into functions.
* Adds testing of high level functions and some low level unit testing.
* Adds a csv download button the interactive table in the regional summary table.
* Makevars updated to remove the dependency on GNU Make by @hsbadr

# EpiNow2 1.1.0

* Implemented reporting templates
* Bug fix of estimate reporting
* Added additional reporting of runtime errors
* Examples for `global_map` and `country_map` expanded by @ellisp
* Improved ISO code matching in `global_map` from @ellisp
* Improvements so that data frames and tibbles are supported as inputs.
* Updated reporting templates
* Updated reporting of estimates to clearly summarise cases by infection and report date. 
* Made all region summary plots optional.
* Made reporting of decimal places more standardised across metrics. 
* README updated by @kathsherratt
* Logging added by @joeHickson
* Updated plotting to be limited to a scaling of reported data (prevents upper CIs from skewing the plot).
* Added uncertainty plot bounds to control y axis on plots for clarity purposes.
* `regional_summary` now saves input reported cases data `reported_cases.csv`.
* Added an optional no delay model where Rt is estimated directly from the data. This option is not supported when using backcalculation only.

# EpiNow2 1.0.0

* Rebased package from [EpiNow](https://epiforecasts.io/EpiNow/)
* Implemented backcalculation, estimation, forecasting, and bootstrapped distribution fitting.
* Added options to estimate the time-varying reproduction number using a Gaussian process (both stationary and non-stationary), combined with optional user supplied breakpoints. Alternatively a static reproduction number can be assumed which when combined with breakpoints becomes piecewise linear.
