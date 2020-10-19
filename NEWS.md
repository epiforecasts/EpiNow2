#EpiNow2 1.3.0

This release includes the optional use of the [`cmdstanr`](https://mc-stan.org/cmdstanr/) package to interface directly with CmdStan. Because `cmdstanr` is a lightweight package that directly interfaces with CmdStan, new features that have been introduced in CmdStan will be immediately available for users of EpiNow2 without having to wait for their introduction into `rstan` (e.g. within-chain parallelization).

## New features

* Added `backend` argument to specifiy one of "rstan" (default) or "cmdstan" to the `estimate_infections` function.


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

## Other changes

* Updates the interface for specifying how output is returned.
* Moved all inherited from stan arguments into `create_stan_args` with the option to override using `stan_args`. This leads to breaking changes - see the examples for details of the new interface.
* Updated all example and documentation to reflect the new interface.
* Added a `samples` argument to `get_regional_results` to make loading in samples optional. This also allows samples to be dropped when using `regional_epinow` which reduces RAM usage.
* Cleaned up wrapper functions to move individual jobs into functions.
* Adds testing of high level functions and some low level unit testing.
* Adds a csv download button the interactive table in the regional summary table.

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
