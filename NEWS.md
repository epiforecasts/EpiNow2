# EpiNow2 1.4.9000

## Breaking changes

* The functions `get_dist`, `get_generation_time`, `get_incubation_period` have been deprecated and replaced with examples. By @sbfnk in #481 and reviewed by @seabbs.
* The utility function `update_list()` has been deprecated in favour of `utils::modifyList()` because it comes with an installation of R. By @jamesmbaazam in #491 and reviewed by @seabbs.
* The `fixed` argument to `dist_spec` has been deprecated and replaced by a `fix_dist()` function. By @sbfnk in #503 and reviewed by @seabbs.
* Updated `estimate_infections()` so that rather than imputing missing data, it now skips these data points in the likelihood. This is a breaking change as it alters the behaviour of the model when dates are missing from a time series but are known to be zero. We recommend that users check their results when updating to this version but expect this to in most cases improve performance. By @seabbs in #528 and reviewed by @sbfnk.
* `simulate_infections` has been renamed to `forecast_infections` in line with `simulate_secondary` and `forecast_secondary`. The terminology is: a forecast is done from a fit to existing data, a simulation from first principles. By @sbfnk in #544 and reviewed by @seabbs.
* A new `simulate_infections` function has been added that can be used to simulate from the model from given initial conditions and parameters. By @sbfnk in #557 and reviewed by @jamesmbaazam.
* The function `init_cumulative_fit()` has been deprecated. By @jamesmbaazam in #541 and reviewed by @sbfnk.

## Documentation

* Two new vignettes have been added to cover the workflow and example uses. By @sbfnk in #458 and reviewed by @jamesmbaazam.
* Removed references to the no longer existing `forecast_infections` function. By @sbfnk in #460 and reviewed by @seabbs.
* The contribution guide has been improved to include more detail on ways to contribute new features/enhancements, report bugs, and improve or suggest vignettes. By @jamesmbaazam in #464 and reviewed by @seabbs.
* Updated the code in `inst/CITATION` and added a GitHub Actions workflow to auto-generate `citation.cff` so that the two citation files are always in sync with `DESCRIPTION`. By @jamesmbazam in #467, with contributions from @Bisaloo, and reviewed by @seabbs and @sbfnk.

## Package

* Replaced use of `purrr::transpose()` with `purrr::list_transpose()` because the former is superseded. By @jamesmbaazam in #524 and reviewed by @seabbs.
* Reduced the number of long-running examples. By @sbfnk in #459 and reviewed by @seabbs.
* Changed all instances of arguments that refer to the maximum of a distribution to reflect the maximum. Previously this did, in some instance, refer to the length of the PMF. By @sbfnk in #468.
* Fixed a bug in the bounds of delays when setting initial conditions. By @sbfnk in #474.
* Added input checking to `estimate_infections()`, `estimate_secondary()`, `estimate_truncation()`, `simulate_infections()`, and `epinow()`. `check_reports_valid()` has been added to validate the reports dataset passed to these functions. Tests are added to check `check_reports_valid()`. As part of input validation, the various `*_opts()` functions now return subclasses of the same name as the functions and are tested against passed arguments to ensure the right `*_opts()` is passed to the right argument. For example, the `obs` argument in `estimate_secondary()` is expected to only receive arguments passed through `obs_opts()` and will error otherwise. By @jamesmbaazam in #476 and reviewed by @sbfnk and @seabbs.
* Added the possibility of specifying a fixed observation scaling. By @sbfnk in #550 and reviewed by @seabbs.
* Added the possibility of specifying fixed overdispersion. By @sbfnk in #560 and reviewed by @seabbs.
* The example in `estimate_truncation()` has been simplified. The package now ships with a dataset `example_truncated`, which is used in the `estimate_truncation()` example and tests. The steps for creating the `example_truncated` is stored in `./data-raw/estimate-truncation.R`. By @jamesmbaazam in #584 and reviewed by @seabbs and @sbfnk.
* Tests have been updated to only set random seeds before snapshot tests involving random number generation, and unset them subsequently. By @sbfnk in .

## Model changes

* Updated the parameterisation of the dispersion term `phi` to be `phi = 1 / sqrt_phi ^ 2` rather than the previous parameterisation `phi = 1 / sqrt(sqrt_phi)` based on the suggested prior [here](https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations#story-when-the-generic-prior-fails-the-case-of-the-negative-binomial) and the performance benefits seen in the `epinowcast` package (see [here](https://github.com/epinowcast/epinowcast/blob/8eff560d1fd8305f5fb26c21324b2bfca1f002b4/inst/stan/epinowcast.stan#L314)). By @seabbs in #487 and reviewed by @sbfnk.
* Added an `na` argument to `obs_opts()` that allows the user to specify whether NA values in the data should be interpreted as missing or accumulated in the next non-NA data point. By @sbfnk in #534 and reviewed by @seabbs.

# EpiNow2 1.4.0

This release contains some bug fixes, minor new features, and the initial stages of some broader improvement to future handling of delay distributions.

## Breaking changes

- The external distribution interface has been updated to use the `dist_spec()` function. This comes with a range of benefits, including optimising model fitting when static delays are used (by convolving when first defined vs in stan), easy printing (using `print()`), and easy plotting (using `plot()`). It also makes it possible to use all supported distributions everywhere (i.e, as a generation time or reporting delay). However, while for now backwards compatibility has been ensured this update will break most users' code eventually as the interface has changed. See the documentation for `dist_spec()` for more details. By @sbfnk in #363 and reviewed by @seabbs.

## Package

* Model description has been expanded to include more detail. By @sbfnk in #373 and reviewed by @seabbs.
* Moved to a GitHub Action to only lint changed files. By @seabbs in #378.
* Linted the package with a wider range of default linters. By @seabbs in #378.
* Added a GitHub Action to build the README when it is altered. By @seabbs.
* Added handling of edge case where we sample from the negative binomial with mean close or equal to 0. By @sbfnk in #366 and reviewed by @seabbs.
* Replaced use of nested `ifelse()` and `data.table::fifelse()` in the code base with `data.table::fcase()`. By @jamesmbaazam in #383 and reviewed by @seabbs.
* Reviewed the example in `calc_backcalc_data()` to call `calc_backcalc_data()` instead of `create_gp_data()`. By @jamesmbaazam in #388 and reviewed by @seabbs.
* Improved compilation times by reducing the number of distinct stan models and deprecated `tune_inv_gamma()`. By @sbfnk in #394 and reviewed by @seabbs.
* Changed touchstone settings so that benchmarks are only performed if the stan model is changed. By @sbfnk in #400 and reviewed by @seabbs.
* [pak](https://pak.r-lib.org/) is now suggested for installing the developmental version of the package. By @jamesmbaazam in #407 and reviewed by @seabbs. This has been successfully tested on MacOS Ventura, Ubuntu 20.04, and Windows 10. Users are advised to use `remotes::install_github("epiforecasts/EpiNow2")` if `pak` fails and if both fail, raise an issue.
* `dist_fit()`'s `samples` argument now takes a default value of 1000 instead of NULL. If a supplied `samples` is less than 1000, it is changed to 1000 and a warning is thrown to indicate the change. By @jamesmbazam in #389 and reviewed by @seabbs.
* The internal distribution interface has been streamlined to reduce code duplication. By @sbfnk in #363 and reviewed by @seabbs.
* A small bug has been fixed where the seeding time was too long. When a single delay is used this shortens the seeding time by one day and when more delays are used it shortens the seeding time by n days where n is the number of delays used e.g. for two parametric delays it's two days. By @sbfnk in #413 and reviewed by @seabbs.
* Some tuning was done to speed up the renewal model. By @sbfnk in #416 and reviewed by @seabbs.
* An approximation of the negative binomial by the Poisson at low levels of overdispersion was disabled as it led to parameter identification issues. By @sbfnk in #432 and reviewed by @seabbs.
* Reduced verbosity of tests. By @sbfnk in #433 and reviewed by @seabbs.
* Updated code style in response to lintr warnings. By @sbfnk in #437 and reviewed by @seabbs.
* Fixed an edge case breaking summary output. Reported by @jrcpulliam, fixed by @sbfnk in #436 and reviewed by @seabbs.
* Added content to the vignette for the estimate_truncation model. By @sbfnk in #439 and reviewed by @seabbs.
* Added a feature to the `estimate_truncation` to allow it to be applied to time series that are shorter than the truncation max. By @sbfnk in #438 and reviewed by @seabbs.
* Changed the `estimate_truncation` to use the `dist_spec` interface, deprecating existing options `max_trunc` and `trunc_dist`. By @sbfnk in #448 and #452 and reviewed by @seabbs.
* Added a `weigh_delay_priors` argument to the main functions, allowing the users to choose whether to weigh delay priors by the number of data points or not. By @sbfnk in #450 and reviewed by @seabbs.

## Documentation

- Added a link to the recent CSTE workshop on using `EpiNow2` to the case studies vignette. By @seabbs in #441 and reviewed by @sbfnk.

# EpiNow2 1.3.5

This is a minor release to resolve issues with the recent CRAN requirement to make use of a C++ 17 compiler which has been causing [issues with the `rstantools` package](https://github.com/stan-dev/rstantools/pull/100).

# EpiNow2 1.3.4

This release focusses on bug fixes and package infrastructure updates along with a few quality of life improvements such as enabling the use of fixed delays and generation times.

Thanks to @seabbs, and @sbfnk and for the South African Centre for Epidemiological Modelling and Analysis (SACEMA) for hosting @seabbs whilst some of the development work on this release was being done.

## Breaking changes

* To streamline the interface with the definition of delay distributions `trunc_opts()` now takes a single argument (`dist`) which defines the truncation delay rather than a arbitrary list of arguments (which was previously used to define the distribution).
* Updated the handling of generation times in the renewal equation to be left truncation adjusted for the zeroth day. This more better the approach taken to estimate generation times but may slightly impact estimates vs those produced using previous versions.
* The range of the `frac_obs` parameter has restricted with an upper bound of 1 to reflect its name and description. This may impact a small number of edge case uses with the majority being models fit using `estimate_secondary()`. By @sbfnk in #340.

## Features

* Adds a new function `simulate_secondary()` for simulating secondary observations under the generative process model assumed by `estimate_secondary`. Unlike `forecast_secondary()` which uses a `stan` model to simulate secondary cases (which shares code with the `estimate_secondary` model) this new function is implemented entirely in R and is therefore useful to sense check any results from the `stan` implementation.
* Adds support for fixed delays (mean only or fixed lognormal distributed) or truncations (fixed lognormal distributed), and for pre-computing these delays as well as generation times if they are fixed. By @sbfnk and @seabbs.
* Support for gamma distributed delays and log-normal distributed generation times.

## Package

* Update the GitHub Action files to new versions.
* Switched to using `seq_along()` rather than `1:length()` in all package code.
* Fixed a broken example in the documentation for `regional_runtimes()`.
* Add compatibility changes for the latest version of `rstan` and `rstantools`.
* Remove legacy use of `pkgnet` for package dependency visualisation.
* Restyled all code using `styler`.
* Dropped dependency on `RcppParallel`.
* Updated `report_cases` to work with the new `delay_opts` helper function.
* Added test coverage for `report_cases` though note this function will likely be deprecated in future releases.
* Switched to `linewidth` in `plot_CrIs` rather than `size` to avoid issues with `ggplot2` 3.4.0.
* Set up validation against synthetic data to run as a CI check.
* Added tests for internal stan convolution functions.
* Update all `get_` distribution functions to return the distribution as well as summary
 parameters.
* Added support for model fitting benchmarking using `touchstone`.

## Documentation

* Slight edits to the model outline for `estimate_infections()`.
* Updated examples to make use of fixed distributions.

## Bugs

* Fixed a bug in `simulate_infections()` where passing a custom number of samples would cause the input vector of R values to be replicated in a column-wise fashion meaning that the intended R trajectory was not simulated.
* Fixed a bug in the `estimate_infections()` deconvolution model where the generation time was not correctly being reversed.

# EpiNow2 1.3.3

This release adds a range of new minor features, squashes bugs, enhances documentation, expands unit testing, implements some minor run-time optimisations, and removes some obsolete features.

Thanks to @Bisaloo, @hsbadr, @LloydChapman, @medewitt, and @sbfnk for contributing to this release.

Thanks to @sbfnk, @pearsonca, and @nicholasdavies for regression testing this release against `1.3.2`.

## New features

* Added supported to `simulate_infections` so that a `data.frame` of R samples can be passed in instead of a vector of R values. By @seabbs.
* Added extraction of posterior samples to the summary method for `estimate_infections`.  By @seabbs.
* Exposed `zero_threshold` to users allowing for control over when zeros or NAs in count data are treated as true zeros versus as reporting errors that require some smoothing.  By @seabbs.
* Added support for varying the length of the day of the week effect (see `obs_opts()`). This allows, for example, fitting to data with cases only reported every 3 days. By @seabbs.
* Adds option to `plot_estimates()` and higher level functions to choose which estimate type to plot. By @seabbs.
* Adds support for fixed generation times (either mean only or fixed gamma distributed). By @sbfnk.
* Adds support for optionally using an inverse gamma prior for the lengthscale of the gaussian process. This scaled prior has been tested for both short and long simulations where the default prior may make the model unstable. The new prior is more stable for long simulations and adaptively change the distribution based on the simulation length (total number of days) without relying on the user inputs or the fixed defaults. It can be tested by setting ls_sd = 0 in gp_opts(). By @hsbadr.
* Updated the prior on the magnitude of the gaussian process to be 0.05 vs 0.1 leading to slightly more stable estimates. By @hsbadr.
* Added an argument (`plot`) to `regional_summary` to allow plotting to be optional. Closes #250. By @seabbs in #317

## Model changes

* Added support for varying the length of the day of the week effect (see `obs_opts()`). This allows, for example, fitting to data with cases only reported every 3 days. 
* Minor optimisations in the observation model by only using the `target` likelihood definition approach when required and in the use of `fmax` and `fmin` over using if statements.  By @seabbs.
* Added support for users setting the overdispersion (parameterised as one over the square root of phi) of the reporting process. This is accessible via the `phi` argument of `obs_opts` with the default of a normal distribution with mean 0 and standard deviation of 1 truncated at 0 remaining unchanged.  By @seabbs.
* Added additive noise term to the `estimate_truncation` model to deal with zeroes. By @sbfnk.
* Switched to using optimised versions of the discretised distributions supported for the
reporting delay and the generation time. These are based on an implementation in [`epinowcast`](https://package.epinowcast.org/) by Adrian Lison and Sam Abbott. By @seabbs in #320.

## Documentation

- Updates to all synthetic delays to reduce runtime of examples. By @seabbs.
- Additional statements to make it clear to users examples should be used for real world analysis. By @seabbs.
- Additional context in the README on package functionality.  By @seabbs.
- Added some work in progress model definitions and a resource list for case studies using the package. By @seabbs.

## Package changes

* Added a `contributing.md` to guide contributors and added `pre-commit` support to check new contributions styling.  By @seabbs.
* Better test skipping thanks to @Bisaloo.
* Switched from `cowplot::theme_cowplot()` to `ggplot2::theme_bw()`. This allows the removal of `cowplot` as a dependency as well making plots visible for users saving as pngs and using a dark theme. By @seabbs.
* By default `epinow` and downstream functions remove leading zeros. Now this is optional with the new `filter_leading_zeros` option. Thanks to @LloydChapman in #285.
* Basic tests have been added to cover `estimate_secondary()`, `forecast_secondary()`, and `estimate_truncation()`. By @seabbs in #315.
* Add basic snapshot tests for `adjust_infection_to_report`. By @seabbs in #316.
* Update to use `rstantools` to manage compiler flags.
* Update the Dockerfile to work better with vscode.

## Other changes

* Updated the classification of growth to use stable rather than unsure when Rt is approximately 1.  By @seabbs.
* The default parallisation has been changed to `future::multisession()` from `future::multiprocess()` as the latter is being depreciated in the `future` package.  By @seabbs and @sbfnk.
* Ensure the seeding time is at least the maximum generation time (@sbfnk).
 
## Deprecated features

* `simulate_cases()` and `simulate_infections()` have been deprecated and have been removed. These functions depend on `EpiSoon` which itself is archived and near equivalent functionality is available within `EpiNow2` and in other packages (@seabbs).
* Functions supporting secondary forecasting using `simulate_infections()` (i.e in `epinow())
have been removed along with the arguments that supported them (@seabbs).
* `global_map()`, `country_map()`, and `theme_map()` have all been deprecated and have been removed. These functions were used to support reporting of reproduction number
estimates and are considered out of scope for `EpiNow2`. If finding useful contacting the
`EpiNow2` developers (@seabbs).

## Bug fixes

* Fixed a bug in the deconvolution Rt estimation method where the mean of the generation time was being used as the standard deviation. For the default package generation time these are close and so the impact will be limited but in cases where the standard deviation is << than the mean this should result in more accurate Rt estimates.  By @seabbs. 
* Fixed a bug where the number of threads used by the data.table package were set to one in the global environment. Now the number of threads used by data.table are set to whatever the used specified on exit. By @medewitt.
* Fixed a bug in `simulate_infections` and `forecast_secondary` which meant that a Poisson observation model used for estimation would lead to a error. By @seabbs.
* Fixed a bug where `use_rt = FALSE` did not properly cancel user settings. By @sbfnk.
* Fixed a bug in `estimate_truncation` where phi was not initialised. By @sbfnk.
* Fixed a bug where `zero_threshold` was being ignored and so no post-processing was happening. To maintain backwards compatibility the default has been changed to `Inf` (i.e. no zero threshold). By @LloydChapman in #285.
* Fixed a bug where setting `obs_opts(return_likelihood = TRUE)` fails. By @sbfnk in #333.

# EpiNow2 1.3.2

In this release model run times have been reduced using a combination of code optimisation and testing to reduce the likelihood of long running edge cases. Model flexibility has also been increased, particularly for the back calculation approach which now supports an increased range of prior choices. A significant development in this release is the edition of the experimental `estimate_secondary` model (and supporting `forecast` and `plot` functions). This allows a downstream target to be forecast from an observation. Example use cases include forecasting deaths from test positive cases and hospital bed usage from hospital admissions. This approach is intended to provide an alternative to models in which multiple targets are estimated jointly.

## New features

* Added a new argument, `prior`, to `backcalc_opts()`. This allows the use of different priors for the 
underlying latent infections when estimating using deconvolution/back-calculation rather than the package
default of using a generated Rt model (enable this option by setting `rt = NULL`). The default prior 
remains smoothed mean delay shifted reported cases but optionally no prior can now also be used (for 
scenarios when the data is very untrustworthy but likely to perform extremely poorly in real time).In addition,
the previously estimated infections can be used (i.e infections[t] = infections[t-1] * exp(GP)) with this being
an approximate version of the generative Rt model that does not weight previous infections using the generation
time.
* Updates the smoothing applied to mean shifted reported cases used as a prior for back calculation when 
`prior = "reports"` to be a partial centred moving average rather than a right aligned moving average. 
This choice means that increasing the `prior` window does not alter the location of epidemic peaks as when using a right alighted moving average.
* Updates the default smoothing applied to mean shifted reported cases to be 14 days rather than 7 as usage indicates this 
provided too much weight to small scale changes. This remains user set able.
* Adds a new argument `init_fit` to `stan_opts()` that enables the user to pass in a `stanfit` to use to initialise a model fit in `estimate_infections()`. Optionally `init_fit = "cumulative"` can also be passed which first fits to cumulative data and then uses the result to initialise the full fit on incidence data. This approach is based on the approach used in [epidemia](https://github.com/ImperialCollegeLondon/epidemia/) authored by James Scott. Currently `stan` warnings from this initial fit are broadcast to the user and may cause concern as the short run time and approximate settings often lead to poor convergence. 
* Adds  `estimate_secondary` and `forecast_secondary` along with a `plot` method and a new option function (`secondary_opts()`). These functions implement a generic model for forecasting a secondary observation (such as hospital bed usage, deaths in hospital) that entirely depends on a primary observation (such as hospital admissions) via a combination of convolving over a delay and adding/subtracting current observations. They share the same observation model and optional features used by `estimate_infections` and so support data truncation, scaling (between primary and secondary observations), multiple log normal delays, a day of the week effect, and various error models. `stationary_opts()` allows for easy specification of the most common use cases (incidence and prevalence variables). See the documentation and examples for model details. 

## Other changes

* Updates `discretised_gamma_pmf` (discretised truncated Gamma PMF) and `discretised_lognormal_pmf` (discretised truncated lognormal PMF) to limit/clip the values of the parameters by prespecified lower and upper bounds.
* Tightens the initialisation of fitting in `estimate_infections` by reducing all standard deviations used by a scaling factor of 0.1 in `create_initial_conditions`.
* Adds boundary checking on `gt_mean` (the mean of the generation time) to reject samples with a mean greater than `gt_max` (the maximum allowed generation time). Adds boundary checking to reject standard deviations that are negative. Adds a boundary check on R values to reject them if 10 times greater than the mean of the initial prior. In some scenarios this will require users to supply a prior not is not completely misspecified (i.e if the prior has a mean of 1 and the posterior has a mean of 50).  
* Refactor of `update_rt` (an internal `stan` function found in `inst/stan/functions/rt.stan`) to be vectorised. This change reduces run times by approximately 1- ~ 20% (though only tested on a small subset of examples) and opens the way for future model extensions (such as additive rather than multiplicative random walks, and introducing covariates).
* Switched to reporting two significant figures in all summary tables
* Reduced minimum default Gaussian process length scale to 3 days from 7 based on experience running the model at scale. 

# EpiNow2 1.3.1

This release focusses on model stability, with a functional rewrite of the model implementation, finalising the interface across the package, and introducing additional tooling. The additional tooling includes: support for adjusting for and estimating data truncation, multiple approaches for estimating Rt (including the default generative Rt approach, de-convolution coupled with Rt calculation, and `EpiEstim` like estimation on observed cases but with a robust observation model), optional scaling of observed data, and optional adjustment of future forecasts based on population susceptibility. The examples have also been expanded with links out to Covid-19 specific work flows that may be of interest to users. The implementation and model options are now considered to be maturing with the next release planned to contain documentation on the underlying approach, case studies, validation, evaluation the various supported options, and tools for dealing with secondary reports that are dependent on a primary report (i.e hospital admissions and hospital bed usage). If interested in contributing to any of these features please contact the package authors or submit a PR. User contributions are warmly welcomed.

## New features

* Rewritten the interface for `estimate_infections` to be divided into calls to `_opts()` functions. Options are now divided by type for delays (`delay_opts()`), Rt (`rt_opts()`), backcalculation (`backcalc_opts()`), the Gaussian process (`gp_opts()`), and stan arguments (`stan_opts()`). This has resulted in a larger number of the arguments from `estimate_infections` being folded into the related `_opts()` function. Please see the function documentation and examples for details.
* Added support for region specific settings for all arguments that take an `_opts()` function in `regional_epinow` using the helper functions `opts_list` and `update_list` or alternatively by constructing a named list with an entry for each region to be estimated. 
* Extended the functionality of the back calculation model so that Rt can be produced via calculation. These estimates are potentially less reliable than those produced using the generative model but the model can be estimated in a fraction of the time. In essence this is similar to using a back projection method and then estimating Rt using `{EpiEstim}` (here with a default window of 1 but this can be updated using `backcalc_opts(rt_window))` but this approaches incorporates uncertainty from all inputs in a single estimate. 
* Reduced the default maximum generation time and incubation period allowed in the truncated distribution (from 30 days to 15). This decreases the model run time substantially at a marginal accuracy cost. This new default is not suitable for longer generation times and should be modified by the user if these are used.
* Adds basic S3 plot and summary measures for `epinow` and `estimate_infections`.
* Updates the initialisation of the generative Rt model (the default) so that initial infections that occur in unobserved time (i.e before the first reported case) are generated using an exponential growth model with priors based on fitting the same model to the first week of data. This replaces the previous approach which was to use delay shifted reported cases multiplied by independent noise terms. It reduces degrees of freedom and fitting time at the cost of some model flexibility. Alternatives such as using the generative Rt model were considered but ultimately these approaches were not used as they introduced spurious variation to the gaussian process and result in unreliable Rt estimates due to the lack of historic infections.
* New `simulate_infections` function from @sbfnk which allows the simulation of different Rt traces when combined with estimates as produced by `estimate_infections`. This function is likely to form the basis for moving all forecasting out of `estimate_infections` which may improve model stability. 
* Updates the implementation of the Gaussian process to support the Matern 3/2 Kernel (and set this as the default) in addition to the squared exponential kernel. Updates the handling of Gaussian process arguments so that only overridden settings need to be passed by the user when making changes. Settings are now defined, and documented, in `gp_opts()`. The length scale is now defined using a log normal truncated prior with a mean of 21 days and a standard deviation of 7 days truncated at 3 days and the length of the data by default. This prior is an area of active research and so may change in future releases.
* Updates the over dispersion prior to be `1 / sqrt(half_normal(0, rho_prior))` based on [this](https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations) advice and as the over dispersion being measured is in reports and not infections and hence a priori there is not strong evidence for over dispersion (which may be the case for infections) so the previous prior was overly weighted towards this.
* Updates the interface for the observation model with arguments now  passed using `obs_opts()`. This removes `week_effect` and `family` from the main argument list which will allow for future extensions. Also adds a new argument `scale` which controls the uncertain fraction of cases that are eventually observed (defined as normally distributed). Setting this parameter will not 
impact Rt estimates.
* Updates the interface to the Rt settings with all arguments passed via `rt`, using `rt_opts()`, this includes the initial prior,`use_breakpoints`, and `future`. Adds a new helper argument `rw` which enables easy parameterisation of a fixed length random walk. These changes also help make it clear that these arguments only impact the Rt generative model and not the back calculation model.
* Adds an adjustment for population susceptibility based on that used in [`{epidemia}`](https://github.com/ImperialCollegeLondon/epidemia) when Rt is fixed into the future (set by passing a population to `rt_opts(pop = initial susceptible population)`. Note this only impacts case forecasts and not output Rt estimates and only impacts estimates at all beyond the forecast horizon as those based on data already account for population susceptibility by definition. The impact of this assumption can be explored using `simulate_infections` (by updating `est$arg$pop` in the example).
* Adds `truncation` as a new argument to `estimate_infections` and higher level functions. This takes output from `trunc_opts()` and allows for internally adjusting observed cases for truncation. A new method `estimate_truncation` has also been added to support estimating a log normal truncation distribution from archived versions of the same data set though this method is currently experimental.
* Adds `estimate_delay` as a user friendly wrapper around `bootstrapped_dist_fit`. 

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
* Adds a pooling parameter for the standard deviation of breakpoint effects.
* Updated all documentation and added `{lifecycle}` badges to all functions to indicate development stage.

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
