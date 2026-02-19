# Package index

## Estimation + Reporting

Functions that facilitate end-to-end analysis including imputing cases
by infection, estimating Rt and reporting results.

- [`epinow()`](https://epiforecasts.io/EpiNow2/reference/epinow.md)
  **\[stable\]** : Real-time Rt Estimation, Forecasting and Reporting
- [`epinow2_cmdstan_model()`](https://epiforecasts.io/EpiNow2/reference/epinow2_cmdstan_model.md)
  : Load and compile an EpiNow2 cmdstanr model
- [`get_parameters()`](https://epiforecasts.io/EpiNow2/reference/get_parameters.md)
  **\[experimental\]** : Get parameters from distributions or fitted
  models
- [`get_samples()`](https://epiforecasts.io/EpiNow2/reference/get_samples.md)
  **\[stable\]** : Get posterior samples from a fitted model
- [`print(`*`<epinowfit>`*`)`](https://epiforecasts.io/EpiNow2/reference/print.epinowfit.md)
  : Print information about an object that has resulted from a model
  fit.
- [`regional_epinow()`](https://epiforecasts.io/EpiNow2/reference/regional_epinow.md)
  **\[maturing\]** : Real-time Rt Estimation, Forecasting and Reporting
  by Region
- [`summary(`*`<epinow>`*`)`](https://epiforecasts.io/EpiNow2/reference/summary.epinow.md)
  **\[stable\]** : Summary output from epinow

## Estimate, Simulate, and Forecast Parameters

Function to estimate, simulate and forecast parameters of interest.

- [`estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.md)
  **\[maturing\]** : Estimate Infections, the Time-Varying Reproduction
  Number and the Rate of Growth
- [`forecast_infections()`](https://epiforecasts.io/EpiNow2/reference/forecast_infections.md)
  **\[stable\]** : Forecast infections from a given fit and trajectory
  of the time-varying reproduction number
- [`estimate_secondary()`](https://epiforecasts.io/EpiNow2/reference/estimate_secondary.md)
  **\[stable\]** : Estimate a Secondary Observation from a Primary
  Observation
- [`forecast_secondary()`](https://epiforecasts.io/EpiNow2/reference/forecast_secondary.md)
  **\[experimental\]** : Forecast Secondary Observations Given a Fit
  from estimate_secondary
- [`estimate_delay()`](https://epiforecasts.io/EpiNow2/reference/estimate_delay.md)
  **\[maturing\]** : Estimate a Delay Distribution
- [`estimate_truncation()`](https://epiforecasts.io/EpiNow2/reference/estimate_truncation.md)
  **\[stable\]** : Estimate Truncation of Observed Data

## Specify Arguments

Functions used by estimate_infections

- [`backcalc_opts()`](https://epiforecasts.io/EpiNow2/reference/backcalc_opts.md)
  **\[stable\]** : Back Calculation Options
- [`delay_opts()`](https://epiforecasts.io/EpiNow2/reference/delay_opts.md)
  **\[stable\]** : Delay Distribution Options
- [`filter_opts()`](https://epiforecasts.io/EpiNow2/reference/filter_opts.md)
  **\[maturing\]** : Filter Options for a Target Region
- [`forecast_opts()`](https://epiforecasts.io/EpiNow2/reference/forecast_opts.md)
  **\[stable\]** : Forecast options
- [`gt_opts()`](https://epiforecasts.io/EpiNow2/reference/generation_time_opts.md)
  [`generation_time_opts()`](https://epiforecasts.io/EpiNow2/reference/generation_time_opts.md)
  **\[stable\]** : Generation Time Distribution Options
- [`gp_opts()`](https://epiforecasts.io/EpiNow2/reference/gp_opts.md)
  **\[stable\]** : Approximate Gaussian Process Settings
- [`obs_opts()`](https://epiforecasts.io/EpiNow2/reference/obs_opts.md)
  **\[stable\]** : Observation Model Options
- [`rt_opts()`](https://epiforecasts.io/EpiNow2/reference/rt_opts.md)
  **\[stable\]** : Time-Varying Reproduction Number Options
- [`secondary_opts()`](https://epiforecasts.io/EpiNow2/reference/secondary_opts.md)
  **\[stable\]** : Secondary Reports Options
- [`stan_laplace_opts()`](https://epiforecasts.io/EpiNow2/reference/stan_laplace_opts.md)
  **\[experimental\]** : Stan Laplace algorithm Options
- [`stan_opts()`](https://epiforecasts.io/EpiNow2/reference/stan_opts.md)
  **\[stable\]** : Stan Options
- [`stan_pathfinder_opts()`](https://epiforecasts.io/EpiNow2/reference/stan_pathfinder_opts.md)
  **\[experimental\]** : Stan pathfinder algorithm Options
- [`stan_sampling_opts()`](https://epiforecasts.io/EpiNow2/reference/stan_sampling_opts.md)
  **\[stable\]** : Stan Sampling Options
- [`stan_vb_opts()`](https://epiforecasts.io/EpiNow2/reference/stan_vb_opts.md)
  **\[stable\]** : Stan Variational Bayes Options
- [`trunc_opts()`](https://epiforecasts.io/EpiNow2/reference/trunc_opts.md)
  **\[stable\]** : Truncation Distribution Options
- [`opts_list()`](https://epiforecasts.io/EpiNow2/reference/opts_list.md)
  **\[maturing\]** : Forecast optiong

## Preprocess Data

Functions used for preprocessing data

- [`fill_missing()`](https://epiforecasts.io/EpiNow2/reference/fill_missing.md)
  **\[experimental\]** : Fill missing data in a data set to prepare it
  for use within the package

- [`add_breakpoints()`](https://epiforecasts.io/EpiNow2/reference/add_breakpoints.md)
  : Add breakpoints to certain dates in a data set.

- [`filter_leading_zeros()`](https://epiforecasts.io/EpiNow2/reference/filter_leading_zeros.md)
  : Filter leading zeros from a data set.

- [`apply_zero_threshold()`](https://epiforecasts.io/EpiNow2/reference/apply_zero_threshold.md)
  :

  Convert zero case counts to `NA` (missing) if the 7-day average is
  above a threshold.

## Regional Analysis

Functions used for summarising across regions (designed for use with
regional_epinow)

- [`regional_summary()`](https://epiforecasts.io/EpiNow2/reference/regional_summary.md)
  **\[maturing\]** : Regional Summary Output
- [`regional_runtimes()`](https://epiforecasts.io/EpiNow2/reference/regional_runtimes.md)
  **\[maturing\]** : Summarise Regional Runtimes
- [`get_regional_results()`](https://epiforecasts.io/EpiNow2/reference/get_regional_results.md)
  **\[stable\]** : Get Combined Regional Results

## Summarise Results

Functions for summarising results

- [`summary(`*`<epinow>`*`)`](https://epiforecasts.io/EpiNow2/reference/summary.epinow.md)
  **\[stable\]** : Summary output from epinow
- [`summary(`*`<estimate_infections>`*`)`](https://epiforecasts.io/EpiNow2/reference/summary.estimate_infections.md)
  **\[stable\]** : Summary output from estimate_infections
- [`summary(`*`<estimate_secondary>`*`)`](https://epiforecasts.io/EpiNow2/reference/summary.estimate_secondary.md)
  **\[stable\]** : Summarise results from estimate_secondary
- [`summary(`*`<estimate_truncation>`*`)`](https://epiforecasts.io/EpiNow2/reference/summary.estimate_truncation.md)
  **\[stable\]** : Summarise results from estimate_truncation
- [`summary(`*`<forecast_infections>`*`)`](https://epiforecasts.io/EpiNow2/reference/summary.forecast_infections.md)
  **\[stable\]** : Summary output from forecast_infections
- [`backcalc_opts()`](https://epiforecasts.io/EpiNow2/reference/backcalc_opts.md)
  **\[stable\]** : Back Calculation Options
- [`calc_CrI()`](https://epiforecasts.io/EpiNow2/reference/calc_CrI.md)
  **\[stable\]** : Calculate Credible Interval
- [`calc_CrIs()`](https://epiforecasts.io/EpiNow2/reference/calc_CrIs.md)
  **\[stable\]** : Calculate Credible Intervals
- [`calc_summary_measures()`](https://epiforecasts.io/EpiNow2/reference/calc_summary_measures.md)
  **\[stable\]** : Calculate All Summary Measures
- [`calc_summary_stats()`](https://epiforecasts.io/EpiNow2/reference/calc_summary_stats.md)
  **\[stable\]** : Calculate Summary Statistics
- [`make_conf()`](https://epiforecasts.io/EpiNow2/reference/make_conf.md)
  **\[stable\]** : Format Credible Intervals
- [`map_prob_change()`](https://epiforecasts.io/EpiNow2/reference/map_prob_change.md)
  **\[stable\]** : Categorise the Probability of Change for Rt

## Plot Results

Plot generated results

- [`plot(`*`<dist_spec>`*`)`](https://epiforecasts.io/EpiNow2/reference/plot.dist_spec.md)
  **\[experimental\]** : Plot PMF and CDF for a dist_spec object
- [`plot(`*`<estimate_infections>`*`)`](https://epiforecasts.io/EpiNow2/reference/plot.estimate_infections.md)
  **\[maturing\]** : Plot method for estimate_infections
- [`plot(`*`<estimate_secondary>`*`)`](https://epiforecasts.io/EpiNow2/reference/plot.estimate_secondary.md)
  **\[experimental\]** : Plot method for estimate_secondary
- [`plot(`*`<estimate_truncation>`*`)`](https://epiforecasts.io/EpiNow2/reference/plot.estimate_truncation.md)
  **\[experimental\]** : Plot method for estimate_truncation
- [`plot(`*`<forecast_infections>`*`)`](https://epiforecasts.io/EpiNow2/reference/plot.forecast_infections.md)
  **\[maturing\]** : Plot method for forecast_infections
- [`plot(`*`<forecast_secondary>`*`)`](https://epiforecasts.io/EpiNow2/reference/plot.forecast_secondary.md)
  **\[stable\]** : Plot method for forecast_secondary objects
- [`plot_CrIs()`](https://epiforecasts.io/EpiNow2/reference/plot_CrIs.md)
  **\[stable\]** : Plot EpiNow2 Credible Intervals
- [`plot_estimates()`](https://epiforecasts.io/EpiNow2/reference/plot_estimates.md)
  **\[questioning\]** : Plot Estimates
- [`plot_summary()`](https://epiforecasts.io/EpiNow2/reference/plot_summary.md)
  **\[questioning\]** : Plot a Summary of the Latest Results
- [`report_plots()`](https://epiforecasts.io/EpiNow2/reference/report_plots.md)
  **\[questioning\]** : Report plots

## Report Results

Functions to report results

- [`report_plots()`](https://epiforecasts.io/EpiNow2/reference/report_plots.md)
  **\[questioning\]** : Report plots
- [`report_summary()`](https://epiforecasts.io/EpiNow2/reference/report_summary.md)
  **\[questioning\]** : Provide Summary Statistics for Estimated
  Infections and Rt

## Distribution Functions

Functions to define and parameterise distributions

- [`LogNormal()`](https://epiforecasts.io/EpiNow2/reference/Distributions.md)
  [`Gamma()`](https://epiforecasts.io/EpiNow2/reference/Distributions.md)
  [`Normal()`](https://epiforecasts.io/EpiNow2/reference/Distributions.md)
  [`Fixed()`](https://epiforecasts.io/EpiNow2/reference/Distributions.md)
  [`NonParametric()`](https://epiforecasts.io/EpiNow2/reference/Distributions.md)
  : Probability distributions

- [`c(`*`<dist_spec>`*`)`](https://epiforecasts.io/EpiNow2/reference/c.dist_spec.md)
  **\[experimental\]** : Combines multiple delay distributions for
  further processing

- [`collapse(`*`<dist_spec>`*`)`](https://epiforecasts.io/EpiNow2/reference/collapse.md)
  **\[experimental\]** : Collapse nonparametric distributions in a
  \<dist_spec\>

- [`discretise(`*`<dist_spec>`*`)`](https://epiforecasts.io/EpiNow2/reference/discretise.md)
  [`discretize()`](https://epiforecasts.io/EpiNow2/reference/discretise.md)
  **\[experimental\]** : Discretise a \<dist_spec\>

- [`` `==`( ``*`<dist_spec>`*`)`](https://epiforecasts.io/EpiNow2/reference/equals-.dist_spec.md)
  [`` `!=`( ``*`<dist_spec>`*`)`](https://epiforecasts.io/EpiNow2/reference/equals-.dist_spec.md)
  : Compares two delay distributions

- [`fix_parameters(`*`<dist_spec>`*`)`](https://epiforecasts.io/EpiNow2/reference/fix_parameters.md)
  **\[experimental\]** :

  Fix the parameters of a `<dist_spec>`

- [`get_parameters()`](https://epiforecasts.io/EpiNow2/reference/get_parameters.md)
  **\[experimental\]** : Get parameters from distributions or fitted
  models

- [`is_constrained(`*`<dist_spec>`*`)`](https://epiforecasts.io/EpiNow2/reference/is_constrained.md)
  **\[experimental\]** : Check if a \<dist_spec\> is constrained, i.e.
  has a finite maximum or nonzero CDF cutoff.

- [`max(`*`<dist_spec>`*`)`](https://epiforecasts.io/EpiNow2/reference/max.dist_spec.md)
  **\[experimental\]** : Returns the maximum of one or more delay
  distribution

- [`mean(`*`<dist_spec>`*`)`](https://epiforecasts.io/EpiNow2/reference/mean.dist_spec.md)
  **\[experimental\]** : Returns the mean of one or more delay
  distribution

- [`new_dist_spec()`](https://epiforecasts.io/EpiNow2/reference/new_dist_spec.md)
  **\[experimental\]** :

  Internal function for generating a `dist_spec` given parameters and a
  distribution.

- [`plot(`*`<dist_spec>`*`)`](https://epiforecasts.io/EpiNow2/reference/plot.dist_spec.md)
  **\[experimental\]** : Plot PMF and CDF for a dist_spec object

- [`` `+`( ``*`<dist_spec>`*`)`](https://epiforecasts.io/EpiNow2/reference/plus-.dist_spec.md)
  **\[experimental\]** : Creates a delay distribution as the sum of two
  other delay distributions.

- [`print(`*`<dist_spec>`*`)`](https://epiforecasts.io/EpiNow2/reference/print.dist_spec.md)
  **\[experimental\]** : Prints the parameters of one or more delay
  distributions

- [`bound_dist()`](https://epiforecasts.io/EpiNow2/reference/bound_dist.md)
  **\[experimental\]** :

  Define bounds of a `<dist_spec>`

- [`get_pmf()`](https://epiforecasts.io/EpiNow2/reference/get_pmf.md)
  **\[experimental\]** : Get the probability mass function of a
  nonparametric distribution

- [`get_distribution()`](https://epiforecasts.io/EpiNow2/reference/get_distribution.md)
  **\[experimental\]** :

  Get the distribution of a `<dist_spec>`

## Fit Delay Distributions

Functions to fit delay distributions

- [`bootstrapped_dist_fit()`](https://epiforecasts.io/EpiNow2/reference/bootstrapped_dist_fit.md)
  **\[stable\]** : Fit a Subsampled Bootstrap to Integer Values and
  Summarise Distribution Parameters
- [`dist_fit()`](https://epiforecasts.io/EpiNow2/reference/dist_fit.md)
  **\[stable\]** : Fit an Integer Adjusted Exponential, Gamma or
  Lognormal distributions

## Simulation

Functions to help with simulating data or mapping to reported cases

- [`simulate_infections()`](https://epiforecasts.io/EpiNow2/reference/simulate_infections.md)
  : Simulate infections using the renewal equation
- [`simulate_secondary()`](https://epiforecasts.io/EpiNow2/reference/simulate_secondary.md)
  : Simulate secondary observations from primary observations
- [`convolve_and_scale()`](https://epiforecasts.io/EpiNow2/reference/convolve_and_scale.md)
  : Convolve and scale a time series

## Data

Package datasets that may be used to parameterise other functions or in
examples

- [`example_generation_time`](https://epiforecasts.io/EpiNow2/reference/example_generation_time.md)
  **\[stable\]** : Example generation time
- [`example_incubation_period`](https://epiforecasts.io/EpiNow2/reference/example_incubation_period.md)
  **\[stable\]** : Example incubation period
- [`example_reporting_delay`](https://epiforecasts.io/EpiNow2/reference/example_reporting_delay.md)
  **\[stable\]** : Example reporting delay
- [`example_confirmed`](https://epiforecasts.io/EpiNow2/reference/example_confirmed.md)
  **\[stable\]** : Example Confirmed Case Data Set
- [`example_truncated`](https://epiforecasts.io/EpiNow2/reference/example_truncated.md)
  **\[stable\]** : Example Case Data Set with Truncation

## Data Access

Functions for extracting data from objects or getting data from sources

- [`get_distribution()`](https://epiforecasts.io/EpiNow2/reference/get_distribution.md)
  **\[experimental\]** :

  Get the distribution of a `<dist_spec>`

- [`get_parameters()`](https://epiforecasts.io/EpiNow2/reference/get_parameters.md)
  **\[experimental\]** : Get parameters from distributions or fitted
  models

- [`get_pmf()`](https://epiforecasts.io/EpiNow2/reference/get_pmf.md)
  **\[experimental\]** : Get the probability mass function of a
  nonparametric distribution

- [`get_predictions()`](https://epiforecasts.io/EpiNow2/reference/get_predictions.md)
  **\[stable\]** : Get predictions from a fitted model

- [`get_regional_results()`](https://epiforecasts.io/EpiNow2/reference/get_regional_results.md)
  **\[stable\]** : Get Combined Regional Results

- [`get_samples()`](https://epiforecasts.io/EpiNow2/reference/get_samples.md)
  **\[stable\]** : Get posterior samples from a fitted model

- [`extract_CrIs()`](https://epiforecasts.io/EpiNow2/reference/extract_CrIs.md)
  **\[stable\]** : Extract Credible Intervals Present

- [`extract_inits()`](https://epiforecasts.io/EpiNow2/reference/extract_inits.md)
  **\[experimental\]** : Generate initial conditions from a Stan fit

- [`extract_samples()`](https://epiforecasts.io/EpiNow2/reference/extract_samples.md)
  : Extract all samples from a stan fit

- [`extract_stan_param()`](https://epiforecasts.io/EpiNow2/reference/extract_stan_param.md)
  **\[stable\]** : Extract a parameter summary from a Stan object

## Data Cleaning

Functions for cleaning data

- [`clean_nowcasts()`](https://epiforecasts.io/EpiNow2/reference/clean_nowcasts.md)
  **\[stable\]** : Clean Nowcasts for a Supplied Date
- [`clean_regions()`](https://epiforecasts.io/EpiNow2/reference/clean_regions.md)
  **\[stable\]** : Clean Regions

## Setup

Functions used for setting up functionality

- [`setup_default_logging()`](https://epiforecasts.io/EpiNow2/reference/setup_default_logging.md)
  **\[questioning\]** : Setup Default Logging
- [`setup_future()`](https://epiforecasts.io/EpiNow2/reference/setup_future.md)
  **\[stable\]** : Set up Future Backend
- [`setup_logging()`](https://epiforecasts.io/EpiNow2/reference/setup_logging.md)
  **\[questioning\]** : Setup Logging

## Utilities

Utility functions

- [`run_region()`](https://epiforecasts.io/EpiNow2/reference/run_region.md)
  **\[maturing\]** : Run epinow with Regional Processing Code
- [`expose_stan_fns()`](https://epiforecasts.io/EpiNow2/reference/expose_stan_fns.md)
  **\[stable\]** : Expose internal package stan functions in R
- [`convert_to_logmean()`](https://epiforecasts.io/EpiNow2/reference/convert_to_logmean.md)
  **\[stable\]** : Convert mean and sd to log mean for a log normal
  distribution
- [`convert_to_logsd()`](https://epiforecasts.io/EpiNow2/reference/convert_to_logsd.md)
  **\[stable\]** : Convert mean and sd to log standard deviation for a
  log normal distribution
- [`growth_to_R()`](https://epiforecasts.io/EpiNow2/reference/growth_to_R.md)
  **\[questioning\]** : Convert Growth Rates to Reproduction numbers.
- [`R_to_growth()`](https://epiforecasts.io/EpiNow2/reference/R_to_growth.md)
  **\[questioning\]** : Convert Reproduction Numbers to Growth Rates
- [`update_secondary_args()`](https://epiforecasts.io/EpiNow2/reference/update_secondary_args.md)
  **\[stable\]** : Update estimate_secondary default priors

## Internal

- [`EpiNow2`](https://epiforecasts.io/EpiNow2/reference/EpiNow2-package.md)
  [`EpiNow2-package`](https://epiforecasts.io/EpiNow2/reference/EpiNow2-package.md)
  : EpiNow2: Estimate and Forecast Real-Time Infection Dynamics

- [`add_day_of_week()`](https://epiforecasts.io/EpiNow2/reference/add_day_of_week.md)
  : Adds a day of the week vector

- [`add_horizon()`](https://epiforecasts.io/EpiNow2/reference/add_horizon.md)
  : Add missing values for future dates

- [`allocate_delays()`](https://epiforecasts.io/EpiNow2/reference/allocate_delays.md)
  **\[stable\]** : Allocate Delays into Required Stan Format

- [`allocate_empty()`](https://epiforecasts.io/EpiNow2/reference/allocate_empty.md)
  **\[stable\]** : Allocate Empty Parameters to a List

- [`apply_default_cdf_cutoff()`](https://epiforecasts.io/EpiNow2/reference/apply_default_cdf_cutoff.md)
  : Apply default CDF cutoff to a \<dist_spec\> if it is unconstrained

- [`` `$`( ``*`<epinow>`*`)`](https://epiforecasts.io/EpiNow2/reference/cash-.epinow.md)
  **\[deprecated\]** : Extract elements from epinow objects with
  deprecated warnings

- [`` `$`( ``*`<estimate_infections>`*`)`](https://epiforecasts.io/EpiNow2/reference/cash-.estimate_infections.md)
  **\[deprecated\]** : Extract elements from estimate_infections objects
  with deprecated warnings

- [`` `$`( ``*`<estimate_secondary>`*`)`](https://epiforecasts.io/EpiNow2/reference/cash-.estimate_secondary.md)
  **\[deprecated\]** : Extract elements from estimate_secondary objects
  with deprecated warnings

- [`check_generation_time()`](https://epiforecasts.io/EpiNow2/reference/check_generation_time.md)
  **\[stable\]** : Validate probability distribution for using as
  generation time

- [`check_reports_valid()`](https://epiforecasts.io/EpiNow2/reference/check_reports_valid.md)
  **\[stable\]** : Validate data input

- [`check_sparse_pmf_tail()`](https://epiforecasts.io/EpiNow2/reference/check_sparse_pmf_tail.md)
  : Check that PMF tail is not sparse

- [`check_stan_delay()`](https://epiforecasts.io/EpiNow2/reference/check_stan_delay.md)
  **\[stable\]** : Validate probability distribution for passing to stan

- [`check_truncation_length()`](https://epiforecasts.io/EpiNow2/reference/check_truncation_length.md)
  : Check and warn if truncation distribution is longer than observed
  time

- [`combine_tv_and_static_params()`](https://epiforecasts.io/EpiNow2/reference/combine_tv_and_static_params.md)
  : Combine time-varying and static parameters

- [`construct_output()`](https://epiforecasts.io/EpiNow2/reference/construct_output.md)
  **\[stable\]** : Construct Output

- [`convert_to_natural()`](https://epiforecasts.io/EpiNow2/reference/convert_to_natural.md)
  **\[experimental\]** : Internal function for converting parameters to
  natural parameters.

- [`copy_results_to_latest()`](https://epiforecasts.io/EpiNow2/reference/copy_results_to_latest.md)
  **\[questioning\]** : Copy Results From Dated Folder to Latest

- [`create_backcalc_data()`](https://epiforecasts.io/EpiNow2/reference/create_backcalc_data.md)
  **\[stable\]** : Create Back Calculation Data

- [`create_delay_inits()`](https://epiforecasts.io/EpiNow2/reference/create_delay_inits.md)
  : Create initial conditions for delays

- [`create_future_rt()`](https://epiforecasts.io/EpiNow2/reference/create_future_rt.md)
  **\[stable\]** : Construct the Required Future Rt assumption

- [`create_gp_data()`](https://epiforecasts.io/EpiNow2/reference/create_gp_data.md)
  **\[stable\]** : Create Gaussian Process Data

- [`create_infection_summary()`](https://epiforecasts.io/EpiNow2/reference/create_infection_summary.md)
  **\[stable\]** : Create summary output from infection estimation
  objects

- [`create_initial_conditions()`](https://epiforecasts.io/EpiNow2/reference/create_initial_conditions.md)
  **\[stable\]** : Create Initial Conditions Generating Function

- [`create_obs_model()`](https://epiforecasts.io/EpiNow2/reference/create_obs_model.md)
  **\[stable\]** : Create Observation Model Settings

- [`create_rt_data()`](https://epiforecasts.io/EpiNow2/reference/create_rt_data.md)
  **\[stable\]** : Create Time-varying Reproduction Number Data

- [`create_sampling_log_message()`](https://epiforecasts.io/EpiNow2/reference/create_sampling_log_message.md)
  : Create sampling log message

- [`create_shifted_cases()`](https://epiforecasts.io/EpiNow2/reference/create_shifted_cases.md)
  **\[stable\]** : Create Delay Shifted Cases

- [`create_stan_args()`](https://epiforecasts.io/EpiNow2/reference/create_stan_args.md)
  **\[stable\]** : Create a List of Stan Arguments

- [`create_stan_data()`](https://epiforecasts.io/EpiNow2/reference/create_stan_data.md)
  **\[stable\]** : Create Stan Data Required for estimate_infections

- [`create_stan_delays()`](https://epiforecasts.io/EpiNow2/reference/create_stan_delays.md)
  : Create delay variables for stan

- [`create_stan_params()`](https://epiforecasts.io/EpiNow2/reference/create_stan_params.md)
  : Create parameters for stan

- [`default_fill_missing_obs()`](https://epiforecasts.io/EpiNow2/reference/default_fill_missing_obs.md)
  **\[deprecated\]** : Temporary function to support the transition to
  full support of missing data.

- [`discrete_pmf()`](https://epiforecasts.io/EpiNow2/reference/discrete_pmf.md)
  **\[questioning\]** : Discretised probability mass function

- [`dist_spec_distributions()`](https://epiforecasts.io/EpiNow2/reference/dist_spec_distributions.md)
  **\[experimental\]** : Get parametric distribution types

- [`epinow2_rstan_model()`](https://epiforecasts.io/EpiNow2/reference/epinow2_rstan_model.md)
  : Load an EpiNow2 rstan model.

- [`epinow2_stan_model()`](https://epiforecasts.io/EpiNow2/reference/epinow2_stan_model.md)
  : Return a stan model object for the appropriate backend

- [`estimates_by_report_date()`](https://epiforecasts.io/EpiNow2/reference/estimates_by_report_date.md)
  **\[questioning\]** : Estimate Cases by Report Date

- [`extract_delay_params()`](https://epiforecasts.io/EpiNow2/reference/extract_delay_params.md)
  : Extract delay distributions from a fitted model

- [`extract_delays()`](https://epiforecasts.io/EpiNow2/reference/extract_delays.md)
  : Extract samples from all delay parameters

- [`extract_latent_state()`](https://epiforecasts.io/EpiNow2/reference/extract_latent_state.md)
  **\[stable\]** : Extract samples for a latent state from a Stan model

- [`extract_parameter_samples()`](https://epiforecasts.io/EpiNow2/reference/extract_parameter_samples.md)
  **\[deprecated\]** : Extract parameter samples from a Stan model

- [`extract_parameters()`](https://epiforecasts.io/EpiNow2/reference/extract_parameters.md)
  : Extract samples from all parameters

- [`extract_params()`](https://epiforecasts.io/EpiNow2/reference/extract_params.md)
  **\[experimental\]** : Extract parameter names

- [`extract_scalar_params()`](https://epiforecasts.io/EpiNow2/reference/extract_scalar_params.md)
  : Extract scalar parameters from a fitted model

- [`extract_single_dist()`](https://epiforecasts.io/EpiNow2/reference/extract_single_dist.md)
  **\[experimental\]** :

  Extract a single element of a composite `<dist_spec>`

- [`fit_model()`](https://epiforecasts.io/EpiNow2/reference/fit_model.md)
  : Fit a model using the chosen backend.

- [`fit_model_approximate()`](https://epiforecasts.io/EpiNow2/reference/fit_model_approximate.md)
  **\[maturing\]** : Fit a Stan Model using an approximate method

- [`fit_model_with_nuts()`](https://epiforecasts.io/EpiNow2/reference/fit_model_with_nuts.md)
  **\[maturing\]** : Fit a Stan Model using the NUTs sampler

- [`format_fit()`](https://epiforecasts.io/EpiNow2/reference/format_fit.md)
  **\[stable\]** : Format Posterior Samples

- [`format_quantile_predictions()`](https://epiforecasts.io/EpiNow2/reference/format_quantile_predictions.md)
  : Format quantile predictions

- [`format_sample_predictions()`](https://epiforecasts.io/EpiNow2/reference/format_sample_predictions.md)
  : Format sample predictions

- [`format_samples_with_dates()`](https://epiforecasts.io/EpiNow2/reference/format_samples_with_dates.md)
  : Format raw Stan samples with dates and metadata

- [`format_simulation_output()`](https://epiforecasts.io/EpiNow2/reference/format_simulation_output.md)
  **\[stable\]** : Format Simulation Output from Stan

- [`get_element()`](https://epiforecasts.io/EpiNow2/reference/get_element.md)
  :

  Extracts an element of a `<dist_spec>`

- [`get_raw_result()`](https://epiforecasts.io/EpiNow2/reference/get_raw_result.md)
  **\[stable\]** : Get a Single Raw Result

- [`get_regions()`](https://epiforecasts.io/EpiNow2/reference/get_regions.md)
  **\[stable\]** : Get Folders with Results

- [`get_regions_with_most_reports()`](https://epiforecasts.io/EpiNow2/reference/get_regions_with_most_reports.md)
  **\[stable\]** : Get Regions with Most Reported Cases

- [`get_seeding_time()`](https://epiforecasts.io/EpiNow2/reference/get_seeding_time.md)
  : Estimate seeding time from delays and generation time

- [`lapply_func()`](https://epiforecasts.io/EpiNow2/reference/lapply_func.md)
  : Choose a parallel or sequential apply function

- [`lower_bounds()`](https://epiforecasts.io/EpiNow2/reference/lower_bounds.md)
  **\[experimental\]** : Get the lower bounds of the parameters of a
  distribution

- [`make_param()`](https://epiforecasts.io/EpiNow2/reference/make_param.md)
  : Internal function to create a parameter list

- [`match_output_arguments()`](https://epiforecasts.io/EpiNow2/reference/match_output_arguments.md)
  **\[stable\]** : Match User Supplied Arguments with Supported Options

- [`merge_trunc_pred_obs()`](https://epiforecasts.io/EpiNow2/reference/merge_trunc_pred_obs.md)
  : Merge truncation predictions with observations for display

- [`natural_params()`](https://epiforecasts.io/EpiNow2/reference/natural_params.md)
  **\[experimental\]** : Get the names of the natural parameters of a
  distribution

- [`ndist()`](https://epiforecasts.io/EpiNow2/reference/ndist.md) :

  Calculate the number of distributions in a `<dist_spec>`

- [`pad_reported_cases()`](https://epiforecasts.io/EpiNow2/reference/pad_reported_cases.md)
  : Pads reported cases with daily initial zeros

- [`posterior_to_normal()`](https://epiforecasts.io/EpiNow2/reference/posterior_to_normal.md)
  : Create a Normal distribution from posterior samples

- [`prepare_truncation_obs()`](https://epiforecasts.io/EpiNow2/reference/prepare_truncation_obs.md)
  : Prepare truncation observations for Stan

- [`process_region()`](https://epiforecasts.io/EpiNow2/reference/process_region.md)
  **\[maturing\]** : Process regional estimate

- [`process_regions()`](https://epiforecasts.io/EpiNow2/reference/process_regions.md)
  **\[stable\]** : Process all Region Estimates

- [`reconstruct_delay()`](https://epiforecasts.io/EpiNow2/reference/reconstruct_delay.md)
  : Reconstruct a dist_spec from stored stan data and posterior

- [`reconstruct_nonparametric()`](https://epiforecasts.io/EpiNow2/reference/reconstruct_nonparametric.md)
  : Reconstruct a nonparametric delay distribution

- [`reconstruct_parametric()`](https://epiforecasts.io/EpiNow2/reference/reconstruct_parametric.md)
  : Reconstruct a parametric delay distribution

- [`regional_runtimes()`](https://epiforecasts.io/EpiNow2/reference/regional_runtimes.md)
  **\[maturing\]** : Summarise Regional Runtimes

- [`save_estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/save_estimate_infections.md)
  **\[stable\]** : Save Estimated Infections

- [`save_input()`](https://epiforecasts.io/EpiNow2/reference/save_input.md)
  **\[stable\]** : Save Observed Data

- [`sd(`*`<dist_spec>`*`)`](https://epiforecasts.io/EpiNow2/reference/sd.md)
  **\[experimental\]** : Returns the standard deviation of one or more
  delay distribution

- [`select_plots()`](https://epiforecasts.io/EpiNow2/reference/select_plots.md)
  :

  Internal helper function to select plots from those created by
  [`report_plots()`](https://epiforecasts.io/EpiNow2/reference/report_plots.md)

- [`set_dt_single_thread()`](https://epiforecasts.io/EpiNow2/reference/set_dt_single_thread.md)
  : Set to Single Threading

- [`setup_dt()`](https://epiforecasts.io/EpiNow2/reference/setup_dt.md)
  **\[stable\]** : Convert to Data Table

- [`setup_target_folder()`](https://epiforecasts.io/EpiNow2/reference/setup_target_folder.md)
  **\[stable\]** : Setup Target Folder for Saving

- [`stable_convolve()`](https://epiforecasts.io/EpiNow2/reference/stable_convolve.md)
  : Numerically stable convolution function for two pmf vectors

- [`` `[[`( ``*`<epinow>`*`)`](https://epiforecasts.io/EpiNow2/reference/sub-sub-.epinow.md)
  **\[deprecated\]** : Extract elements from epinow objects with bracket
  notation

- [`` `[[`( ``*`<estimate_infections>`*`)`](https://epiforecasts.io/EpiNow2/reference/sub-sub-.estimate_infections.md)
  **\[deprecated\]** : Extract elements from estimate_infections objects
  with bracket notation

- [`` `[[`( ``*`<estimate_secondary>`*`)`](https://epiforecasts.io/EpiNow2/reference/sub-sub-.estimate_secondary.md)
  **\[deprecated\]** : Extract elements from estimate_secondary objects
  with bracket notation

- [`summarise_key_measures()`](https://epiforecasts.io/EpiNow2/reference/summarise_key_measures.md)
  **\[maturing\]** : Summarise rt and cases

- [`summarise_results()`](https://epiforecasts.io/EpiNow2/reference/summarise_results.md)
  **\[questioning\]** : Summarise Real-time Results

- [`update_horizon()`](https://epiforecasts.io/EpiNow2/reference/update_horizon.md)
  **\[stable\]** : Updates Forecast Horizon Based on Input Data and
  Target
