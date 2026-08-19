# Package index

## Estimation + Reporting

Functions that facilitate end-to-end analysis including imputing cases
by infection, estimating Rt and reporting results.

- [`as_forecast_sample(`*`<estimate_infections>`*`)`](https://epiforecasts.io/EpiNow2/dev/reference/as_forecast_sample.md)
  [`as_forecast_sample(`*`<epinow>`*`)`](https://epiforecasts.io/EpiNow2/dev/reference/as_forecast_sample.md)
  [`as_forecast_sample(`*`<forecast_secondary>`*`)`](https://epiforecasts.io/EpiNow2/dev/reference/as_forecast_sample.md)
  [`as_forecast_sample(`*`<estimate_truncation>`*`)`](https://epiforecasts.io/EpiNow2/dev/reference/as_forecast_sample.md)
  **\[experimental\]** :

  Convert EpiNow2 model output to a `forecast_sample` object

- [`epinow()`](https://epiforecasts.io/EpiNow2/dev/reference/epinow.md)
  : Real-time Rt Estimation, Forecasting and Reporting

- [`epinow2_cmdstan_model()`](https://epiforecasts.io/EpiNow2/dev/reference/epinow2_cmdstan_model.md)
  : Load and compile an EpiNow2 cmdstanr model

- [`get_parameters()`](https://epiforecasts.io/EpiNow2/dev/reference/get_parameters.md)
  : Get parameters from distributions or fitted models

- [`get_samples()`](https://epiforecasts.io/EpiNow2/dev/reference/get_samples.md)
  : Get posterior samples from a fitted model

- [`print(`*`<epinowfit>`*`)`](https://epiforecasts.io/EpiNow2/dev/reference/print.epinowfit.md)
  : Print information about an object that has resulted from a model
  fit.

- [`regional_epinow()`](https://epiforecasts.io/EpiNow2/dev/reference/regional_epinow.md)
  : Real-time Rt Estimation, Forecasting and Reporting by Region

- [`summary(`*`<epinow>`*`)`](https://epiforecasts.io/EpiNow2/dev/reference/summary.epinow.md)
  : Summary output from epinow

## Estimate, Simulate, and Forecast Parameters

Function to estimate, simulate and forecast parameters of interest.

- [`estimate_infections()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_infections.md)
  : Estimate Infections, the Time-Varying Reproduction Number and the
  Rate of Growth
- [`forecast_infections()`](https://epiforecasts.io/EpiNow2/dev/reference/forecast_infections.md)
  : Forecast infections from a given fit and trajectory of the
  time-varying reproduction number
- [`estimate_secondary()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_secondary.md)
  : Estimate a Secondary Observation from a Primary Observation
- [`forecast_secondary()`](https://epiforecasts.io/EpiNow2/dev/reference/forecast_secondary.md)
  : Forecast Secondary Observations Given a Fit from estimate_secondary
- [`estimate_delay()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_delay.md)
  **\[deprecated\]** : Estimate a Delay Distribution
- [`estimate_dist()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_dist.md)
  **\[experimental\]** : Estimate a delay distribution using
  primarycensored
- [`estimate_truncation()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_truncation.md)
  : Estimate truncation of observed data

## Specify Arguments

Functions used by estimate_infections

- [`backcalc_opts()`](https://epiforecasts.io/EpiNow2/dev/reference/backcalc_opts.md)
  : Back Calculation Options
- [`delay_opts()`](https://epiforecasts.io/EpiNow2/dev/reference/delay_opts.md)
  : Delay Distribution Options
- [`filter_opts()`](https://epiforecasts.io/EpiNow2/dev/reference/filter_opts.md)
  : Filter Options for a Target Region
- [`forecast_opts()`](https://epiforecasts.io/EpiNow2/dev/reference/forecast_opts.md)
  : Forecast options
- [`gt_opts()`](https://epiforecasts.io/EpiNow2/dev/reference/generation_time_opts.md)
  [`generation_time_opts()`](https://epiforecasts.io/EpiNow2/dev/reference/generation_time_opts.md)
  : Generation Time Distribution Options
- [`gp_opts()`](https://epiforecasts.io/EpiNow2/dev/reference/gp_opts.md)
  : Approximate Gaussian Process Settings
- [`obs_opts()`](https://epiforecasts.io/EpiNow2/dev/reference/obs_opts.md)
  : Observation Model Options
- [`rt_opts()`](https://epiforecasts.io/EpiNow2/dev/reference/rt_opts.md)
  : Time-Varying Reproduction Number Options
- [`secondary_opts()`](https://epiforecasts.io/EpiNow2/dev/reference/secondary_opts.md)
  : Secondary Reports Options
- [`stan_laplace_opts()`](https://epiforecasts.io/EpiNow2/dev/reference/stan_laplace_opts.md)
  **\[experimental\]** : Stan Laplace algorithm Options
- [`stan_opts()`](https://epiforecasts.io/EpiNow2/dev/reference/stan_opts.md)
  : Stan Options
- [`stan_pathfinder_opts()`](https://epiforecasts.io/EpiNow2/dev/reference/stan_pathfinder_opts.md)
  **\[experimental\]** : Stan pathfinder algorithm Options
- [`stan_sampling_opts()`](https://epiforecasts.io/EpiNow2/dev/reference/stan_sampling_opts.md)
  : Stan Sampling Options
- [`stan_vb_opts()`](https://epiforecasts.io/EpiNow2/dev/reference/stan_vb_opts.md)
  : Stan Variational Bayes Options
- [`trunc_opts()`](https://epiforecasts.io/EpiNow2/dev/reference/trunc_opts.md)
  : Truncation Distribution Options
- [`opts_list()`](https://epiforecasts.io/EpiNow2/dev/reference/opts_list.md)
  : Forecast optiong

## Preprocess Data

Functions used for preprocessing data

- [`fill_missing()`](https://epiforecasts.io/EpiNow2/dev/reference/fill_missing.md)
  : Fill missing data in a data set to prepare it for use within the
  package

- [`add_breakpoints()`](https://epiforecasts.io/EpiNow2/dev/reference/add_breakpoints.md)
  : Add breakpoints to certain dates in a data set.

- [`filter_leading_zeros()`](https://epiforecasts.io/EpiNow2/dev/reference/filter_leading_zeros.md)
  : Filter leading zeros from a data set.

- [`apply_zero_threshold()`](https://epiforecasts.io/EpiNow2/dev/reference/apply_zero_threshold.md)
  :

  Convert zero case counts to `NA` (missing) if the 7-day average is
  above a threshold.

## Regional Analysis

Functions used for summarising across regions (designed for use with
regional_epinow)

- [`regional_summary()`](https://epiforecasts.io/EpiNow2/dev/reference/regional_summary.md)
  : Regional Summary Output
- [`regional_runtimes()`](https://epiforecasts.io/EpiNow2/dev/reference/regional_runtimes.md)
  : Summarise Regional Runtimes
- [`get_regional_results()`](https://epiforecasts.io/EpiNow2/dev/reference/get_regional_results.md)
  : Get Combined Regional Results

## Summarise Results

Functions for summarising results

- [`summary(`*`<epinow>`*`)`](https://epiforecasts.io/EpiNow2/dev/reference/summary.epinow.md)
  : Summary output from epinow
- [`summary(`*`<estimate_dist>`*`)`](https://epiforecasts.io/EpiNow2/dev/reference/summary.estimate_dist.md)
  **\[experimental\]** : Summarise results from estimate_dist
- [`summary(`*`<estimate_infections>`*`)`](https://epiforecasts.io/EpiNow2/dev/reference/summary.estimate_infections.md)
  : Summary output from estimate_infections
- [`summary(`*`<estimate_secondary>`*`)`](https://epiforecasts.io/EpiNow2/dev/reference/summary.estimate_secondary.md)
  : Summarise results from estimate_secondary
- [`summary(`*`<estimate_truncation>`*`)`](https://epiforecasts.io/EpiNow2/dev/reference/summary.estimate_truncation.md)
  : Summarise results from estimate_truncation
- [`summary(`*`<forecast_infections>`*`)`](https://epiforecasts.io/EpiNow2/dev/reference/summary.forecast_infections.md)
  : Summary output from forecast_infections
- [`backcalc_opts()`](https://epiforecasts.io/EpiNow2/dev/reference/backcalc_opts.md)
  : Back Calculation Options
- [`calc_CrI()`](https://epiforecasts.io/EpiNow2/dev/reference/calc_CrI.md)
  : Calculate Credible Interval
- [`calc_CrIs()`](https://epiforecasts.io/EpiNow2/dev/reference/calc_CrIs.md)
  : Calculate Credible Intervals
- [`calc_summary_measures()`](https://epiforecasts.io/EpiNow2/dev/reference/calc_summary_measures.md)
  : Calculate All Summary Measures
- [`calc_summary_stats()`](https://epiforecasts.io/EpiNow2/dev/reference/calc_summary_stats.md)
  : Calculate Summary Statistics
- [`make_conf()`](https://epiforecasts.io/EpiNow2/dev/reference/make_conf.md)
  : Format Credible Intervals
- [`map_prob_change()`](https://epiforecasts.io/EpiNow2/dev/reference/map_prob_change.md)
  : Categorise the Probability of Change for Rt

## Plot Results

Plot generated results

- [`plot(`*`<dist_spec>`*`)`](https://epiforecasts.io/EpiNow2/dev/reference/plot.dist_spec.md)
  : Plot PMF and CDF for a dist_spec object
- [`plot(`*`<estimate_infections>`*`)`](https://epiforecasts.io/EpiNow2/dev/reference/plot.estimate_infections.md)
  : Plot method for estimate_infections
- [`plot(`*`<estimate_secondary>`*`)`](https://epiforecasts.io/EpiNow2/dev/reference/plot.estimate_secondary.md)
  : Plot method for estimate_secondary
- [`plot(`*`<estimate_truncation>`*`)`](https://epiforecasts.io/EpiNow2/dev/reference/plot.estimate_truncation.md)
  : Plot method for estimate_truncation
- [`plot(`*`<forecast_infections>`*`)`](https://epiforecasts.io/EpiNow2/dev/reference/plot.forecast_infections.md)
  : Plot method for forecast_infections
- [`plot(`*`<forecast_secondary>`*`)`](https://epiforecasts.io/EpiNow2/dev/reference/plot.forecast_secondary.md)
  : Plot method for forecast_secondary objects
- [`plot_CrIs()`](https://epiforecasts.io/EpiNow2/dev/reference/plot_CrIs.md)
  : Plot EpiNow2 Credible Intervals
- [`plot_estimates()`](https://epiforecasts.io/EpiNow2/dev/reference/plot_estimates.md)
  : Plot Estimates
- [`plot_summary()`](https://epiforecasts.io/EpiNow2/dev/reference/plot_summary.md)
  : Plot a Summary of the Latest Results
- [`report_plots()`](https://epiforecasts.io/EpiNow2/dev/reference/report_plots.md)
  **\[superseded\]** : Report plots

## Report Results

Functions to report results

- [`report_plots()`](https://epiforecasts.io/EpiNow2/dev/reference/report_plots.md)
  **\[superseded\]** : Report plots
- [`report_summary()`](https://epiforecasts.io/EpiNow2/dev/reference/report_summary.md)
  **\[superseded\]** : Provide Summary Statistics for Estimated
  Infections and Rt

## Distribution Functions

Functions to define and parameterise distributions

- [`LogNormal()`](https://epiforecasts.io/EpiNow2/dev/reference/Distributions.md)
  [`Gamma()`](https://epiforecasts.io/EpiNow2/dev/reference/Distributions.md)
  [`Normal()`](https://epiforecasts.io/EpiNow2/dev/reference/Distributions.md)
  [`Exp()`](https://epiforecasts.io/EpiNow2/dev/reference/Distributions.md)
  [`Weibull()`](https://epiforecasts.io/EpiNow2/dev/reference/Distributions.md)
  [`NonParametric()`](https://epiforecasts.io/EpiNow2/dev/reference/Distributions.md)
  [`Fixed()`](https://epiforecasts.io/EpiNow2/dev/reference/Distributions.md)
  [`Dirichlet()`](https://epiforecasts.io/EpiNow2/dev/reference/Distributions.md)
  : Probability distributions

- [`c(`*`<dist_spec>`*`)`](https://epiforecasts.io/EpiNow2/dev/reference/c.dist_spec.md)
  : Combines multiple delay distributions for further processing

- [`collapse(`*`<dist_spec>`*`)`](https://epiforecasts.io/EpiNow2/dev/reference/collapse.md)
  : Collapse nonparametric distributions in a \<dist_spec\>

- [`discretise(`*`<dist_spec>`*`)`](https://epiforecasts.io/EpiNow2/dev/reference/discretise.md)
  [`discretize()`](https://epiforecasts.io/EpiNow2/dev/reference/discretise.md)
  : Discretise a \<dist_spec\>

- [`` `==`( ``*`<dist_spec>`*`)`](https://epiforecasts.io/EpiNow2/dev/reference/equals-.dist_spec.md)
  [`` `!=`( ``*`<dist_spec>`*`)`](https://epiforecasts.io/EpiNow2/dev/reference/equals-.dist_spec.md)
  : Compares two delay distributions

- [`fix_parameters(`*`<dist_spec>`*`)`](https://epiforecasts.io/EpiNow2/dev/reference/fix_parameters.md)
  :

  Fix the parameters of a `<dist_spec>`

- [`get_parameters()`](https://epiforecasts.io/EpiNow2/dev/reference/get_parameters.md)
  : Get parameters from distributions or fitted models

- [`is_constrained(`*`<dist_spec>`*`)`](https://epiforecasts.io/EpiNow2/dev/reference/is_constrained.md)
  : Check if a \<dist_spec\> is constrained, i.e. has a finite maximum
  or nonzero CDF cutoff.

- [`max(`*`<dist_spec>`*`)`](https://epiforecasts.io/EpiNow2/dev/reference/max.dist_spec.md)
  : Returns the maximum of one or more delay distribution

- [`mean(`*`<dist_spec>`*`)`](https://epiforecasts.io/EpiNow2/dev/reference/mean.dist_spec.md)
  : Returns the mean of one or more delay distribution

- [`new_dist_spec()`](https://epiforecasts.io/EpiNow2/dev/reference/new_dist_spec.md)
  :

  Internal function for generating a `dist_spec` given parameters and a
  distribution.

- [`plot(`*`<dist_spec>`*`)`](https://epiforecasts.io/EpiNow2/dev/reference/plot.dist_spec.md)
  : Plot PMF and CDF for a dist_spec object

- [`` `+`( ``*`<dist_spec>`*`)`](https://epiforecasts.io/EpiNow2/dev/reference/plus-.dist_spec.md)
  : Creates a delay distribution as the sum of two other delay
  distributions.

- [`print(`*`<dist_spec>`*`)`](https://epiforecasts.io/EpiNow2/dev/reference/print.dist_spec.md)
  : Prints the parameters of one or more delay distributions

- [`bound_dist()`](https://epiforecasts.io/EpiNow2/dev/reference/bound_dist.md)
  :

  Define bounds of a `<dist_spec>`

- [`get_pmf()`](https://epiforecasts.io/EpiNow2/dev/reference/get_pmf.md)
  : Get the probability mass function of a nonparametric distribution

- [`get_distribution()`](https://epiforecasts.io/EpiNow2/dev/reference/get_distribution.md)
  :

  Get the distribution of a `<dist_spec>`

## Fit Delay Distributions

Functions to fit delay distributions

- [`bootstrapped_dist_fit()`](https://epiforecasts.io/EpiNow2/dev/reference/bootstrapped_dist_fit.md)
  : Fit a Subsampled Bootstrap to Integer Values and Summarise
  Distribution Parameters
- [`dist_fit()`](https://epiforecasts.io/EpiNow2/dev/reference/dist_fit.md)
  : Fit an Integer Adjusted Exponential, Gamma or Lognormal
  distributions

## Simulation

Functions to help with simulating data or mapping to reported cases

- [`simulate_infections()`](https://epiforecasts.io/EpiNow2/dev/reference/simulate_infections.md)
  : Simulate infections using the renewal equation
- [`simulate_secondary()`](https://epiforecasts.io/EpiNow2/dev/reference/simulate_secondary.md)
  : Simulate secondary observations from primary observations
- [`convolve_and_scale()`](https://epiforecasts.io/EpiNow2/dev/reference/convolve_and_scale.md)
  : Convolve and scale a time series

## Data

Package datasets that may be used to parameterise other functions or in
examples

- [`example_generation_time`](https://epiforecasts.io/EpiNow2/dev/reference/example_generation_time.md)
  : Example generation time
- [`example_incubation_period`](https://epiforecasts.io/EpiNow2/dev/reference/example_incubation_period.md)
  : Example incubation period
- [`example_reporting_delay`](https://epiforecasts.io/EpiNow2/dev/reference/example_reporting_delay.md)
  : Example reporting delay
- [`example_confirmed`](https://epiforecasts.io/EpiNow2/dev/reference/example_confirmed.md)
  : Example Confirmed Case Data Set
- [`example_truncated`](https://epiforecasts.io/EpiNow2/dev/reference/example_truncated.md)
  : Example Case Data Set with Truncation

## Data Access

Functions for extracting data from objects or getting data from sources

- [`get_distribution()`](https://epiforecasts.io/EpiNow2/dev/reference/get_distribution.md)
  :

  Get the distribution of a `<dist_spec>`

- [`get_parameters()`](https://epiforecasts.io/EpiNow2/dev/reference/get_parameters.md)
  : Get parameters from distributions or fitted models

- [`get_pmf()`](https://epiforecasts.io/EpiNow2/dev/reference/get_pmf.md)
  : Get the probability mass function of a nonparametric distribution

- [`get_predictions()`](https://epiforecasts.io/EpiNow2/dev/reference/get_predictions.md)
  : Get predictions from a fitted model

- [`get_regional_results()`](https://epiforecasts.io/EpiNow2/dev/reference/get_regional_results.md)
  : Get Combined Regional Results

- [`get_samples()`](https://epiforecasts.io/EpiNow2/dev/reference/get_samples.md)
  : Get posterior samples from a fitted model

- [`extract_CrIs()`](https://epiforecasts.io/EpiNow2/dev/reference/extract_CrIs.md)
  : Extract Credible Intervals Present

- [`extract_inits()`](https://epiforecasts.io/EpiNow2/dev/reference/extract_inits.md)
  **\[experimental\]** : Generate initial conditions from a Stan fit

- [`extract_samples()`](https://epiforecasts.io/EpiNow2/dev/reference/extract_samples.md)
  : Extract all samples from a stan fit

- [`extract_stan_param()`](https://epiforecasts.io/EpiNow2/dev/reference/extract_stan_param.md)
  : Extract a parameter summary from a Stan object

## Data Cleaning

Functions for cleaning data

- [`clean_nowcasts()`](https://epiforecasts.io/EpiNow2/dev/reference/clean_nowcasts.md)
  : Clean Nowcasts for a Supplied Date
- [`clean_regions()`](https://epiforecasts.io/EpiNow2/dev/reference/clean_regions.md)
  : Clean Regions

## Setup

Functions used for setting up functionality

- [`setup_default_logging()`](https://epiforecasts.io/EpiNow2/dev/reference/setup_default_logging.md)
  : Setup Default Logging
- [`setup_future()`](https://epiforecasts.io/EpiNow2/dev/reference/setup_future.md)
  : Set up Future Backend
- [`setup_logging()`](https://epiforecasts.io/EpiNow2/dev/reference/setup_logging.md)
  : Setup Logging

## Utilities

Utility functions

- [`run_region()`](https://epiforecasts.io/EpiNow2/dev/reference/run_region.md)
  : Run epinow with Regional Processing Code
- [`expose_stan_fns()`](https://epiforecasts.io/EpiNow2/dev/reference/expose_stan_fns.md)
  : Expose internal package stan functions in R
- [`convert_to_logmean()`](https://epiforecasts.io/EpiNow2/dev/reference/convert_to_logmean.md)
  : Convert mean and sd to log mean for a log normal distribution
- [`convert_to_logsd()`](https://epiforecasts.io/EpiNow2/dev/reference/convert_to_logsd.md)
  : Convert mean and sd to log standard deviation for a log normal
  distribution
- [`growth_to_R()`](https://epiforecasts.io/EpiNow2/dev/reference/growth_to_R.md)
  **\[superseded\]** : Convert Growth Rates to Reproduction numbers.
- [`R_to_growth()`](https://epiforecasts.io/EpiNow2/dev/reference/R_to_growth.md)
  **\[superseded\]** : Convert Reproduction Numbers to Growth Rates
- [`update_secondary_args()`](https://epiforecasts.io/EpiNow2/dev/reference/update_secondary_args.md)
  : Update estimate_secondary default priors

## Internal

- [`EpiNow2`](https://epiforecasts.io/EpiNow2/dev/reference/EpiNow2-package.md)
  [`EpiNow2-package`](https://epiforecasts.io/EpiNow2/dev/reference/EpiNow2-package.md)
  : EpiNow2: Estimate and Forecast Real-Time Infection Dynamics

- [`add_day_of_week()`](https://epiforecasts.io/EpiNow2/dev/reference/add_day_of_week.md)
  : Adds a day of the week vector

- [`add_horizon()`](https://epiforecasts.io/EpiNow2/dev/reference/add_horizon.md)
  : Add missing values for future dates

- [`allocate_delays()`](https://epiforecasts.io/EpiNow2/dev/reference/allocate_delays.md)
  : Allocate Delays into Required Stan Format

- [`allocate_empty()`](https://epiforecasts.io/EpiNow2/dev/reference/allocate_empty.md)
  : Allocate Empty Parameters to a List

- [`apply_default_cdf_cutoff()`](https://epiforecasts.io/EpiNow2/dev/reference/apply_default_cdf_cutoff.md)
  : Apply default CDF cutoff to a \<dist_spec\> if it is unconstrained

- [`build_np_est_data()`](https://epiforecasts.io/EpiNow2/dev/reference/build_np_est_data.md)
  : Build Stan data for estimated nonparametric delays

- [`` `$`( ``*`<epinow>`*`)`](https://epiforecasts.io/EpiNow2/dev/reference/cash-.epinow.md)
  **\[deprecated\]** : Extract elements from epinow objects with
  deprecation errors

- [`` `$`( ``*`<estimate_infections>`*`)`](https://epiforecasts.io/EpiNow2/dev/reference/cash-.estimate_infections.md)
  **\[deprecated\]** : Extract elements from estimate_infections objects
  with deprecation errors

- [`` `$`( ``*`<estimate_secondary>`*`)`](https://epiforecasts.io/EpiNow2/dev/reference/cash-.estimate_secondary.md)
  **\[deprecated\]** : Extract elements from estimate_secondary objects
  with deprecation errors

- [`check_generation_time()`](https://epiforecasts.io/EpiNow2/dev/reference/check_generation_time.md)
  : Validate probability distribution for using as generation time

- [`check_reports_valid()`](https://epiforecasts.io/EpiNow2/dev/reference/check_reports_valid.md)
  : Validate data input

- [`check_simulation_input()`](https://epiforecasts.io/EpiNow2/dev/reference/check_simulation_input.md)
  : Validate simulation input data frame

- [`check_sparse_pmf_tail()`](https://epiforecasts.io/EpiNow2/dev/reference/check_sparse_pmf_tail.md)
  : Check that PMF tail is not sparse

- [`check_stan_delay()`](https://epiforecasts.io/EpiNow2/dev/reference/check_stan_delay.md)
  : Validate probability distribution for passing to stan

- [`check_truncation_length()`](https://epiforecasts.io/EpiNow2/dev/reference/check_truncation_length.md)
  : Check and warn if truncation distribution is longer than observed
  time

- [`check_truncation_obs_opts()`](https://epiforecasts.io/EpiNow2/dev/reference/check_truncation_obs_opts.md)
  : Check that obs_opts settings unused by estimate_truncation are at
  defaults

- [`combine_tv_and_static_params()`](https://epiforecasts.io/EpiNow2/dev/reference/combine_tv_and_static_params.md)
  : Combine time-varying and static parameters

- [`construct_output()`](https://epiforecasts.io/EpiNow2/dev/reference/construct_output.md)
  : Construct Output

- [`convert_to_natural()`](https://epiforecasts.io/EpiNow2/dev/reference/convert_to_natural.md)
  : Internal function for converting parameters to natural parameters.

- [`copy_results_to_latest()`](https://epiforecasts.io/EpiNow2/dev/reference/copy_results_to_latest.md)
  : Copy Results From Dated Folder to Latest

- [`create_backcalc_data()`](https://epiforecasts.io/EpiNow2/dev/reference/create_backcalc_data.md)
  : Create Back Calculation Data

- [`create_delay_inits()`](https://epiforecasts.io/EpiNow2/dev/reference/create_delay_inits.md)
  : Create initial conditions for delays

- [`create_future_rt()`](https://epiforecasts.io/EpiNow2/dev/reference/create_future_rt.md)
  : Construct the Required Future Rt assumption

- [`create_gp_data()`](https://epiforecasts.io/EpiNow2/dev/reference/create_gp_data.md)
  : Create Gaussian Process Data

- [`create_infection_summary()`](https://epiforecasts.io/EpiNow2/dev/reference/create_infection_summary.md)
  : Create summary output from infection estimation objects

- [`create_initial_conditions()`](https://epiforecasts.io/EpiNow2/dev/reference/create_initial_conditions.md)
  : Create Initial Conditions Generating Function

- [`create_obs_model()`](https://epiforecasts.io/EpiNow2/dev/reference/create_obs_model.md)
  : Create Observation Model Settings

- [`create_rt_data()`](https://epiforecasts.io/EpiNow2/dev/reference/create_rt_data.md)
  : Create Time-varying Reproduction Number Data

- [`create_sampling_log_message()`](https://epiforecasts.io/EpiNow2/dev/reference/create_sampling_log_message.md)
  : Create sampling log message

- [`create_shifted_cases()`](https://epiforecasts.io/EpiNow2/dev/reference/create_shifted_cases.md)
  : Create Delay Shifted Cases

- [`create_stan_args()`](https://epiforecasts.io/EpiNow2/dev/reference/create_stan_args.md)
  : Create a List of Stan Arguments

- [`create_stan_data()`](https://epiforecasts.io/EpiNow2/dev/reference/create_stan_data.md)
  : Create Stan Data Required for estimate_infections

- [`create_stan_delays()`](https://epiforecasts.io/EpiNow2/dev/reference/create_stan_delays.md)
  : Create delay variables for stan

- [`create_stan_params()`](https://epiforecasts.io/EpiNow2/dev/reference/create_stan_params.md)
  : Create parameters for stan

- [`discrete_pmf()`](https://epiforecasts.io/EpiNow2/dev/reference/discrete_pmf.md)
  : Discretised probability mass function

- [`dist_id_to_name()`](https://epiforecasts.io/EpiNow2/dev/reference/dist_id_to_name.md)
  : Get distribution name from primarycensored Stan dist_id

- [`.extract_to_dist_spec()`](https://epiforecasts.io/EpiNow2/dev/reference/dot-extract_to_dist_spec.md)
  : Extract parameters and convert to dist_spec

- [`.get_dist_id()`](https://epiforecasts.io/EpiNow2/dev/reference/dot-get_dist_id.md)
  : Map distribution name to primarycensored ID

- [`.get_param_names()`](https://epiforecasts.io/EpiNow2/dev/reference/dot-get_param_names.md)
  : Map distribution name to parameter names

- [`.prepare_linelist_data()`](https://epiforecasts.io/EpiNow2/dev/reference/dot-prepare_linelist_data.md)
  : Prepare linelist data for delay estimation

- [`epinow2_rstan_model()`](https://epiforecasts.io/EpiNow2/dev/reference/epinow2_rstan_model.md)
  : Load an EpiNow2 rstan model.

- [`epinow2_stan_model()`](https://epiforecasts.io/EpiNow2/dev/reference/epinow2_stan_model.md)
  : Return a stan model object for the appropriate backend

- [`estimates_by_report_date()`](https://epiforecasts.io/EpiNow2/dev/reference/estimates_by_report_date.md)
  : Estimate Cases by Report Date

- [`extract_delay_params()`](https://epiforecasts.io/EpiNow2/dev/reference/extract_delay_params.md)
  : Extract delay distributions from a fitted model

- [`extract_delays()`](https://epiforecasts.io/EpiNow2/dev/reference/extract_delays.md)
  : Extract samples from all delay parameters

- [`extract_latent_state()`](https://epiforecasts.io/EpiNow2/dev/reference/extract_latent_state.md)
  : Extract samples for a latent state from a Stan model

- [`extract_parameters()`](https://epiforecasts.io/EpiNow2/dev/reference/extract_parameters.md)
  : Extract samples from all parameters

- [`extract_params()`](https://epiforecasts.io/EpiNow2/dev/reference/extract_params.md)
  : Extract parameter names

- [`extract_scalar_params()`](https://epiforecasts.io/EpiNow2/dev/reference/extract_scalar_params.md)
  : Extract scalar parameters from a fitted model

- [`extract_single_dist()`](https://epiforecasts.io/EpiNow2/dev/reference/extract_single_dist.md)
  :

  Extract a single element of a composite `<dist_spec>`

- [`fit_model()`](https://epiforecasts.io/EpiNow2/dev/reference/fit_model.md)
  : Fit a model using the chosen backend.

- [`fit_model_approximate()`](https://epiforecasts.io/EpiNow2/dev/reference/fit_model_approximate.md)
  : Fit a Stan Model using an approximate method

- [`fit_model_with_nuts()`](https://epiforecasts.io/EpiNow2/dev/reference/fit_model_with_nuts.md)
  : Fit a Stan Model using the NUTs sampler

- [`format_fit()`](https://epiforecasts.io/EpiNow2/dev/reference/format_fit.md)
  : Format Posterior Samples

- [`format_quantile_predictions()`](https://epiforecasts.io/EpiNow2/dev/reference/format_quantile_predictions.md)
  : Format quantile predictions

- [`format_sample_predictions()`](https://epiforecasts.io/EpiNow2/dev/reference/format_sample_predictions.md)
  : Format sample predictions

- [`format_samples_with_dates()`](https://epiforecasts.io/EpiNow2/dev/reference/format_samples_with_dates.md)
  : Format raw Stan samples with dates and metadata

- [`format_simulation_output()`](https://epiforecasts.io/EpiNow2/dev/reference/format_simulation_output.md)
  : Format Simulation Output from Stan

- [`get_accumulate()`](https://epiforecasts.io/EpiNow2/dev/reference/get_accumulate.md)
  : Get accumulation flags from data

- [`get_element()`](https://epiforecasts.io/EpiNow2/dev/reference/get_element.md)
  :

  Extracts an element of a `<dist_spec>`

- [`get_raw_result()`](https://epiforecasts.io/EpiNow2/dev/reference/get_raw_result.md)
  : Get a Single Raw Result

- [`get_regions()`](https://epiforecasts.io/EpiNow2/dev/reference/get_regions.md)
  : Get Folders with Results

- [`get_regions_with_most_reports()`](https://epiforecasts.io/EpiNow2/dev/reference/get_regions_with_most_reports.md)
  : Get Regions with Most Reported Cases

- [`get_seeding_time()`](https://epiforecasts.io/EpiNow2/dev/reference/get_seeding_time.md)
  : Estimate seeding time from delays and generation time

- [`lapply_func()`](https://epiforecasts.io/EpiNow2/dev/reference/lapply_func.md)
  : Choose a parallel or sequential apply function

- [`lower_bounds()`](https://epiforecasts.io/EpiNow2/dev/reference/lower_bounds.md)
  : Get the lower bounds of the parameters of a distribution

- [`make_init_priors()`](https://epiforecasts.io/EpiNow2/dev/reference/make_init_priors.md)
  : Build the stan-side init-prior data block from a list of priors

- [`make_param()`](https://epiforecasts.io/EpiNow2/dev/reference/make_param.md)
  : Internal function to create a parameter list

- [`match_output_arguments()`](https://epiforecasts.io/EpiNow2/dev/reference/match_output_arguments.md)
  : Match User Supplied Arguments with Supported Options

- [`merge_trunc_pred_obs()`](https://epiforecasts.io/EpiNow2/dev/reference/merge_trunc_pred_obs.md)
  : Merge truncation predictions with observations for display

- [`natural_params()`](https://epiforecasts.io/EpiNow2/dev/reference/natural_params.md)
  : Get the names of the natural parameters of a distribution

- [`ndist()`](https://epiforecasts.io/EpiNow2/dev/reference/ndist.md) :

  Calculate the number of distributions in a `<dist_spec>`

- [`nonparametric_pmf_data()`](https://epiforecasts.io/EpiNow2/dev/reference/nonparametric_pmf_data.md)
  :

  Build PMF data for the nonparametric branch of `plot.dist_spec`

- [`pack_init_prior()`](https://epiforecasts.io/EpiNow2/dev/reference/pack_init_prior.md)
  : Pack a dist_spec into stan-side init-prior fields

- [`pad_reported_cases()`](https://epiforecasts.io/EpiNow2/dev/reference/pad_reported_cases.md)
  : Pads reported cases with daily initial zeros

- [`posterior_to_normal()`](https://epiforecasts.io/EpiNow2/dev/reference/posterior_to_normal.md)
  : Create a Normal distribution from posterior samples

- [`prepare_truncation_obs()`](https://epiforecasts.io/EpiNow2/dev/reference/prepare_truncation_obs.md)
  : Prepare truncation observations for Stan

- [`process_region()`](https://epiforecasts.io/EpiNow2/dev/reference/process_region.md)
  : Process regional estimate

- [`process_regions()`](https://epiforecasts.io/EpiNow2/dev/reference/process_regions.md)
  : Process all Region Estimates

- [`rdirichlet()`](https://epiforecasts.io/EpiNow2/dev/reference/rdirichlet.md)
  : Draw a single sample from a Dirichlet

- [`reconstruct_delay()`](https://epiforecasts.io/EpiNow2/dev/reference/reconstruct_delay.md)
  : Reconstruct a dist_spec from stored stan data and posterior

- [`reconstruct_nonparametric()`](https://epiforecasts.io/EpiNow2/dev/reference/reconstruct_nonparametric.md)
  : Reconstruct a nonparametric delay distribution

- [`reconstruct_parametric()`](https://epiforecasts.io/EpiNow2/dev/reference/reconstruct_parametric.md)
  : Reconstruct a parametric delay distribution

- [`regional_runtimes()`](https://epiforecasts.io/EpiNow2/dev/reference/regional_runtimes.md)
  : Summarise Regional Runtimes

- [`save_estimate_infections()`](https://epiforecasts.io/EpiNow2/dev/reference/save_estimate_infections.md)
  : Save Estimated Infections

- [`save_input()`](https://epiforecasts.io/EpiNow2/dev/reference/save_input.md)
  : Save Observed Data

- [`sd(`*`<dist_spec>`*`)`](https://epiforecasts.io/EpiNow2/dev/reference/sd.md)
  : Returns the standard deviation of one or more delay distribution

- [`select_plots()`](https://epiforecasts.io/EpiNow2/dev/reference/select_plots.md)
  :

  Internal helper function to select plots from those created by
  [`report_plots()`](https://epiforecasts.io/EpiNow2/dev/reference/report_plots.md)

- [`set_dt_single_thread()`](https://epiforecasts.io/EpiNow2/dev/reference/set_dt_single_thread.md)
  : Set to Single Threading

- [`setup_dt()`](https://epiforecasts.io/EpiNow2/dev/reference/setup_dt.md)
  : Convert to Data Table

- [`setup_target_folder()`](https://epiforecasts.io/EpiNow2/dev/reference/setup_target_folder.md)
  : Setup Target Folder for Saving

- [`stable_convolve()`](https://epiforecasts.io/EpiNow2/dev/reference/stable_convolve.md)
  : Numerically stable convolution function for two pmf vectors

- [`` `[[`( ``*`<epinow>`*`)`](https://epiforecasts.io/EpiNow2/dev/reference/sub-sub-.epinow.md)
  **\[deprecated\]** : Extract elements from epinow objects with bracket
  notation

- [`` `[[`( ``*`<estimate_infections>`*`)`](https://epiforecasts.io/EpiNow2/dev/reference/sub-sub-.estimate_infections.md)
  **\[deprecated\]** : Extract elements from estimate_infections objects
  with bracket notation

- [`` `[[`( ``*`<estimate_secondary>`*`)`](https://epiforecasts.io/EpiNow2/dev/reference/sub-sub-.estimate_secondary.md)
  **\[deprecated\]** : Extract elements from estimate_secondary objects
  with bracket notation

- [`summarise_key_measures()`](https://epiforecasts.io/EpiNow2/dev/reference/summarise_key_measures.md)
  : Summarise rt and cases

- [`summarise_results()`](https://epiforecasts.io/EpiNow2/dev/reference/summarise_results.md)
  : Summarise Real-time Results

- [`update_horizon()`](https://epiforecasts.io/EpiNow2/dev/reference/update_horizon.md)
  : Updates Forecast Horizon Based on Input Data and Target
