# EpiNow2 1.2.0

* Added timeout and timing option to `regional_epinow`
* Improved logging of warnings in `regional_epinow`
* Added inner 20% high density region.
* Added mean and sd to all reporting summaries.
* Added a summary of the growth rate and doubling time.
* Added a `samples` argument to `get_regional_results` to make loading in samples optional. This also allows samples to be dropped when
  using `regional_epinow` which reduces RAM usage.
* Cleaned up implementation of `estimate_infections` to move processing code into internal functions
  
  
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
