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

# EpiNow2 1.0.0

* Rebased package from [EpiNow](https://epiforecasts.io/EpiNow/)
* Implemented backcalculation, estimation, forecasting, and bootstrapped distribution fitting.
* Added options to estimate the time-varying reproduction number using a Gaussian process (both stationary and non-stationary), combined with optional user supplied breakpoints. Alternatively a static reproduction number can be assumed which when combined with breakpoints becomes piecewise linear.