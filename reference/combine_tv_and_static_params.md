# Combine time-varying and static parameters

Internal helper that combines time-varying parameters (which get their
variable name from list keys) with static parameters (which already have
a variable column from extractors).

## Usage

``` r
combine_tv_and_static_params(time_varying_list, raw_samples, args)
```

## Arguments

- time_varying_list:

  Named list of time-varying parameter data.tables

- raw_samples:

  Raw samples from extract_samples()

- args:

  Model arguments containing delay and parameter specifications

## Value

A `data.table` combining all parameters with variable column
