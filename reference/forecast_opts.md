# Forecast options

**\[stable\]** Defines a list specifying the arguments passed to
underlying stan backend functions via
[`stan_sampling_opts()`](https://epiforecasts.io/EpiNow2/reference/stan_sampling_opts.md)
and
[`stan_vb_opts()`](https://epiforecasts.io/EpiNow2/reference/stan_vb_opts.md).
Custom settings can be supplied which override the defaults.

## Usage

``` r
forecast_opts(horizon = 7, accumulate)
```

## Arguments

- horizon:

  Numeric, defaults to 7. Number of days into the future to forecast.

- accumulate:

  Integer, the number of days to accumulate in forecasts, if any. If not
  given and observations are accumulated at constant frequency in the
  data used for fitting then the same accumulation will be used in
  forecasts unless set explicitly here.

## Value

A `<forecast_opts>` object of forecast setting.

## See also

[`fill_missing()`](https://epiforecasts.io/EpiNow2/reference/fill_missing.md)

## Examples

``` r
forecast_opts(horizon = 28, accumulate = 7)
#> $horizon
#> [1] 28
#> 
#> $accumulate
#> [1] 7
#> 
#> attr(,"class")
#> [1] "forecast_opts" "list"         
```
