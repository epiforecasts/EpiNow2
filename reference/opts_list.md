# Forecast optiong

**\[maturing\]** Define a list of `_opts()` to pass to
[`regional_epinow()`](https://epiforecasts.io/EpiNow2/reference/regional_epinow.md)
`_opts()` accepting arguments. This is useful when different settings
are needed between regions within a single
[`regional_epinow()`](https://epiforecasts.io/EpiNow2/reference/regional_epinow.md)
call. Using `opts_list()` the defaults can be applied to all regions
present with an override passed to regions as necessary (either within
`opts_list()` or externally).

## Usage

``` r
opts_list(opts, reported_cases, ...)
```

## Arguments

- opts:

  An `_opts()` function call such as
  [`rt_opts()`](https://epiforecasts.io/EpiNow2/reference/rt_opts.md).

- reported_cases:

  A data frame containing a `region` variable indicating the target
  regions.

- ...:

  Optional override for region defaults. See the examples for use case.

## Value

A named list of options per region which can be passed to the `_opt`
accepting arguments of `regional_epinow`.

## See also

[`regional_epinow()`](https://epiforecasts.io/EpiNow2/reference/regional_epinow.md)
[`rt_opts()`](https://epiforecasts.io/EpiNow2/reference/rt_opts.md)

## Examples

``` r
# uses example case vector
cases <- example_confirmed[1:40]
cases <- data.table::rbindlist(list(
  data.table::copy(cases)[, region := "testland"],
  cases[, region := "realland"]
))

# default settings
opts_list(rt_opts(), cases)
#> $testland
#> $use_rt
#> [1] TRUE
#> 
#> $rw
#> [1] 0
#> 
#> $use_breakpoints
#> [1] TRUE
#> 
#> $future
#> [1] "latest"
#> 
#> $gp_on
#> [1] "R_t-1"
#> 
#> $pop_period
#> [1] "forecast"
#> 
#> $pop_floor
#> [1] 1
#> 
#> $growth_method
#> [1] "infections"
#> 
#> $pop
#> - fixed value:
#>   0
#> 
#> $prior
#> - lognormal distribution:
#>   meanlog:
#>     -0.35
#>   sdlog:
#>     0.83
#> 
#> attr(,"class")
#> [1] "rt_opts" "list"   
#> 
#> $realland
#> $use_rt
#> [1] TRUE
#> 
#> $rw
#> [1] 0
#> 
#> $use_breakpoints
#> [1] TRUE
#> 
#> $future
#> [1] "latest"
#> 
#> $gp_on
#> [1] "R_t-1"
#> 
#> $pop_period
#> [1] "forecast"
#> 
#> $pop_floor
#> [1] 1
#> 
#> $growth_method
#> [1] "infections"
#> 
#> $pop
#> - fixed value:
#>   0
#> 
#> $prior
#> - lognormal distribution:
#>   meanlog:
#>     -0.35
#>   sdlog:
#>     0.83
#> 
#> attr(,"class")
#> [1] "rt_opts" "list"   
#> 

# add a weekly random walk in realland
opts_list(rt_opts(), cases, realland = rt_opts(rw = 7))
#> $testland
#> $use_rt
#> [1] TRUE
#> 
#> $rw
#> [1] 0
#> 
#> $use_breakpoints
#> [1] TRUE
#> 
#> $future
#> [1] "latest"
#> 
#> $gp_on
#> [1] "R_t-1"
#> 
#> $pop_period
#> [1] "forecast"
#> 
#> $pop_floor
#> [1] 1
#> 
#> $growth_method
#> [1] "infections"
#> 
#> $pop
#> - fixed value:
#>   0
#> 
#> $prior
#> - lognormal distribution:
#>   meanlog:
#>     -0.35
#>   sdlog:
#>     0.83
#> 
#> attr(,"class")
#> [1] "rt_opts" "list"   
#> 
#> $realland
#> $use_rt
#> [1] TRUE
#> 
#> $rw
#> [1] 7
#> 
#> $use_breakpoints
#> [1] TRUE
#> 
#> $future
#> [1] "latest"
#> 
#> $gp_on
#> [1] "R_t-1"
#> 
#> $pop_period
#> [1] "forecast"
#> 
#> $pop_floor
#> [1] 1
#> 
#> $growth_method
#> [1] "infections"
#> 
#> $pop
#> - fixed value:
#>   0
#> 
#> $prior
#> - lognormal distribution:
#>   meanlog:
#>     -0.35
#>   sdlog:
#>     0.83
#> 
#> attr(,"class")
#> [1] "rt_opts" "list"   
#> 

# add a weekly random walk externally
rt <- opts_list(rt_opts(), cases)
rt$realland$rw <- 7
rt
#> $testland
#> $use_rt
#> [1] TRUE
#> 
#> $rw
#> [1] 0
#> 
#> $use_breakpoints
#> [1] TRUE
#> 
#> $future
#> [1] "latest"
#> 
#> $gp_on
#> [1] "R_t-1"
#> 
#> $pop_period
#> [1] "forecast"
#> 
#> $pop_floor
#> [1] 1
#> 
#> $growth_method
#> [1] "infections"
#> 
#> $pop
#> - fixed value:
#>   0
#> 
#> $prior
#> - lognormal distribution:
#>   meanlog:
#>     -0.35
#>   sdlog:
#>     0.83
#> 
#> attr(,"class")
#> [1] "rt_opts" "list"   
#> 
#> $realland
#> $use_rt
#> [1] TRUE
#> 
#> $rw
#> [1] 7
#> 
#> $use_breakpoints
#> [1] TRUE
#> 
#> $future
#> [1] "latest"
#> 
#> $gp_on
#> [1] "R_t-1"
#> 
#> $pop_period
#> [1] "forecast"
#> 
#> $pop_floor
#> [1] 1
#> 
#> $growth_method
#> [1] "infections"
#> 
#> $pop
#> - fixed value:
#>   0
#> 
#> $prior
#> - lognormal distribution:
#>   meanlog:
#>     -0.35
#>   sdlog:
#>     0.83
#> 
#> attr(,"class")
#> [1] "rt_opts" "list"   
#> 
```
