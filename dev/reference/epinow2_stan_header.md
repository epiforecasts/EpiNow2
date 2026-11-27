# Path to the C++ header used by the EpiNow2 Stan models

Models that include `inst/stan/functions` need this header, which
implements Stan functions declared there without a body.

## Usage

``` r
epinow2_stan_header()
```

## Value

A character string with the path to the header.

## Examples

``` r
epinow2_stan_header()
#> [1] "/home/runner/work/_temp/Library/EpiNow2/include/epinow2.hpp"
```
