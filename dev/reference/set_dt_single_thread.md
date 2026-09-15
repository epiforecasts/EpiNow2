# Set to Single Threading

This function sets the threads used by `{data.table}` to 1 in the parent
function and then restores the initial `{data.table}` threads when the
function exits. This is primarily used as an internal function inside of
other functions and will generally not be used on its own.

## Usage

``` r
set_dt_single_thread()
```

## Value

an environment in the parent frame named "dt_settings"

## Examples

``` r
# \donttest{
data.table::setDTthreads(2)
test_function <- function() {
  set_dt_single_thread()

  print(data.table::getDTthreads())
}
test_function()
#> [1] 1
data.table::getDTthreads()
#> [1] 2
# }
```
