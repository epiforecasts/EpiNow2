# Extract samples from all delay parameters

Extracts samples from all delay parameters using the delay ID lookup
system. Similar to extract_parameters(), this extracts all delay
distribution parameters and uses the \*\_id variables (e.g., delay_id,
trunc_id) to assign meaningful names.

## Usage

``` r
extract_delays(samples)
```

## Arguments

- samples:

  Extracted stan model (using
  [`rstan::extract()`](https://rdrr.io/pkg/rstan/man/stanfit-method-extract.html))

## Value

A `<data.table>` with columns: parameter, sample, value, or NULL if
delay parameters don't exist in the samples
