# Expose internal package stan functions in R

**\[stable\]** his function exposes internal stan functions in R from a
user supplied list of target files. Allows for testing of stan functions
in R and potentially user use in R code.

## Usage

``` r
expose_stan_fns(files, target_dir, ...)
```

## Arguments

- files:

  A character vector indicating the target files.

- target_dir:

  A character string indicating the target directory for the file.

- ...:

  Additional arguments passed to
  [`rstan::expose_stan_functions()`](https://mc-stan.org/rstan/reference/expose_stan_functions.html).

## Value

No return value, called for side effects
