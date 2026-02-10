# Fit a Stan Model using the NUTs sampler

**\[maturing\]** Fits a stan model using
[`rstan::sampling()`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html).
Provides the optional ability to run chains using `future` with error
catching, timeouts and merging of completed chains.

## Usage

``` r
fit_model_with_nuts(
  args,
  future = FALSE,
  max_execution_time = Inf,
  id = "stan"
)
```

## Arguments

- args:

  List of stan arguments.

- future:

  Logical, defaults to `FALSE`. Should `future` be used to run stan
  chains in parallel.

- max_execution_time:

  Numeric, defaults to Inf. What is the maximum execution time per chain
  in seconds. Results will still be returned as long as at least 2
  chains complete successfully within the timelimit.

- id:

  A character string used to assign logging information on error. Used
  by
  [`regional_epinow()`](https://epiforecasts.io/EpiNow2/reference/regional_epinow.md)
  to assign errors to regions. Alter the default to run with error
  catching.

## Value

A stan model object
