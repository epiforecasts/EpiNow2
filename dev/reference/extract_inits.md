# Generate initial conditions from a Stan fit

**\[experimental\]** Extracts posterior samples to use to initialise a
full model fit. This may be useful for certain data sets where the
sampler gets stuck or cannot easily be initialised. In
[`estimate_infections()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_infections.md),
[`epinow()`](https://epiforecasts.io/EpiNow2/dev/reference/epinow.md)
and
[`regional_epinow()`](https://epiforecasts.io/EpiNow2/dev/reference/regional_epinow.md)
this option can be engaged by setting `stan_opts(init_fit = <stanfit>)`.

This implementation is based on the approach taken in
[epidemia](https://github.com/ImperialCollegeLondon/epidemia/) authored
by James Scott.

## Usage

``` r
extract_inits(fit, current_inits, exclude_list = NULL, samples = 50)
```

## Arguments

- fit:

  A `<stanfit>` object.

- current_inits:

  A function that returns a list of initial conditions (such as
  [`create_initial_conditions()`](https://epiforecasts.io/EpiNow2/dev/reference/create_initial_conditions.md)).
  Only used in `exclude_list` is specified.

- exclude_list:

  A character vector of parameters to not initialise from the fit
  object, defaulting to `NULL`.

- samples:

  Numeric, defaults to 50. Number of posterior samples.

## Value

A function that when called returns a set of initial conditions as a
named list.
