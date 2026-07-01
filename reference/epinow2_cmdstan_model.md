# Load and compile an EpiNow2 cmdstanr model

The function has been adapted from a similar function in the epinowcast
package (Copyright holder: epinowcast authors, under MIT License).

## Usage

``` r
epinow2_cmdstan_model(
  model = "estimate_infections",
  dir = system.file("stan", package = "EpiNow2"),
  verbose = FALSE,
  ...
)
```

## Arguments

- model:

  A character string indicating the model to use. Needs to be present in
  `dir` (with extension `.stan`). Defaults to "estimate_infections".

- dir:

  A character string specifying the path to any stan files to include in
  the model. If missing the package default is used.

- verbose:

  Logical, defaults to `TRUE`. Should verbose messages be shown.

- ...:

  Additional arguments passed to
  [`cmdstanr::cmdstan_model()`](https://mc-stan.org/cmdstanr/reference/cmdstan_model.html).

## Value

A `cmdstanr` model.
