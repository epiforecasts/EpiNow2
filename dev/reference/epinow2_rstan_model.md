# Load an EpiNow2 rstan model.

The models are pre-compiled upon package install and is returned here.

## Usage

``` r
epinow2_rstan_model(model = "estimate_infections")
```

## Arguments

- model:

  A character string indicating the model to use. Needs to be amongst
  the compiled models shipped with "EpiNow2" (see the `stan` directory
  for a list). Defaults to "estimate_infections".

## Value

An `rstan` model.
