# Set up Future Backend

**\[stable\]** A utility function that aims to streamline the set up of
the required future backend with sensible defaults for most users of
[`regional_epinow()`](https://epiforecasts.io/EpiNow2/dev/reference/regional_epinow.md).
More advanced users are recommended to setup their own `{future}`
backend based on their available resources. Running this requires the
`{future}` package to be installed.

## Usage

``` r
setup_future(
  data,
  strategies = c("multisession", "multisession"),
  min_cores_per_worker = 4
)
```

## Arguments

- data:

  A `<data.frame>` of disease reports (confirm) by date (date), and
  region (`region`).

- strategies:

  A vector length 1 to 2 of strategies to pass to
  [`future::plan()`](https://future.futureverse.org/reference/plan.html).
  Nesting of parallelisation is from the top level down. The default is
  to set up nesting parallelisation with both using
  [`future::multisession()`](https://future.futureverse.org/reference/multisession.html)
  ([`future::multicore()`](https://future.futureverse.org/reference/multicore.html)
  will likely be a faster option on supported platforms). For single
  level parallelisation use a single strategy or
  [`future::plan()`](https://future.futureverse.org/reference/plan.html)
  directly. See
  [`future::plan()`](https://future.futureverse.org/reference/plan.html)
  for options.

- min_cores_per_worker:

  Numeric, the minimum number of cores per worker. Defaults to 4 which
  assumes 4 MCMC chains are in use per region.

## Value

Numeric number of cores to use per worker. If greater than 1 pass to
`stan_args = list(cores = "output from setup future")` or use
`future = TRUE`. If only a single strategy is used then nothing is
returned.
