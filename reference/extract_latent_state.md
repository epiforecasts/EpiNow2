# Extract samples for a latent state from a Stan model

**\[stable\]** Extracts a time-varying latent state from a list of stan
output and returns it as a `<data.table>`.

## Usage

``` r
extract_latent_state(param, samples, dates)
```

## Arguments

- param:

  Character string indicating the latent state to extract

- samples:

  Extracted stan model (using
  [`rstan::extract()`](https://mc-stan.org/rstan/reference/stanfit-method-extract.html))

- dates:

  A vector identifying the dimensionality of the latent state to
  extract. Generally this will be a date.

## Value

A `<data.frame>` containing the following columns:

- time:

  Integer index (1..N) corresponding to the position in the supplied
  dates vector. This is the row/time-step index that maps to the date
  column

- date:

  The date corresponding to this time step

- sample:

  Integer sample ID from the posterior

- value:

  Numeric value of the parameter sample
