# Extract all samples from a stan fit

If the `object` argument is a `<stanfit>` object, it simply returns the
result of
[`rstan::extract()`](https://mc-stan.org/rstan/reference/stanfit-method-extract.html).
If it is a `<CmdStanMCMC>` it returns samples in the same format as
[`rstan::extract()`](https://mc-stan.org/rstan/reference/stanfit-method-extract.html)
does for `<stanfit>` objects.

## Usage

``` r
extract_samples(stan_fit, pars = NULL, include = TRUE)
```

## Arguments

- stan_fit:

  A `<stanfit>` or `<CmdStanMCMC>` object as returned by
  [`fit_model()`](https://epiforecasts.io/EpiNow2/reference/fit_model.md).

- pars:

  Any selection of parameters to extract

- include:

  whether the parameters specified in `pars` should be included (`TRUE`,
  the default) or excluded (`FALSE`)

## Value

List of data.tables with samples
