# Build PMF data for the nonparametric branch of `plot.dist_spec`

For a fixed nonparametric delay returns a single row per bin (the stored
PMF). For a Dirichlet-backed estimated delay, draws `samples` PMFs from
the alpha vector via
[`rdirichlet()`](https://epiforecasts.io/EpiNow2/dev/reference/rdirichlet.md)
and returns one row per sample-bin pair so the calling plot can render
an uncertainty band.

## Usage

``` r
nonparametric_pmf_data(x, i, samples)
```

## Arguments

- x:

  The `<dist_spec>` being plotted.

- i:

  Index of the nonparametric component within `x`.

- samples:

  Number of PMFs to draw when alpha is present.

## Value

A `data.table` with columns `sample`, `x`, `p`, `distribution`.
