# Create sampling log message

Internal function that creates a formatted log message describing the
sampling parameters. The message format varies by method, with different
information shown for exact sampling vs approximate methods (VB,
Laplace, Pathfinder). Optionally includes time steps and forecast
horizon if present.

## Usage

``` r
create_sampling_log_message(args, method)
```

## Arguments

- args:

  List of stan arguments containing:

  - object: Stan model object (CmdStanModel or stanmodel)

  - For sampling method: iter_sampling, iter_warmup (cmdstanr) or iter,
    warmup (rstan), chains

  - For vb method: iter, trials, output_samples

  - For laplace method: trials

  - For pathfinder method: trials, draws

  - data: List potentially containing t (time steps) and horizon
    (forecast)

- method:

  Character string indicating the sampling method. One of "sampling"
  (exact MCMC), "vb" (variational Bayes), "laplace" (Laplace
  approximation), or "pathfinder" (pathfinder algorithm).

## Value

A character string containing the formatted log message with a %s
placeholder for the id parameter (to be filled by sprintf or flog.debug)
