# Fit an Integer Adjusted Exponential, Gamma or Lognormal distributions

**\[stable\]** Fits an integer adjusted exponential, gamma or lognormal
distribution using stan.

## Usage

``` r
dist_fit(
  values = NULL,
  samples = 1000,
  cores = 1,
  chains = 2,
  dist = "exp",
  verbose = FALSE,
  backend = "rstan"
)
```

## Arguments

- values:

  Numeric vector of values

- samples:

  Numeric, number of samples to take. Must be \>= 1000. Defaults to
  1000.

- cores:

  Numeric, defaults to 1. Number of CPU cores to use (no effect if
  greater than the number of chains).

- chains:

  Numeric, defaults to 2. Number of MCMC chains to use. More is better
  with the minimum being two.

- dist:

  Character string, which distribution to fit. Defaults to exponential
  (`"exp"`) but gamma (`"gamma"`) and lognormal (`"lognormal"`) are also
  supported.

- verbose:

  Logical, defaults to FALSE. Should verbose progress messages be
  printed.

- backend:

  Character string indicating the backend to use for fitting stan
  models. Supported arguments are "rstan" (default) or "cmdstanr".

## Value

A stan fit of an interval censored distribution

## Examples

``` r
# \donttest{
# integer adjusted exponential model
dist_fit(rexp(1:100, 2),
  samples = 1000, dist = "exp",
  cores = ifelse(interactive(), 4, 1), verbose = TRUE
)
#> 
#> SAMPLING FOR MODEL 'dist_fit' NOW (CHAIN 1).
#> Chain 1: Rejecting initial value:
#> Chain 1:   Log probability evaluates to log(0), i.e. negative infinity.
#> Chain 1:   Stan can't start sampling from this initial value.
#> Chain 1: 
#> Chain 1: Gradient evaluation took 6.6e-05 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.66 seconds.
#> Chain 1: Adjust your expectations accordingly!
#> Chain 1: 
#> Chain 1: 
#> Chain 1: Iteration:    1 / 1500 [  0%]  (Warmup)
#> Chain 1: Iteration:   50 / 1500 [  3%]  (Warmup)
#> Chain 1: Iteration:  100 / 1500 [  6%]  (Warmup)
#> Chain 1: Iteration:  150 / 1500 [ 10%]  (Warmup)
#> Chain 1: Iteration:  200 / 1500 [ 13%]  (Warmup)
#> Chain 1: Iteration:  250 / 1500 [ 16%]  (Warmup)
#> Chain 1: Iteration:  300 / 1500 [ 20%]  (Warmup)
#> Chain 1: Iteration:  350 / 1500 [ 23%]  (Warmup)
#> Chain 1: Iteration:  400 / 1500 [ 26%]  (Warmup)
#> Chain 1: Iteration:  450 / 1500 [ 30%]  (Warmup)
#> Chain 1: Iteration:  500 / 1500 [ 33%]  (Warmup)
#> Chain 1: Iteration:  550 / 1500 [ 36%]  (Warmup)
#> Chain 1: Iteration:  600 / 1500 [ 40%]  (Warmup)
#> Chain 1: Iteration:  650 / 1500 [ 43%]  (Warmup)
#> Chain 1: Iteration:  700 / 1500 [ 46%]  (Warmup)
#> Chain 1: Iteration:  750 / 1500 [ 50%]  (Warmup)
#> Chain 1: Iteration:  800 / 1500 [ 53%]  (Warmup)
#> Chain 1: Iteration:  850 / 1500 [ 56%]  (Warmup)
#> Chain 1: Iteration:  900 / 1500 [ 60%]  (Warmup)
#> Chain 1: Iteration:  950 / 1500 [ 63%]  (Warmup)
#> Chain 1: Iteration: 1000 / 1500 [ 66%]  (Warmup)
#> Chain 1: Iteration: 1001 / 1500 [ 66%]  (Sampling)
#> Chain 1: Iteration: 1050 / 1500 [ 70%]  (Sampling)
#> Chain 1: Iteration: 1100 / 1500 [ 73%]  (Sampling)
#> Chain 1: Iteration: 1150 / 1500 [ 76%]  (Sampling)
#> Chain 1: Iteration: 1200 / 1500 [ 80%]  (Sampling)
#> Chain 1: Iteration: 1250 / 1500 [ 83%]  (Sampling)
#> Chain 1: Iteration: 1300 / 1500 [ 86%]  (Sampling)
#> Chain 1: Iteration: 1350 / 1500 [ 90%]  (Sampling)
#> Chain 1: Iteration: 1400 / 1500 [ 93%]  (Sampling)
#> Chain 1: Iteration: 1450 / 1500 [ 96%]  (Sampling)
#> Chain 1: Iteration: 1500 / 1500 [100%]  (Sampling)
#> Chain 1: 
#> Chain 1:  Elapsed Time: 0.14 seconds (Warm-up)
#> Chain 1:                0.084 seconds (Sampling)
#> Chain 1:                0.224 seconds (Total)
#> Chain 1: 
#> 
#> SAMPLING FOR MODEL 'dist_fit' NOW (CHAIN 2).
#> Chain 2: 
#> Chain 2: Gradient evaluation took 3.6e-05 seconds
#> Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0.36 seconds.
#> Chain 2: Adjust your expectations accordingly!
#> Chain 2: 
#> Chain 2: 
#> Chain 2: Iteration:    1 / 1500 [  0%]  (Warmup)
#> Chain 2: Iteration:   50 / 1500 [  3%]  (Warmup)
#> Chain 2: Iteration:  100 / 1500 [  6%]  (Warmup)
#> Chain 2: Iteration:  150 / 1500 [ 10%]  (Warmup)
#> Chain 2: Iteration:  200 / 1500 [ 13%]  (Warmup)
#> Chain 2: Iteration:  250 / 1500 [ 16%]  (Warmup)
#> Chain 2: Iteration:  300 / 1500 [ 20%]  (Warmup)
#> Chain 2: Iteration:  350 / 1500 [ 23%]  (Warmup)
#> Chain 2: Iteration:  400 / 1500 [ 26%]  (Warmup)
#> Chain 2: Iteration:  450 / 1500 [ 30%]  (Warmup)
#> Chain 2: Iteration:  500 / 1500 [ 33%]  (Warmup)
#> Chain 2: Iteration:  550 / 1500 [ 36%]  (Warmup)
#> Chain 2: Iteration:  600 / 1500 [ 40%]  (Warmup)
#> Chain 2: Iteration:  650 / 1500 [ 43%]  (Warmup)
#> Chain 2: Iteration:  700 / 1500 [ 46%]  (Warmup)
#> Chain 2: Iteration:  750 / 1500 [ 50%]  (Warmup)
#> Chain 2: Iteration:  800 / 1500 [ 53%]  (Warmup)
#> Chain 2: Iteration:  850 / 1500 [ 56%]  (Warmup)
#> Chain 2: Iteration:  900 / 1500 [ 60%]  (Warmup)
#> Chain 2: Iteration:  950 / 1500 [ 63%]  (Warmup)
#> Chain 2: Iteration: 1000 / 1500 [ 66%]  (Warmup)
#> Chain 2: Iteration: 1001 / 1500 [ 66%]  (Sampling)
#> Chain 2: Iteration: 1050 / 1500 [ 70%]  (Sampling)
#> Chain 2: Iteration: 1100 / 1500 [ 73%]  (Sampling)
#> Chain 2: Iteration: 1150 / 1500 [ 76%]  (Sampling)
#> Chain 2: Iteration: 1200 / 1500 [ 80%]  (Sampling)
#> Chain 2: Iteration: 1250 / 1500 [ 83%]  (Sampling)
#> Chain 2: Iteration: 1300 / 1500 [ 86%]  (Sampling)
#> Chain 2: Iteration: 1350 / 1500 [ 90%]  (Sampling)
#> Chain 2: Iteration: 1400 / 1500 [ 93%]  (Sampling)
#> Chain 2: Iteration: 1450 / 1500 [ 96%]  (Sampling)
#> Chain 2: Iteration: 1500 / 1500 [100%]  (Sampling)
#> Chain 2: 
#> Chain 2:  Elapsed Time: 0.13 seconds (Warm-up)
#> Chain 2:                0.058 seconds (Sampling)
#> Chain 2:                0.188 seconds (Total)
#> Chain 2: 
#> WARN [2026-03-12 08:37:51] dist_fit (chain: 1): Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess - 
#> WARN [2026-03-12 08:37:51] dist_fit (chain: 2): Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess - 
#> Inference for Stan model: dist_fit.
#> 2 chains, each with iter=1500; warmup=1000; thin=1; 
#> post-warmup draws per chain=500, total post-warmup draws=1000.
#> 
#>             mean se_mean   sd   2.5%    25%    50%    75%  97.5% n_eff Rhat
#> lambda[1]   2.47    0.02 0.40   1.79   2.19   2.43   2.72   3.39   295    1
#> lp__      -17.12    0.05 0.77 -19.31 -17.27 -16.83 -16.64 -16.58   206    1
#> 
#> Samples were drawn using NUTS(diag_e) at Thu Mar 12 08:37:51 2026.
#> For each parameter, n_eff is a crude measure of effective sample size,
#> and Rhat is the potential scale reduction factor on split chains (at 
#> convergence, Rhat=1).


# integer adjusted gamma model
dist_fit(rgamma(1:100, 5, 5),
  samples = 1000, dist = "gamma",
  cores = ifelse(interactive(), 4, 1), verbose = TRUE
)
#> 
#> SAMPLING FOR MODEL 'dist_fit' NOW (CHAIN 1).
#> Chain 1: 
#> Chain 1: Gradient evaluation took 0.000274 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 2.74 seconds.
#> Chain 1: Adjust your expectations accordingly!
#> Chain 1: 
#> Chain 1: 
#> Chain 1: Iteration:    1 / 1500 [  0%]  (Warmup)
#> Chain 1: Iteration:   50 / 1500 [  3%]  (Warmup)
#> Chain 1: Iteration:  100 / 1500 [  6%]  (Warmup)
#> Chain 1: Iteration:  150 / 1500 [ 10%]  (Warmup)
#> Chain 1: Iteration:  200 / 1500 [ 13%]  (Warmup)
#> Chain 1: Iteration:  250 / 1500 [ 16%]  (Warmup)
#> Chain 1: Iteration:  300 / 1500 [ 20%]  (Warmup)
#> Chain 1: Iteration:  350 / 1500 [ 23%]  (Warmup)
#> Chain 1: Iteration:  400 / 1500 [ 26%]  (Warmup)
#> Chain 1: Iteration:  450 / 1500 [ 30%]  (Warmup)
#> Chain 1: Iteration:  500 / 1500 [ 33%]  (Warmup)
#> Chain 1: Iteration:  550 / 1500 [ 36%]  (Warmup)
#> Chain 1: Iteration:  600 / 1500 [ 40%]  (Warmup)
#> Chain 1: Iteration:  650 / 1500 [ 43%]  (Warmup)
#> Chain 1: Iteration:  700 / 1500 [ 46%]  (Warmup)
#> Chain 1: Iteration:  750 / 1500 [ 50%]  (Warmup)
#> Chain 1: Iteration:  800 / 1500 [ 53%]  (Warmup)
#> Chain 1: Iteration:  850 / 1500 [ 56%]  (Warmup)
#> Chain 1: Iteration:  900 / 1500 [ 60%]  (Warmup)
#> Chain 1: Iteration:  950 / 1500 [ 63%]  (Warmup)
#> Chain 1: Iteration: 1000 / 1500 [ 66%]  (Warmup)
#> Chain 1: Iteration: 1001 / 1500 [ 66%]  (Sampling)
#> Chain 1: Iteration: 1050 / 1500 [ 70%]  (Sampling)
#> Chain 1: Iteration: 1100 / 1500 [ 73%]  (Sampling)
#> Chain 1: Iteration: 1150 / 1500 [ 76%]  (Sampling)
#> Chain 1: Iteration: 1200 / 1500 [ 80%]  (Sampling)
#> Chain 1: Iteration: 1250 / 1500 [ 83%]  (Sampling)
#> Chain 1: Iteration: 1300 / 1500 [ 86%]  (Sampling)
#> Chain 1: Iteration: 1350 / 1500 [ 90%]  (Sampling)
#> Chain 1: Iteration: 1400 / 1500 [ 93%]  (Sampling)
#> Chain 1: Iteration: 1450 / 1500 [ 96%]  (Sampling)
#> Chain 1: Iteration: 1500 / 1500 [100%]  (Sampling)
#> Chain 1: 
#> Chain 1:  Elapsed Time: 2.546 seconds (Warm-up)
#> Chain 1:                1.256 seconds (Sampling)
#> Chain 1:                3.802 seconds (Total)
#> Chain 1: 
#> 
#> SAMPLING FOR MODEL 'dist_fit' NOW (CHAIN 2).
#> Chain 2: 
#> Chain 2: Gradient evaluation took 0.000332 seconds
#> Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 3.32 seconds.
#> Chain 2: Adjust your expectations accordingly!
#> Chain 2: 
#> Chain 2: 
#> Chain 2: Iteration:    1 / 1500 [  0%]  (Warmup)
#> Chain 2: Iteration:   50 / 1500 [  3%]  (Warmup)
#> Chain 2: Iteration:  100 / 1500 [  6%]  (Warmup)
#> Chain 2: Iteration:  150 / 1500 [ 10%]  (Warmup)
#> Chain 2: Iteration:  200 / 1500 [ 13%]  (Warmup)
#> Chain 2: Iteration:  250 / 1500 [ 16%]  (Warmup)
#> Chain 2: Iteration:  300 / 1500 [ 20%]  (Warmup)
#> Chain 2: Iteration:  350 / 1500 [ 23%]  (Warmup)
#> Chain 2: Iteration:  400 / 1500 [ 26%]  (Warmup)
#> Chain 2: Iteration:  450 / 1500 [ 30%]  (Warmup)
#> Chain 2: Iteration:  500 / 1500 [ 33%]  (Warmup)
#> Chain 2: Iteration:  550 / 1500 [ 36%]  (Warmup)
#> Chain 2: Iteration:  600 / 1500 [ 40%]  (Warmup)
#> Chain 2: Iteration:  650 / 1500 [ 43%]  (Warmup)
#> Chain 2: Iteration:  700 / 1500 [ 46%]  (Warmup)
#> Chain 2: Iteration:  750 / 1500 [ 50%]  (Warmup)
#> Chain 2: Iteration:  800 / 1500 [ 53%]  (Warmup)
#> Chain 2: Iteration:  850 / 1500 [ 56%]  (Warmup)
#> Chain 2: Iteration:  900 / 1500 [ 60%]  (Warmup)
#> Chain 2: Iteration:  950 / 1500 [ 63%]  (Warmup)
#> Chain 2: Iteration: 1000 / 1500 [ 66%]  (Warmup)
#> Chain 2: Iteration: 1001 / 1500 [ 66%]  (Sampling)
#> Chain 2: Iteration: 1050 / 1500 [ 70%]  (Sampling)
#> Chain 2: Iteration: 1100 / 1500 [ 73%]  (Sampling)
#> Chain 2: Iteration: 1150 / 1500 [ 76%]  (Sampling)
#> Chain 2: Iteration: 1200 / 1500 [ 80%]  (Sampling)
#> Chain 2: Iteration: 1250 / 1500 [ 83%]  (Sampling)
#> Chain 2: Iteration: 1300 / 1500 [ 86%]  (Sampling)
#> Chain 2: Iteration: 1350 / 1500 [ 90%]  (Sampling)
#> Chain 2: Iteration: 1400 / 1500 [ 93%]  (Sampling)
#> Chain 2: Iteration: 1450 / 1500 [ 96%]  (Sampling)
#> Chain 2: Iteration: 1500 / 1500 [100%]  (Sampling)
#> Chain 2: 
#> Chain 2:  Elapsed Time: 2.532 seconds (Warm-up)
#> Chain 2:                0.992 seconds (Sampling)
#> Chain 2:                3.524 seconds (Total)
#> Chain 2: 
#> WARN [2026-03-12 08:37:59] dist_fit (chain: 1): Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess - 
#> WARN [2026-03-12 08:37:59] dist_fit (chain: 2): Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess - 
#> WARN [2026-03-12 08:37:59] dist_fit (chain: 1): Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess - 
#> WARN [2026-03-12 08:37:59] dist_fit (chain: 2): Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess - 
#> Inference for Stan model: dist_fit.
#> 2 chains, each with iter=1500; warmup=1000; thin=1; 
#> post-warmup draws per chain=500, total post-warmup draws=1000.
#> 
#>                mean se_mean   sd   2.5%    25%    50%    75%  97.5% n_eff Rhat
#> alpha_raw[1]   0.94    0.04 0.56   0.06   0.52   0.86   1.29   2.22   179 1.01
#> beta_raw[1]    0.97    0.04 0.56   0.13   0.57   0.92   1.33   2.19   187 1.02
#> alpha[1]       6.29    0.04 0.56   5.41   5.87   6.22   6.64   7.57   179 1.01
#> beta[1]        6.15    0.04 0.56   5.30   5.75   6.10   6.51   7.36   187 1.02
#> lp__         -14.89    0.15 1.49 -18.80 -15.43 -14.44 -13.83 -13.44    94 1.02
#> 
#> Samples were drawn using NUTS(diag_e) at Thu Mar 12 08:37:59 2026.
#> For each parameter, n_eff is a crude measure of effective sample size,
#> and Rhat is the potential scale reduction factor on split chains (at 
#> convergence, Rhat=1).

# integer adjusted lognormal model
dist_fit(rlnorm(1:100, log(5), 0.2),
  samples = 1000, dist = "lognormal",
  cores = ifelse(interactive(), 4, 1), verbose = TRUE
)
#> 
#> SAMPLING FOR MODEL 'dist_fit' NOW (CHAIN 1).
#> Chain 1: 
#> Chain 1: Gradient evaluation took 5.6e-05 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.56 seconds.
#> Chain 1: Adjust your expectations accordingly!
#> Chain 1: 
#> Chain 1: 
#> Chain 1: Iteration:    1 / 1500 [  0%]  (Warmup)
#> Chain 1: Iteration:   50 / 1500 [  3%]  (Warmup)
#> Chain 1: Iteration:  100 / 1500 [  6%]  (Warmup)
#> Chain 1: Iteration:  150 / 1500 [ 10%]  (Warmup)
#> Chain 1: Iteration:  200 / 1500 [ 13%]  (Warmup)
#> Chain 1: Iteration:  250 / 1500 [ 16%]  (Warmup)
#> Chain 1: Iteration:  300 / 1500 [ 20%]  (Warmup)
#> Chain 1: Iteration:  350 / 1500 [ 23%]  (Warmup)
#> Chain 1: Iteration:  400 / 1500 [ 26%]  (Warmup)
#> Chain 1: Iteration:  450 / 1500 [ 30%]  (Warmup)
#> Chain 1: Iteration:  500 / 1500 [ 33%]  (Warmup)
#> Chain 1: Iteration:  550 / 1500 [ 36%]  (Warmup)
#> Chain 1: Iteration:  600 / 1500 [ 40%]  (Warmup)
#> Chain 1: Iteration:  650 / 1500 [ 43%]  (Warmup)
#> Chain 1: Iteration:  700 / 1500 [ 46%]  (Warmup)
#> Chain 1: Iteration:  750 / 1500 [ 50%]  (Warmup)
#> Chain 1: Iteration:  800 / 1500 [ 53%]  (Warmup)
#> Chain 1: Iteration:  850 / 1500 [ 56%]  (Warmup)
#> Chain 1: Iteration:  900 / 1500 [ 60%]  (Warmup)
#> Chain 1: Iteration:  950 / 1500 [ 63%]  (Warmup)
#> Chain 1: Iteration: 1000 / 1500 [ 66%]  (Warmup)
#> Chain 1: Iteration: 1001 / 1500 [ 66%]  (Sampling)
#> Chain 1: Iteration: 1050 / 1500 [ 70%]  (Sampling)
#> Chain 1: Iteration: 1100 / 1500 [ 73%]  (Sampling)
#> Chain 1: Iteration: 1150 / 1500 [ 76%]  (Sampling)
#> Chain 1: Iteration: 1200 / 1500 [ 80%]  (Sampling)
#> Chain 1: Iteration: 1250 / 1500 [ 83%]  (Sampling)
#> Chain 1: Iteration: 1300 / 1500 [ 86%]  (Sampling)
#> Chain 1: Iteration: 1350 / 1500 [ 90%]  (Sampling)
#> Chain 1: Iteration: 1400 / 1500 [ 93%]  (Sampling)
#> Chain 1: Iteration: 1450 / 1500 [ 96%]  (Sampling)
#> Chain 1: Iteration: 1500 / 1500 [100%]  (Sampling)
#> Chain 1: 
#> Chain 1:  Elapsed Time: 0.31 seconds (Warm-up)
#> Chain 1:                0.166 seconds (Sampling)
#> Chain 1:                0.476 seconds (Total)
#> Chain 1: 
#> 
#> SAMPLING FOR MODEL 'dist_fit' NOW (CHAIN 2).
#> Chain 2: 
#> Chain 2: Gradient evaluation took 5.1e-05 seconds
#> Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0.51 seconds.
#> Chain 2: Adjust your expectations accordingly!
#> Chain 2: 
#> Chain 2: 
#> Chain 2: Iteration:    1 / 1500 [  0%]  (Warmup)
#> Chain 2: Iteration:   50 / 1500 [  3%]  (Warmup)
#> Chain 2: Iteration:  100 / 1500 [  6%]  (Warmup)
#> Chain 2: Iteration:  150 / 1500 [ 10%]  (Warmup)
#> Chain 2: Iteration:  200 / 1500 [ 13%]  (Warmup)
#> Chain 2: Iteration:  250 / 1500 [ 16%]  (Warmup)
#> Chain 2: Iteration:  300 / 1500 [ 20%]  (Warmup)
#> Chain 2: Iteration:  350 / 1500 [ 23%]  (Warmup)
#> Chain 2: Iteration:  400 / 1500 [ 26%]  (Warmup)
#> Chain 2: Iteration:  450 / 1500 [ 30%]  (Warmup)
#> Chain 2: Iteration:  500 / 1500 [ 33%]  (Warmup)
#> Chain 2: Iteration:  550 / 1500 [ 36%]  (Warmup)
#> Chain 2: Iteration:  600 / 1500 [ 40%]  (Warmup)
#> Chain 2: Iteration:  650 / 1500 [ 43%]  (Warmup)
#> Chain 2: Iteration:  700 / 1500 [ 46%]  (Warmup)
#> Chain 2: Iteration:  750 / 1500 [ 50%]  (Warmup)
#> Chain 2: Iteration:  800 / 1500 [ 53%]  (Warmup)
#> Chain 2: Iteration:  850 / 1500 [ 56%]  (Warmup)
#> Chain 2: Iteration:  900 / 1500 [ 60%]  (Warmup)
#> Chain 2: Iteration:  950 / 1500 [ 63%]  (Warmup)
#> Chain 2: Iteration: 1000 / 1500 [ 66%]  (Warmup)
#> Chain 2: Iteration: 1001 / 1500 [ 66%]  (Sampling)
#> Chain 2: Iteration: 1050 / 1500 [ 70%]  (Sampling)
#> Chain 2: Iteration: 1100 / 1500 [ 73%]  (Sampling)
#> Chain 2: Iteration: 1150 / 1500 [ 76%]  (Sampling)
#> Chain 2: Iteration: 1200 / 1500 [ 80%]  (Sampling)
#> Chain 2: Iteration: 1250 / 1500 [ 83%]  (Sampling)
#> Chain 2: Iteration: 1300 / 1500 [ 86%]  (Sampling)
#> Chain 2: Iteration: 1350 / 1500 [ 90%]  (Sampling)
#> Chain 2: Iteration: 1400 / 1500 [ 93%]  (Sampling)
#> Chain 2: Iteration: 1450 / 1500 [ 96%]  (Sampling)
#> Chain 2: Iteration: 1500 / 1500 [100%]  (Sampling)
#> Chain 2: 
#> Chain 2:  Elapsed Time: 0.326 seconds (Warm-up)
#> Chain 2:                0.154 seconds (Sampling)
#> Chain 2:                0.48 seconds (Total)
#> Chain 2: 
#> Inference for Stan model: dist_fit.
#> 2 chains, each with iter=1500; warmup=1000; thin=1; 
#> post-warmup draws per chain=500, total post-warmup draws=1000.
#> 
#>            mean se_mean   sd   2.5%    25%    50%    75%  97.5% n_eff Rhat
#> mu[1]      1.62    0.00 0.02   1.58   1.60   1.62   1.63   1.66   630    1
#> sigma[1]   0.17    0.00 0.02   0.14   0.15   0.16   0.18   0.20   739    1
#> lp__     -74.72    0.04 0.92 -77.17 -75.16 -74.41 -74.03 -73.76   499    1
#> 
#> Samples were drawn using NUTS(diag_e) at Thu Mar 12 08:38:00 2026.
#> For each parameter, n_eff is a crude measure of effective sample size,
#> and Rhat is the potential scale reduction factor on split chains (at 
#> convergence, Rhat=1).
# }
```
