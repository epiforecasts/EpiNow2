# Number of time points modelled by the Gaussian process

Mirrors `setup_noise()` in the Stan model.

## Usage

``` r
gp_noise_terms(data)
```

## Arguments

- data:

  A list containing the following numeric values: `t`, `seeding_time`,
  `horizon`, `future_fixed`, `fixed_from`, `stationary` and
  `estimate_r`.

## Value

An integer giving the length of the Gaussian process.
