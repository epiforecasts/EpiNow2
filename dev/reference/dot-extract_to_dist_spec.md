# Extract parameters and convert to dist_spec

Extract parameters and convert to dist_spec

## Usage

``` r
.extract_to_dist_spec(fit, dist, max_value)
```

## Arguments

- fit:

  A fitted Stan model object.

- dist:

  Character, the distribution name (one of `"lognormal"`, `"gamma"`,
  `"normal"`, `"exp"`, or `"weibull"`).

- max_value:

  Numeric, the maximum delay value to retain on the returned
  `<dist_spec>`.

## Value

A `<dist_spec>` with posterior mean/sd parameters.
