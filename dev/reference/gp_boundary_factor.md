# Boundary factor used when relating the lengthscale to the basis functions

Boundary factor used when relating the lengthscale to the basis
functions

## Usage

``` r
gp_boundary_factor(L, periodic)
```

## Arguments

- L:

  Numeric boundary factor.

- periodic:

  Logical, whether the kernel is periodic. The periodic kernel has no
  boundary, so a factor of 1 is used.

## Value

A numeric boundary factor.
