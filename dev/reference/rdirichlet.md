# Draw a single sample from a Dirichlet

Base R does not provide an `rdirichlet()`. We use the
gamma-normalisation method also used by the Stan model: draw an
independent `Gamma(alpha_i, 1)` per bin and rescale by the segment sum.
Bins with `alpha == 0` stay at zero so structural zeros (e.g. the t = 0
generation-time bin) are preserved.

## Usage

``` r
rdirichlet(alpha)
```

## Arguments

- alpha:

  A non-negative numeric vector of concentration parameters.

## Value

A numeric vector the same length as `alpha`, summing to 1 over the
positive-alpha entries.

## References

Stan discourse, "Ragged array of simplexes",
<https://discourse.mc-stan.org/t/ragged-array-of-simplexes/1382/21>.
