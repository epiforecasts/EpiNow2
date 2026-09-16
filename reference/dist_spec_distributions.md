# Get parametric distribution types

**\[experimental\]** Returns the mapping of Stan integer codes to
distribution names. Stan uses 0 for lognormal, 1 for gamma.

## Usage

``` r
dist_spec_distributions()
```

## Value

A character vector of distribution names in Stan order.

## Examples

``` r
if (FALSE) { # \dontrun{
dist_spec_distributions()
dist_spec_distributions()[1]  # "lognormal"
} # }
```
