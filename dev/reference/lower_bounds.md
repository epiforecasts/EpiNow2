# Get the lower bounds of the parameters of a distribution

**\[experimental\]** This is used to avoid sampling parameter values
that have no support.

## Usage

``` r
lower_bounds(distribution)
```

## Arguments

- distribution:

  Character; the distribution to use.

## Value

A numeric vector, the lower bounds.

## Examples

``` r
if (FALSE) { # \dontrun{
lower_bounds("lognormal")
} # }
```
