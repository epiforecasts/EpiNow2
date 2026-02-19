# Get the names of the natural parameters of a distribution

**\[experimental\]** These are the parameters used in the stan models.
All other parameter representations are converted to these using
[`convert_to_natural()`](https://epiforecasts.io/EpiNow2/reference/convert_to_natural.md)
before being passed to the stan models.

## Usage

``` r
natural_params(distribution)
```

## Arguments

- distribution:

  Character; the distribution to use.

## Value

A character vector, the natural parameters.

## Examples

``` r
if (FALSE) { # \dontrun{
natural_params("gamma")
} # }
```
