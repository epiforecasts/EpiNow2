# Compares two delay distributions

Compares two delay distributions

## Usage

``` r
# S3 method for class 'dist_spec'
e1 == e2

# S3 method for class 'dist_spec'
e1 != e2
```

## Arguments

- e1:

  The first delay distribution (of type \<dist_spec\>) to combine.

- e2:

  The second delay distribution (of type \<dist_spec\>) to combine.

## Value

TRUE or FALSE

## Examples

``` r
Fixed(1) == Normal(1, 0.5)
#> [1] FALSE
```
