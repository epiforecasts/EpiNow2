# Reconstruct a dist_spec from stored stan data and posterior

Reconstruct a dist_spec from stored stan data and posterior

## Usage

``` r
reconstruct_delay(object, delay_name)
```

## Arguments

- object:

  A fitted model object containing fit and args

- delay_name:

  The name of the delay (e.g., "generation_time")

## Value

A dist_spec object, or NULL if the delay doesn't exist
