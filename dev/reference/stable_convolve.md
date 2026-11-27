# Numerically stable convolution function for two pmf vectors

Unlike [`stats::convolve()`](https://rdrr.io/r/stats/convolve.html),
this function does not use the FFT algorithm, which can generate
negative numbers when below machine precision.

## Usage

``` r
stable_convolve(a, b)
```

## Arguments

- a:

  Numeric vector, the first sequence.

- b:

  Numeric vector, the second sequence.

## Value

A numeric vector representing the convolution of `a` and `b`.
