# Convert Reproduction Numbers to Growth Rates

**\[questioning\]** See
[here](https://www.medrxiv.org/content/10.1101/2020.01.30.20019877v3.full.pdf)
\# nolint for justification. Now handled internally by stan so may be
removed in future updates if no user demand.

## Usage

``` r
R_to_growth(R, gamma_mean, gamma_sd)
```

## Arguments

- R:

  Numeric, Reproduction number estimates

- gamma_mean:

  Numeric, mean of the gamma distribution

- gamma_sd:

  Numeric, standard deviation of the gamma distribution .

## Value

Numeric vector of reproduction number estimates

## Examples

``` r
R_to_growth(2.18, 4, 1)
#> [1] 0.1996541
```
