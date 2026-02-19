# Convert Growth Rates to Reproduction numbers.

**\[questioning\]** See
[here](https://www.medrxiv.org/content/10.1101/2020.01.30.20019877v3.full.pdf)
\# nolint for justification. Now handled internally by stan so may be
removed in future updates if no user demand.

## Usage

``` r
growth_to_R(r, gamma_mean, gamma_sd)
```

## Arguments

- r:

  Numeric, rate of growth estimates.

- gamma_mean:

  Numeric, mean of the gamma distribution

- gamma_sd:

  Numeric, standard deviation of the gamma distribution .

## Value

Numeric vector of reproduction number estimates

## Examples

``` r
growth_to_R(0.2, 4, 1)
#> [1] 2.182875
```
