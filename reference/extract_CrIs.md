# Extract Credible Intervals Present

**\[stable\]** Helper function to extract the credible intervals present
in a `<data.frame>`.

## Usage

``` r
extract_CrIs(summarised)
```

## Arguments

- summarised:

  A `<data.frame>` as processed by `calc_CrIs`

## Value

A numeric vector of credible intervals detected in the `<data.frame>`.

## Examples

``` r
samples <- data.frame(value = 1:10, type = "car")
summarised <- calc_CrIs(samples,
  summarise_by = "type",
  CrIs = c(seq(0.05, 0.95, 0.05))
)
extract_CrIs(summarised)
#>  [1] 95 90 85 80 75 70 65 60 55 50 45 40 35 30 25 20 15 10  5
```
