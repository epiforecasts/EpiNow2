# Categorise the Probability of Change for Rt

**\[stable\]** Categorises a numeric variable into "Increasing" (\<
0.05), "Likely increasing" (\<0.4), "Stable" (\< 0.6), "Likely
decreasing" (\< 0.95), "Decreasing" (\<= 1)

## Usage

``` r
map_prob_change(var)
```

## Arguments

- var:

  Numeric variable to be categorised

## Value

A character variable.

## Examples

``` r
var <- seq(0.01, 1, 0.01)
var
#>   [1] 0.01 0.02 0.03 0.04 0.05 0.06 0.07 0.08 0.09 0.10 0.11 0.12 0.13 0.14 0.15
#>  [16] 0.16 0.17 0.18 0.19 0.20 0.21 0.22 0.23 0.24 0.25 0.26 0.27 0.28 0.29 0.30
#>  [31] 0.31 0.32 0.33 0.34 0.35 0.36 0.37 0.38 0.39 0.40 0.41 0.42 0.43 0.44 0.45
#>  [46] 0.46 0.47 0.48 0.49 0.50 0.51 0.52 0.53 0.54 0.55 0.56 0.57 0.58 0.59 0.60
#>  [61] 0.61 0.62 0.63 0.64 0.65 0.66 0.67 0.68 0.69 0.70 0.71 0.72 0.73 0.74 0.75
#>  [76] 0.76 0.77 0.78 0.79 0.80 0.81 0.82 0.83 0.84 0.85 0.86 0.87 0.88 0.89 0.90
#>  [91] 0.91 0.92 0.93 0.94 0.95 0.96 0.97 0.98 0.99 1.00

map_prob_change(var)
#>   [1] Increasing        Increasing        Increasing        Increasing       
#>   [5] Likely increasing Likely increasing Likely increasing Likely increasing
#>   [9] Likely increasing Likely increasing Likely increasing Likely increasing
#>  [13] Likely increasing Likely increasing Likely increasing Likely increasing
#>  [17] Likely increasing Likely increasing Likely increasing Likely increasing
#>  [21] Likely increasing Likely increasing Likely increasing Likely increasing
#>  [25] Likely increasing Likely increasing Likely increasing Likely increasing
#>  [29] Likely increasing Likely increasing Likely increasing Likely increasing
#>  [33] Likely increasing Likely increasing Likely increasing Likely increasing
#>  [37] Likely increasing Likely increasing Likely increasing Stable           
#>  [41] Stable            Stable            Stable            Stable           
#>  [45] Stable            Stable            Stable            Stable           
#>  [49] Stable            Stable            Stable            Stable           
#>  [53] Stable            Stable            Stable            Stable           
#>  [57] Stable            Stable            Stable            Likely decreasing
#>  [61] Likely decreasing Likely decreasing Likely decreasing Likely decreasing
#>  [65] Likely decreasing Likely decreasing Likely decreasing Likely decreasing
#>  [69] Likely decreasing Likely decreasing Likely decreasing Likely decreasing
#>  [73] Likely decreasing Likely decreasing Likely decreasing Likely decreasing
#>  [77] Likely decreasing Likely decreasing Likely decreasing Likely decreasing
#>  [81] Likely decreasing Likely decreasing Likely decreasing Likely decreasing
#>  [85] Likely decreasing Likely decreasing Likely decreasing Likely decreasing
#>  [89] Likely decreasing Likely decreasing Likely decreasing Likely decreasing
#>  [93] Likely decreasing Likely decreasing Decreasing        Decreasing       
#>  [97] Decreasing        Decreasing        Decreasing        Decreasing       
#> Levels: Increasing Likely increasing Stable Likely decreasing Decreasing
```
