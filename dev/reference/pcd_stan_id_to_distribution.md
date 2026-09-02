# Map a primarycensored Stan distribution ID to a distspec distribution

Maps a `primarycensored` Stan distribution ID back to a `distspec`
distribution name. Builds a reverse lookup from
[`primarycensored::pcd_stan_dist_id()`](https://primarycensored.epinowcast.org/reference/pcd_stan_dist_id.html)
for supported distributions.

## Usage

``` r
pcd_stan_id_to_distribution(dist_id)
```

## Arguments

- dist_id:

  Integer Stan distribution ID from primarycensored.

## Value

A character string distribution name.
