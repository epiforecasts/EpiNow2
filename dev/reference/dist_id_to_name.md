# Get distribution name from primarycensored Stan dist_id

Maps a primarycensored Stan distribution ID back to an EpiNow2
distribution name. Builds a reverse lookup from
[`primarycensored::pcd_stan_dist_id()`](https://primarycensored.epinowcast.org/reference/pcd_stan_dist_id.html)
for supported distributions.

## Usage

``` r
dist_id_to_name(dist_id)
```

## Arguments

- dist_id:

  Integer Stan distribution ID from primarycensored.

## Value

A character string distribution name.
