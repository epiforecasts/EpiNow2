# Clean Nowcasts for a Supplied Date

**\[stable\]** This function removes nowcasts in the format produced by
`EpiNow2` from a target directory for the date supplied.

## Usage

``` r
clean_nowcasts(date = Sys.Date(), nowcast_dir = ".")
```

## Arguments

- date:

  Date object. Defaults to today's date

- nowcast_dir:

  Character string giving the filepath to the nowcast results directory.
  Defaults to the current directory.

## Value

No return value, called for side effects
