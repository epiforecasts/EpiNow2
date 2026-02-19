# Filter Options for a Target Region

**\[maturing\]** A helper function that allows the selection of region
specific settings if present and otherwise applies the overarching
settings.

## Usage

``` r
filter_opts(opts, region)
```

## Arguments

- opts:

  Either a list of calls to an `_opts()` function or a single call to an
  `_opts()` function.

- region:

  A character string indicating a region of interest.

## Value

A list of options
