# Warn on `EpiNow2::` use of a re-exported distribution function

Helper for the deprecated re-exports in this file. When the function was
called via the `EpiNow2::` prefix (the usage being deprecated) it issues
a deprecation warning pointing to distspec. Bare calls resolve here too,
because EpiNow2 sits above distspec on the search path, but are left
silent since they behave identically to distspec and were never the
target.

## Usage

``` r
reexport_deprecate(name)
```

## Arguments

- name:

  Name of the re-exported function, used in the message.

## Value

`NULL`, invisibly; called for the deprecation-warning side effect.
