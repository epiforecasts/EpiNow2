# Copy Results From Dated Folder to Latest

**\[questioning\]** Copies output from the dated folder to a latest
folder. May be undergo changes in later releases.

## Usage

``` r
copy_results_to_latest(target_folder = NULL, latest_folder = NULL)
```

## Arguments

- target_folder:

  Character string specifying where to save results (will create if not
  present).

- latest_folder:

  Character string containing the path to the latest target folder. As
  produced by `setup_target_folder`.

## Value

No return value, called for side effects
