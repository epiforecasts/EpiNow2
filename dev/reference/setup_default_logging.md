# Setup Default Logging

**\[questioning\]** Sets up default logging. Usage of logging is
currently being explored as the current setup cannot log stan errors or
progress.

## Usage

``` r
setup_default_logging(
  logs = tempdir(check = TRUE),
  mirror_epinow = FALSE,
  target_date = NULL
)
```

## Arguments

- logs:

  Character path indicating the target folder in which to store log
  information. Defaults to the temporary directory if not specified.
  Default logging can be disabled if `logs` is set to NULL. If
  specifying a custom logging setup then the code for
  `setup_default_logging()` and the
  [`setup_logging()`](https://epiforecasts.io/EpiNow2/dev/reference/setup_logging.md)
  function are a sensible place to start.

- mirror_epinow:

  Logical, defaults to FALSE. Should internal logging be returned from
  [`epinow()`](https://epiforecasts.io/EpiNow2/dev/reference/epinow.md)
  to the console.

- target_date:

  Date, defaults to maximum found in the data if not specified.

## Value

No return value, called for side effects

## Examples

``` r
setup_default_logging()
#> Logging threshold set at INFO for the name logger
#> Writing EpiNow2 logs to the console and:
#> /tmp/RtmpR6VQvJ/regional-epinow/latest.log.
#> Logging threshold set at INFO for the name logger
#> Writing EpiNow2.epinow logs to: /tmp/RtmpR6VQvJ/epinow/latest.log.
```
