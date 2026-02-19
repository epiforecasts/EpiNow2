# Setup Logging

**\[questioning\]** Sets up `{futile.logger}` logging, which is
integrated into `{EpiNow2}`. See the documentation for `{futile.logger}`
for full details. By default `{EpiNow2}` prints all logs at the "INFO"
level and returns them to the console. Usage of logging is currently
being explored as the current setup cannot log stan errors or progress.

## Usage

``` r
setup_logging(
  threshold = "INFO",
  file = NULL,
  mirror_to_console = FALSE,
  name = "EpiNow2"
)
```

## Arguments

- threshold:

  Character string indicating the logging level see (?futile.logger for
  details of the available options). Defaults to "INFO".

- file:

  Character string indicating the path to save logs to. By default logs
  will be written to the console.

- mirror_to_console:

  Logical, defaults to `FALSE`. If saving logs to a file should they
  also be duplicated in the console.

- name:

  Character string defaulting to EpiNow2. This indicates the name of the
  logger to setup. The default logger for EpiNow2 is called EpiNow2.
  Nested options include: Epinow2.epinow which controls all logging for
  [`epinow()`](https://epiforecasts.io/EpiNow2/dev/reference/epinow.md)
  and nested functions, EpiNow2.epinow.estimate_infections (logging in
  [`estimate_infections()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_infections.md),
  and EpiNow2.epinow.estimate_infections.fit (logging in fitting
  functions).

## Value

Nothing
