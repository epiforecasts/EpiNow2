# Match User Supplied Arguments with Supported Options

**\[stable\]** Match user supplied arguments with supported options and
return a logical list for internal usage.

## Usage

``` r
match_output_arguments(
  input_args = NULL,
  supported_args = NULL,
  logger = NULL,
  level = "info"
)
```

## Arguments

- input_args:

  A character vector of input arguments (can be partial).

- supported_args:

  A character vector of supported output arguments.

- logger:

  A character vector indicating the logger to target messages at.
  Defaults to no logging.

- level:

  Character string defaulting to "info". Logging level see documentation
  of futile.logger for details. Supported options are "info" and
  "debug".

## Value

A logical vector of named output arguments
