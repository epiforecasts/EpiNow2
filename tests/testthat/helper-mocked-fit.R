#' Get a fitted `<estimate_infections>` object for use in mocks
#'
#' Strips the `<epinow>` class and timing from the shared fixture so that the
#' result looks like the return value of [estimate_infections()]. It is used
#' with [testthat::local_mocked_bindings()] to test the wrappers around
#' [estimate_infections()] without running a new fit.
#'
#' @return An `<estimate_infections>` object fitted to
#'   `example_confirmed[1:30]` with a seven day forecast horizon.
canned_estimate_infections <- function() {
  fit <- unclass(get_test_fixtures()$estimate_infections)
  fit$timing <- NULL
  class(fit) <- c("estimate_infections", "epinowfit", "list")
  fit
}

#' Capture log messages written to a futile.logger logger
#'
#' Sends messages from logger `name` to a character vector for the rest of the
#' calling test and restores the console appender afterwards.
#'
#' @param name Name of the logger to capture.
#' @param env Environment that controls when capturing stops.
#' @return A function that returns the captured log lines.
capture_log <- function(name, env = parent.frame()) {
  lines <- character(0)
  futile.logger::flog.appender(
    function(line) lines <<- c(lines, line),
    name = name
  )
  withr::defer(
    futile.logger::flog.appender(
      futile.logger::appender.console(), name = name
    ),
    envir = env
  )
  function() lines
}
