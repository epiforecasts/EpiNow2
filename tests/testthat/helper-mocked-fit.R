# A fitted <estimate_infections> object built from the shared fixture, for
# use as the return value of a mocked estimate_infections().
canned_estimate_infections <- function() {
  fit <- unclass(get_test_fixtures()$estimate_infections)
  fit$timing <- NULL
  class(fit) <- c("estimate_infections", "epinowfit", "list")
  fit
}

# Mock for estimate_infections() that returns a fixed fit. The fit is built
# outside the mock because regions run inside futures, where the fixture
# cache cannot be updated.
mock_fit <- function() {
  fit <- canned_estimate_infections()
  function(...) fit
}

# Sends messages from logger `name` to a character vector until the calling
# test ends. Returns a function that gives the captured lines.
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
