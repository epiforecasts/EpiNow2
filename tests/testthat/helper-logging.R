# Restore the threshold and appender of the named loggers after a test
local_loggers <- function(names, env = parent.frame()) {
  thresholds <- lapply(
    names, function(n) futile.logger::flog.threshold(name = n)
  )
  appenders <- lapply(
    names, function(n) futile.logger::flog.appender(name = n)
  )
  withr::defer(
    for (i in seq_along(names)) {
      futile.logger::flog.threshold(thresholds[[i]], name = names[[i]])
      futile.logger::flog.appender(appenders[[i]], name = names[[i]])
    },
    envir = env
  )
}
