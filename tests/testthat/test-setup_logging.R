# Save and restore the state of the loggers a test changes
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

# The console appender writes to stderr
console_log <- function(code) {
  capture.output(code, type = "message")
}

test_that("setup_logging writes logs above the threshold to a file", {
  local_loggers("EpiNow2.test")
  log_file <- withr::local_tempfile(fileext = ".log")
  messages <- capture_messages(
    setup_logging("WARN", file = log_file, name = "EpiNow2.test")
  )
  expect_match(messages, "Logging threshold set at WARN", all = FALSE)
  expect_match(messages, "logs to:", all = FALSE)
  futile.logger::flog.info("not logged", name = "EpiNow2.test")
  futile.logger::flog.warn("logged warning", name = "EpiNow2.test")
  log <- readLines(log_file)
  expect_length(log, 1)
  expect_match(log, "logged warning")
})

test_that("setup_logging can mirror file logs to the console", {
  local_loggers("EpiNow2.test")
  log_file <- withr::local_tempfile(fileext = ".log")
  messages <- capture_messages(setup_logging(
    "INFO", file = log_file, mirror_to_console = TRUE, name = "EpiNow2.test"
  ))
  expect_match(messages, "console and", all = FALSE)
  expect_match(
    console_log(futile.logger::flog.info("mirrored", name = "EpiNow2.test")),
    "mirrored"
  )
  expect_match(readLines(log_file), "mirrored")
})

test_that("setup_logging logs to the console without a file", {
  local_loggers("EpiNow2.test")
  messages <- capture_messages(setup_logging("INFO", name = "EpiNow2.test"))
  expect_match(messages, "logs to the console", all = FALSE)
  expect_match(
    console_log(futile.logger::flog.info("to console", name = "EpiNow2.test")),
    "to console"
  )
})

test_that("setup_logging sets the root logger when name is NULL", {
  local_loggers("ROOT")
  suppressMessages(setup_logging("ERROR", name = NULL))
  expect_equal(futile.logger::flog.threshold(name = "ROOT"), "ERROR")
})

test_that("setup_default_logging writes dated logs to the target folder", {
  local_loggers(c("EpiNow2", "EpiNow2.epinow"))
  logs <- withr::local_tempdir()
  suppressMessages(
    setup_default_logging(logs = logs, target_date = "2020-01-01")
  )
  expect_true(dir.exists(file.path(logs, "regional-epinow")))
  expect_true(dir.exists(file.path(logs, "epinow")))

  futile.logger::flog.info("epinow message", name = "EpiNow2.epinow")
  expect_match(
    readLines(file.path(logs, "epinow", "2020-01-01.log")), "epinow message"
  )
  expect_match(
    console_log(futile.logger::flog.info("regional message", name = "EpiNow2")),
    "regional message"
  )
  expect_match(
    readLines(file.path(logs, "regional-epinow", "2020-01-01.log")),
    "regional message"
  )
})

test_that("setup_default_logging names logs 'latest' without a target date", {
  local_loggers(c("EpiNow2", "EpiNow2.epinow"))
  logs <- withr::local_tempdir()
  suppressMessages(setup_default_logging(logs = logs))
  futile.logger::flog.info("latest message", name = "EpiNow2.epinow")
  expect_match(
    readLines(file.path(logs, "epinow", "latest.log")), "latest message"
  )
})

test_that("setup_default_logging suppresses logging when logs is NULL", {
  local_loggers(c("ROOT", "EpiNow2.epinow"))
  setup_default_logging(logs = NULL)
  expect_equal(futile.logger::flog.threshold(name = "ROOT"), "FATAL")
  expect_length(
    console_log(futile.logger::flog.error("hidden", name = "EpiNow2.epinow")),
    0
  )
})

test_that("setup_target_folder creates a dated folder and returns paths", {
  root <- withr::local_tempdir()
  folders <- setup_target_folder(root, target_date = "2020-01-01")
  expect_equal(
    folders,
    list(
      date = file.path(root, "2020-01-01"),
      latest = file.path(root, "latest")
    )
  )
  expect_true(dir.exists(folders$date))
  expect_false(dir.exists(folders$latest))
  expect_null(setup_target_folder(NULL, target_date = "2020-01-01"))
})
