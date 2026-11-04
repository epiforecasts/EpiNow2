test_that("match_output_arguments works as expected", {
  out <- rep(FALSE, 3)
  names(out) <- c("fit", "plots", "samples")
  expect_equal(
    EpiNow2:::match_output_arguments(supported_args = names(out)),
    out
  )
  out["plots"] <- TRUE
  expect_equal(
    EpiNow2:::match_output_arguments("plots", supported_args = names(out)), out
  )
  out["samples"] <- TRUE
  expect_equal(EpiNow2:::match_output_arguments(c("plots", "samples"),
    supported_args = names(out)
  ), out)
  expect_equal(
    EpiNow2:::match_output_arguments("p", supported_args = names(out)), out
  )
})

test_that("match_output_arguments logs the outputs it found", {
  logger <- "EpiNow2.test.match_output_arguments"
  logged <- character(0)
  old_threshold <- futile.logger::flog.threshold(name = logger)
  old_appender <- futile.logger::flog.appender(name = logger)
  withr::defer({
    futile.logger::flog.threshold(old_threshold, name = logger)
    futile.logger::flog.appender(old_appender, name = logger)
  })
  futile.logger::flog.threshold("INFO", name = logger)
  futile.logger::flog.appender(
    function(line) logged <<- c(logged, line), name = logger
  )
  EpiNow2:::match_output_arguments(
    "plots", supported_args = c("plots", "samples"), logger = logger
  )
  expect_match(logged[1], "Producing following optional outputs: plots")
  EpiNow2:::match_output_arguments(
    supported_args = c("plots", "samples"), logger = logger
  )
  expect_match(logged[2], "No optional output specified")
  expect_length(logged, 2)
})
