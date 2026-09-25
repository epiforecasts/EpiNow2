skip_on_cran()

# Hand-built posterior samples in the format returned by get_samples() for
# estimate_secondary objects: static parameters have no date, time-varying
# ones do.
secondary_samples <- function() {
  dates <- as.Date("2020-01-01") + 0:1
  rbindlist(list(
    data.table(variable = "reporting[1]", sample = 1:5, value = 1:5),
    data.table(variable = "reporting[2]", sample = 1:5, value = 11:15),
    data.table(variable = "fraction_observed", sample = 1:5, value = 0.1 * 1:5),
    data.table(variable = "phi", sample = 1:5, value = 21:25),
    data.table(
      variable = "sim_secondary", sample = rep(1:5, 2),
      date = rep(dates, each = 5), value = 1:10
    )
  ), fill = TRUE)
}

test_that("summary.epinow errors for a failed run", {
  failed <- structure(
    list(error = "Timed out"),
    class = c("epinow", "estimate_infections", "epinowfit", "list")
  )
  expect_error(summary(failed), "failed epinow run")
  expect_error(summary(failed), "Timed out")
})

test_that("summary.epinow errors for the defunct output argument", {
  fit <- example_regional_output()$regional$testland
  expect_error(
    summary(fit, output = "estimates"),
    class = "defunctError"
  )
})

test_that("get_samples.epinow errors for a failed run", {
  failed <- structure(
    list(error = "Timed out"),
    class = c("epinow", "estimate_infections", "epinowfit", "list")
  )
  expect_error(get_samples(failed), "failed epinow run")
})

test_that("print.epinowfit prints the snapshot summary", {
  fit <- readRDS(system.file(
    package = "EpiNow2", "extdata", "example_estimate_infections.rds"
  ))
  expect_output(print(fit), "Effective reproduction no.")
  expect_output(print(fit), "New infections per day")
})

test_that("summary.estimate_secondary returns key static parameters", {
  local_mocked_bindings(get_samples = function(object, ...) {
    secondary_samples()
  })
  obj <- structure(list(), class = c("estimate_secondary", "epinowfit"))

  compact <- summary(obj)
  expect_s3_class(compact, "data.table")
  expect_equal(
    compact$variable, c("fraction_observed", "reporting[1]", "reporting[2]")
  )
  expect_equal(compact$median, c(0.3, 3, 13))
  expect_equal(compact$mean, c(0.3, 3, 13))

  all_params <- summary(obj, type = "parameters")
  # time-varying parameters are excluded
  expect_equal(
    all_params$variable,
    c("fraction_observed", "phi", "reporting[1]", "reporting[2]")
  )

  phi <- summary(obj, type = "parameters", params = "phi", CrIs = 0.5)
  expect_equal(phi$variable, "phi")
  expect_equal(phi$median, 23)
  expect_true(all(c("lower_50", "upper_50") %in% colnames(phi)))
  expect_false("lower_90" %in% colnames(phi))
})

test_that("summary.estimate_secondary errors for an unknown type", {
  obj <- structure(list(), class = c("estimate_secondary", "epinowfit"))
  expect_error(summary(obj, type = "snapshot"), "must be one of")
})

test_that("print.summary.estimate_truncation shows the distribution", {
  x <- data.table(variable = c("meanlog", "sdlog"), median = c(1, 0.5))
  attr(x, "distribution") <- "lognormal"
  attr(x, "max") <- 10
  class(x) <- c("summary.estimate_truncation", class(x))

  expect_output(
    printed <- print(x),
    "Truncation distribution: lognormal \\(max: 10\\)"
  )
  expect_output(print(x), "meanlog")
  expect_identical(printed, x)
})

test_that("print.summary.estimate_dist shows the fit details", {
  x <- data.table(variable = c("meanlog", "sdlog"), median = c(1, 0.5))
  attr(x, "distribution") <- "lognormal"
  attr(x, "max_value") <- 20
  attr(x, "n_obs") <- 100
  attr(x, "n_strata") <- 12
  attr(x, "primary") <- "uniform"
  attr(x, "max_delay") <- 15
  attr(x, "max_obs_time") <- 30
  attr(x, "n_untruncated") <- 0
  class(x) <- c("summary.estimate_dist", class(x))

  output <- capture.output(printed <- print(x))
  expect_identical(printed, x)
  expect_equal(output[1], "Delay distribution: lognormal (max: 20)")
  expect_equal(output[2], "Observations: 100 (12 unique strata)")
  expect_equal(output[3], "Primary event: uniform ")
  expect_equal(output[4], "Max delay: 15 | Max obs time: 30")
  expect_true(any(grepl("sdlog", output, fixed = TRUE)))

  attr(x, "n_untruncated") <- 3
  expect_output(print(x), "\\(3 strata untruncated\\)")
})
