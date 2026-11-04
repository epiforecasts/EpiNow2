skip_on_cran()

# Uses shared fixtures from setup.R (regional_epinow run once)

futile.logger::flog.threshold("FATAL")

df_non_zero <- function(df) {
  expect_true(nrow(df) > 0)
}

test_that("regional_runtimes produces expected output when with input", {
  fixtures <- get_test_fixtures()
  out <- fixtures$regional

  runtimes <- regional_runtimes(out$regional)
  expect_equal(names(runtimes), c("region", "time"))
  df_non_zero(runtimes)
  expect_s3_class(runtimes$time, "difftime")
})

test_that("regional_runtimes errors without output or a target folder", {
  expect_error(regional_runtimes(), "target folder")
})

test_that("regional_runtimes saves timings to the target folder", {
  regional_out <- example_regional_output()
  target_folder <- withr::local_tempdir()

  expect_null(
    regional_runtimes(regional_out$regional, target_folder = target_folder)
  )
  saved <- fread(file.path(target_folder, "runtimes.csv"))
  expect_equal(saved$region, names(regional_out$regional))
  expect_equal(
    saved$time,
    as.numeric(regional_out$timings$time),
    tolerance = 1e-6
  )
})

test_that("regional_runtimes reads the runtime saved for each region", {
  skip("Known bug, see #1570")
  regional_out <- example_regional_output()
  target_folder <- withr::local_tempdir()
  write_regional_results(regional_out$regional, target_folder)

  runtimes <- regional_runtimes(
    target_folder = target_folder, return_output = TRUE
  )
  expected <- regional_out$timings[order(region)]
  expect_equal(runtimes$region, expected$region)
  expect_equal(as.numeric(runtimes$time), as.numeric(expected$time))
})
