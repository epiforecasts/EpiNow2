reported_cases <- data.frame(region = c("test", "boo"))
futile.logger::flog.threshold("FATAL")

test_that("setup_future runs without error", {
  no_cores <- suppressWarnings(setup_future(reported_cases))
  expect_type(no_cores, "double")
})

test_that("setup_future runs when only using a single level of parallisation", {
  expect_null(suppressWarnings(setup_future(reported_cases, strategies = "multicore")))
})

test_that("setup_future runs with an error when strategies are incorrectly defined", {
  expect_error(suppressWarnings(setup_future(reported_cases,
    strategies = c("nothing", "problems")
  )))
  expect_error(suppressWarnings(setup_future(reported_cases,
    strategies = c(
      "multicore", "multicore",
      "multicore"
    )
  )))
})


test_that("setup_future runs with an error when reported_cases does not contain a region
          variable", {
  reported_cases <- data.frame(x = 1:10)
  expect_error(suppressWarnings(setup_future(reported_cases)))
})
