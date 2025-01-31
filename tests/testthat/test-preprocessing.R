cases <- data.table::copy(example_confirmed)
cases[, confirm := frollsum(confirm, 7)]
cases <- cases[seq(7, nrow(cases), 7)]
cases[2, confirm := NA]

test_that("fill_missing works with NA and missing cases", {
  filled <- fill_missing(
    cases,
    missing_dates = "accumulate", initial_accumulate = 7
  )
  expect_equal(nrow(filled), nrow(cases) * 7)
  expect_true(all(filled[!is.na(confirm), accumulate] == FALSE))
  expect_equal(filled[1:7, accumulate], c(rep(TRUE, 6), FALSE))
})

test_that("fill_missing works with by columns", {
  complete <- CJ(
    date = cases$date,
    country = c("A", "B"),
    obs = c("Cases", "Deaths")
  )
  more_cases <- merge(cases, complete, by = "date")
  filled <- fill_missing(
    more_cases,
    missing_obs = "accumulate", initial_accumulate = 7,
    by = c("country", "obs")
  )
  expect_equal(nrow(filled), nrow(more_cases) * 7)
  expect_true(all(filled[!is.na(confirm), accumulate] == FALSE))
})

test_that("fill_missing warns about initial data points", {
  expect_message(
    fill_missing(cases, missing_dates = "accumulate"),
    "Detected fixed accumulation frequency"
  )
  shifted <- copy(cases)
  shifted[1, date := date + 1]
  expect_warning(
    fill_missing(shifted, missing_dates = "accumulate"),
    "Initial data point not marked as accumulated"
  )
})

test_that("add_horizon works", {
  expect_equal(nrow(add_horizon(cases, horizon = 7L)), nrow(cases) + 7L)
})

test_that("add_horizon identifies gaps correctly", {
  filled <- fill_missing(
    cases,
    missing_dates = "accumulate", initial_accumulate = 7
  )
  expect_message(
    result <- add_horizon(filled, horizon = 7),
    "Forecasts accumulated every 7 days"
  )
  result <- add_horizon(filled, horizon = 7, accumulate = 7)
  expect_true(all(result[seq(.N - 6, .N - 1), accumulate]))
  expect_false(result[.N, accumulate])
})

test_that("add_horizon doesn't try to identify non-equally spaced gaps", {
  reported_irregular <- example_confirmed[c(seq(1, 43, by = 7), 45)]
  filled <- suppressWarnings(
    fill_missing(reported_irregular, missing_dates = "accumulate")
  )
  result <- add_horizon(filled, horizon = 7)
  expect_false(any(result[seq(.N - 6, .N), accumulate]))
})
