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

test_that("add_breakpoints sets breakpoints correctly for valid dates", {
  data <- data.table::data.table(
    date = as.Date("2020-01-01") + 0:4,
    confirm = 1:5
  )
  result <- add_breakpoints(data, dates = as.Date(c("2020-01-02", "2020-01-04")))
  expect_equal(result$breakpoint, c(0, 1, 0, 1, 0))
})

test_that("add_breakpoints adds breakpoint column if missing", {
  data <- data.table::data.table(
    date = as.Date("2020-01-01") + 0:2,
    confirm = 1:3
  )
  result <- add_breakpoints(data, dates = as.Date("2020-01-02"))
  expect_true("breakpoint" %in% colnames(result))
  expect_equal(result$breakpoint, c(0, 1, 0))
})

test_that("add_breakpoints throws error if date not in data", {
  data <- data.table::data.table(
    date = as.Date("2020-01-01") + 0:2,
    confirm = 1:3
  )
  expect_error(
    add_breakpoints(data, dates = as.Date("2020-01-10")),
    "Breakpoint date.*not found in data"
  )
})

test_that("add_breakpoints works with no dates supplied", {
  data <- data.table::data.table(
    date = as.Date("2020-01-01") + 0:2,
    confirm = 1:3
  )
  result <- add_breakpoints(data)
  expect_equal(result$breakpoint, c(0, 0, 0))
})

test_that("add_breakpoints resets NA breakpoints to 0", {
  data <- data.table::data.table(
    date = as.Date("2020-01-01") + 0:2,
    confirm = 1:3,
    breakpoint = c(NA, 1, NA)
  )
  result <- add_breakpoints(data, dates = as.Date("2020-01-01"))
  expect_equal(result$breakpoint, c(1, 1, 0))
})

test_that("pad_reported_cases pads with NA by default and correct number of rows", {
  data <- data.table::data.table(
    date = as.Date("2020-01-01") + 0:4,
    confirm = 1:5
  )
  padded <- pad_reported_cases(data, 3)
  expect_equal(nrow(padded), nrow(data) + 3)
  expect_true(all(is.na(padded$confirm[1:3])))
  expect_equal(padded$date[1], as.Date("2019-12-29"))
  expect_equal(padded$confirm[4:8], 1:5)
})

test_that("pad_reported_cases pads with custom value", {
  data <- data.table::data.table(
    date = as.Date("2020-01-01") + 0:2,
    confirm = 1:3
  )
  padded <- pad_reported_cases(data, 2, with = 0)
  expect_equal(padded$confirm[1:2], c(0, 0))
  expect_equal(padded$confirm[3:5], 1:3)
})

test_that("pad_reported_cases adds accumulate and breakpoint columns if present", {
  data <- data.table::data.table(
    date = as.Date("2020-01-01") + 0:2,
    confirm = 1:3,
    accumulate = c(TRUE, FALSE, TRUE),
    breakpoint = c(0, 1, 0)
  )
  padded <- pad_reported_cases(data, 2, with = 99)
  expect_equal(padded$accumulate[1:2], c(FALSE, FALSE))
  expect_equal(padded$breakpoint[1:2], c(0, 0))
  expect_equal(padded$confirm[1:2], c(99, 99))
  expect_equal(padded$confirm[3:5], 1:3)
})

test_that("pad_reported_cases does nothing if n = 0", {
  data <- data.table::data.table(
    date = as.Date("2020-01-01") + 0:2,
    confirm = 1:3
  )
  padded <- pad_reported_cases(data, 0)
  expect_equal(padded, data)
})

test_that("fill_missing sets missing dates and observations to zero", {
  data <- data.table::data.table(
    date = as.Date("2020-01-01") + c(0, 1, 3),
    confirm = c(1, NA, 4)
  )
  filled <- fill_missing(
    data,
    missing_dates = "zero", missing_obs = "zero", initial_accumulate = 1
  )
  expect_equal(filled$date, as.Date("2020-01-01") + 0:3)
  expect_equal(filled$confirm, c(1, 0, 0, 4))
  expect_false(any(filled$accumulate))
})

test_that("fill_missing treats missing dates and observations separately", {
  data <- data.table::data.table(
    date = as.Date("2020-01-01") + c(0, 1, 3),
    confirm = c(1, NA, 4)
  )
  filled <- fill_missing(
    data,
    missing_dates = "zero", missing_obs = "accumulate",
    initial_accumulate = 1
  )
  expect_equal(filled$confirm, c(1, NA, 0, 4))
  expect_equal(filled$accumulate, c(FALSE, TRUE, FALSE, FALSE))
})

test_that("filter_leading_zeros removes leading zeros only", {
  data <- data.table::data.table(
    date = as.Date("2020-01-01") + c(3, 0, 1, 2, 4),
    confirm = c(0, 0, 0, 2, 3)
  )
  filtered <- filter_leading_zeros(data)
  expect_equal(filtered$date, as.Date("2020-01-01") + 2:4)
  expect_equal(filtered$confirm, c(2, 0, 3))
})

test_that("filter_leading_zeros uses the obs_column argument", {
  data <- data.table::data.table(
    date = as.Date("2020-01-01") + 0:3,
    confirm = c(1, 1, 1, 1),
    secondary = c(0, 0, 5, 6)
  )
  filtered <- filter_leading_zeros(data, obs_column = "secondary")
  expect_equal(filtered$date, as.Date("2020-01-01") + 2:3)
})

test_that("filter_leading_zeros correctly handles leading missing values", {
  skip("Known bug, see #1585")
  data <- data.table::data.table(
    date = as.Date("2020-01-01") + 0:4,
    confirm = c(NA, 0, 2, 0, 3)
  )
  filtered <- filter_leading_zeros(data)
  expect_equal(filtered$date, as.Date("2020-01-01") + 2:4)
})

test_that("filter_leading_zeros errors for bad 'data' specifications", {
  data <- data.frame(date = as.Date("2020-01-01") + 0:1, cases = 1:2)
  expect_error(filter_leading_zeros(data), "confirm")
  expect_error(filter_leading_zeros(data, by = "region"), "region")
})

test_that("apply_zero_threshold replaces zeros after a high 7-day average", {
  data <- data.table::data.table(
    date = as.Date("2020-01-01") + 0:9,
    confirm = c(rep(10, 8), 0, 0)
  )
  # The average of the 7 days before day 9 is 10, so the zero on day 9 is
  # replaced. Before day 10 it is (6 * 10 + 0) / 7, which is below 9.
  result <- EpiNow2:::apply_zero_threshold(data, threshold = 9)
  expect_equal(result$confirm, c(rep(10, 8), NA, 0))
  expect_named(result, c("date", "confirm"))
})

test_that("apply_zero_threshold keeps zeros without a full 7-day history", {
  data <- data.table::data.table(
    date = as.Date("2020-01-01") + 0:6,
    confirm = c(rep(100, 6), 0)
  )
  result <- EpiNow2:::apply_zero_threshold(data, threshold = 1)
  expect_equal(result$confirm, c(rep(100, 6), 0))
})

test_that("apply_zero_threshold leaves data unchanged with default threshold", {
  data <- data.table::data.table(
    date = as.Date("2020-01-01") + 0:9,
    confirm = c(rep(10, 8), 0, NA)
  )
  result <- EpiNow2:::apply_zero_threshold(data)
  expect_equal(result$confirm, c(rep(10, 8), 0, NA))
})

test_that("apply_zero_threshold uses the obs_column argument", {
  data <- data.table::data.table(
    date = as.Date("2020-01-01") + 0:8,
    confirm = rep(0, 9),
    secondary = c(rep(10, 8), 0)
  )
  result <- EpiNow2:::apply_zero_threshold(
    data, threshold = 5, obs_column = "secondary"
  )
  expect_equal(result$secondary, c(rep(10, 8), NA))
  expect_equal(result$confirm, rep(0, 9))
})
