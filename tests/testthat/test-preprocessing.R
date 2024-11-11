cases <- data.table::copy(example_confirmed)
cases[, confirm := frollsum(confirm, 7)]
cases <- cases[seq(7, nrow(cases), 7)]
cases[2, confirm := NA]

test_that("fill_missing works with NA and missing cases", {
  filled <- fill_missing(cases, missing = "accumulate", initial_accumulate = 7)
  expect_equal(nrow(filled), nrow(cases) * 7)
  expect_true(all(filled[!is.na(confirm), accumulate] == FALSE))
  expect_equal(filled[1:7, accumulate], c(rep(TRUE, 6), FALSE))
})

test_that("fill_missing warns about initial data points", {
  expect_warning(
    fill_missing(cases, missing = "accumulate"),
    "Initial data point not marked as accumulated"
  )
})
