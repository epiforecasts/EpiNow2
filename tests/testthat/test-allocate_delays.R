# Test allocate_delays and allocate_empty functions
# These are internal functions so we use ::: accessor

test_that("allocate_delays returns array when no_delays > 0", {
  delay_var <- list(c(1.5, 0.5), c(2.0, 0.3))
  result <- EpiNow2:::allocate_delays(delay_var, no_delays = 2)

  expect_true(is.array(result))
  expect_equal(as.vector(result), c(1.5, 0.5, 2.0, 0.3))
})

test_that("allocate_delays returns empty array when no_delays = 0", {
  delay_var <- list(c(1.5, 0.5))
  result <- EpiNow2:::allocate_delays(delay_var, no_delays = 0)

  expect_true(is.array(result))
  expect_length(result, 0)
})

test_that("allocate_delays handles single delay", {
  delay_var <- list(c(3.0, 1.0))
  result <- EpiNow2:::allocate_delays(delay_var, no_delays = 1)

  expect_true(is.array(result))
  expect_equal(as.vector(result), c(3.0, 1.0))
})

test_that("allocate_empty adds missing parameters with empty arrays", {
  data <- list(existing_param = c(1, 2, 3))
  params <- c("new_param1", "new_param2")

  result <- EpiNow2:::allocate_empty(data, params, n = 5)

  # Original parameter should be unchanged
  expect_equal(result$existing_param, c(1, 2, 3))

  # New parameters should be empty 2D arrays
  expect_true("new_param1" %in% names(result))
  expect_true("new_param2" %in% names(result))
  expect_equal(dim(result$new_param1), c(5, 0))
  expect_equal(dim(result$new_param2), c(5, 0))
})

test_that("allocate_empty does not overwrite existing parameters", {
  data <- list(param1 = c(1, 2, 3))
  params <- c("param1", "param2")

  result <- EpiNow2:::allocate_empty(data, params, n = 5)

  # param1 should not be overwritten
  expect_equal(result$param1, c(1, 2, 3))

  # param2 should be added
  expect_equal(dim(result$param2), c(5, 0))
})

test_that("allocate_empty handles empty params vector", {
  data <- list(a = 1, b = 2)
  result <- EpiNow2:::allocate_empty(data, character(0), n = 5)

  # Should return unchanged data
  expect_equal(result, data)
})

test_that("allocate_empty handles n = 0", {
  data <- list()
  params <- c("param1")

  result <- EpiNow2:::allocate_empty(data, params, n = 0)

  expect_equal(dim(result$param1), c(0, 0))
})
