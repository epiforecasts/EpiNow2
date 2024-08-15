test_that("rt_opts returns expected default values", {
  result <- rt_opts()
  
  expect_s3_class(result, "rt_opts")
  expect_equal(result$prior, list(mean = 1, sd = 1))
  expect_true(result$use_rt)
  expect_equal(result$rw, 0)
  expect_true(result$use_breakpoints)
  expect_equal(result$future, "latest")
  expect_equal(result$pop, 0)
  expect_equal(result$gp_on, "R_t-1")
})

test_that("rt_opts handles custom inputs correctly", {
  result <- rt_opts(
    prior = list(mean = 2, sd = 0.5),
    use_rt = FALSE,
    rw = 7,
    use_breakpoints = FALSE,
    future = "project",
    gp_on = "R0",
    pop = 1000000
  )
  
  expect_equal(result$prior, list(mean = 2, sd = 0.5))
  expect_false(result$use_rt)
  expect_equal(result$rw, 7)
  expect_true(result$use_breakpoints)  # Should be TRUE when rw > 0
  expect_equal(result$future, "project")
  expect_equal(result$pop, 1000000)
  expect_equal(result$gp_on, "R0")
})

test_that("rt_opts sets use_breakpoints to TRUE when rw > 0", {
  result <- rt_opts(rw = 3, use_breakpoints = FALSE)
  expect_true(result$use_breakpoints)
})

test_that("rt_opts throws error for invalid prior", {
  expect_error(rt_opts(prior = list(mean = 1)),
               "prior must have both a mean and sd specified")
  expect_error(rt_opts(prior = list(sd = 1)),
               "prior must have both a mean and sd specified")
})

test_that("rt_opts validates gp_on argument", {
  expect_error(rt_opts(gp_on = "invalid"), "must be one")
})

test_that("rt_opts returns object of correct class", {
  result <- rt_opts()
  expect_s3_class(result, "rt_opts")
  expect_true("list" %in% class(result))
})

test_that("rt_opts handles edge cases correctly", {
  result <- rt_opts(rw = 0.1, pop = -1)
  expect_equal(result$rw, 0.1)
  expect_equal(result$pop, -1)
  expect_true(result$use_breakpoints)
})
