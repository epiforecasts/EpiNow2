test_that("truncate() can perform truncation as expected", {
  reports <- c(10, 20, 30, 40, 50)
  trunc_rev_cmf <- c(1, 0.8, 0.5, 0.2)
  expected <- c(reports[1], reports[2:5] * trunc_rev_cmf)
  expect_equal(truncate(reports, trunc_rev_cmf, FALSE), expected)
})

test_that("truncate() can perform reconstruction as expected", {
  reports <- c(10, 20, 15, 8, 10)
  trunc_rev_cmf <- c(1, 0.8, 0.5, 0.2)
  expected <- c(reports[1], reports[2:5] / trunc_rev_cmf)
  expect_equal(truncate(reports, trunc_rev_cmf, TRUE), expected)
})

test_that("truncate() can handle longer trunc_rev_cmf than reports", {
  reports <- c(10, 20, 30)
  trunc_rev_cmf <- c(1, 0.8, 0.5, 0.2, 0.1)
  expected <- reports * trunc_rev_cmf[3:5]
  expect_equal(truncate(reports, trunc_rev_cmf, FALSE), expected)
})

test_that("truncate() can handle reconstruction with longer trunc_rev_cmf than reports", {
  reports <- c(10, 16, 15)
  trunc_rev_cmf <- c(1, 0.8, 0.5, 0.2, 0.1)
  expected <- reports / trunc_rev_cmf[3:5]
  expect_equal(truncate(reports, trunc_rev_cmf, TRUE), expected)
})
