# Test stan_vb_opts function

test_that("stan_vb_opts returns default settings", {
  result <- stan_vb_opts()

  expect_true(is.list(result))
  expect_equal(result$trials, 10)
  expect_equal(result$iter, 10000)
  expect_equal(result$output_samples, 2000)
})

test_that("stan_vb_opts accepts custom samples", {
  result <- stan_vb_opts(samples = 1000)

  expect_equal(result$output_samples, 1000)
})

test_that("stan_vb_opts accepts custom trials and iter", {
  result <- stan_vb_opts(trials = 5, iter = 5000)

  expect_equal(result$trials, 5)
  expect_equal(result$iter, 5000)
})

test_that("stan_vb_opts passes through additional arguments", {
  result <- stan_vb_opts(tol_rel_obj = 0.001)

  expect_equal(result$tol_rel_obj, 0.001)
})
