skip_on_cran()
skip_on_os("windows")

test_that("day_of_week_effect applies day of week effect correctly", {
  reports <- c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)
  day_of_week <- c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1)
  effect <- c(1.0, 1.1, 1.2)
  
  expected <- reports * effect[day_of_week] * 3
  result <- day_of_week_effect(reports, day_of_week, effect)
  
  expect_equal(result, expected)
})

test_that("scale_obs scales reports by fraction observed", {
  reports <- c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)
  frac_obs <- 0.5
  
  expected <- c(50, 100, 150, 200, 250, 300, 350, 400, 450, 500)
  result <- scale_obs(reports, frac_obs)
  
  expect_equal(result, expected)
})

test_that("truncate_obs truncates reports correctly", {
  reports <- c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)
  trunc_rev_cmf <- c(1.0, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1)
  
  expected_truncate <- c(100, 180, 240, 280, 300, 300, 280, 240, 180, 100)
  result_truncate <- truncate_obs(reports, trunc_rev_cmf, reconstruct = 0)
  
  expect_equal(result_truncate, expected_truncate)
  
  result_reconstruct <- truncate_obs(expected_truncate, trunc_rev_cmf, reconstruct = 1)
  
  expect_equal(result_reconstruct, reports)
})
