test_that("create_rt_data returns expected default values", {
  result <- create_rt_data()

  expect_type(result, "list")
  expect_equal(result$estimate_r, 1)
  expect_equal(result$bp_n, 0)
  expect_equal(result$breakpoints, numeric(0))
  expect_equal(result$use_pop, 0)
  expect_equal(result$future_time, 0)
})

test_that("create_rt_data handles NULL rt input correctly", {
  result <- create_rt_data(rt = NULL)

  expect_equal(result$estimate_r, 0)
})

test_that("create_rt_data handles custom rt_opts correctly", {
  custom_rt <- rt_opts(
    use_rt = FALSE,
    use_breakpoints = FALSE,
    pop = Normal(mean = 1000000, sd = 100)
  )

  result <- create_rt_data(rt = custom_rt, horizon = 7)

  expect_equal(result$estimate_r, 0)
  expect_equal(result$use_pop, 1)
  expect_equal(result$future_time, 7)
})

test_that("create_rt_data deprecates and ignores breakpoints", {
  expect_warning(
    result <- create_rt_data(rt_opts(use_breakpoints = TRUE),
      breakpoints = c(1, 0, 1, 0, 1)
    ),
    "deprecated"
  )
  expect_equal(result$bp_n, 0)
})

test_that("rt_opts(future) is deprecated but still sets the future time", {
  lifecycle::expect_deprecated(rt <- rt_opts(future = "project"))
  result <- create_rt_data(rt, horizon = 7)
  expect_equal(result$future_time, 7)
})

test_that("create_rt_data handles zero sum breakpoints", {
  result <- create_rt_data(rt_opts(use_breakpoints = TRUE),
    breakpoints = rep(0, 5)
  )

  expect_equal(result$bp_n, 0)
})

test_that("create_rt_data warns when fixed population is smaller than cumulative cases", {
  data <- data.table::data.table(
    date = seq.Date(as.Date("2020-01-01"), by = "day", length.out = 10),
    confirm = rep(100, 10)
  )

  # Should warn for fixed population smaller than total cases (1000)
  expect_warning(
    create_rt_data(rt_opts(pop = Fixed(500)), data = data),
    "Population.*is smaller than cumulative cases"
  )

  # Should not warn for fixed population larger than total cases
  expect_no_warning(
    create_rt_data(rt_opts(pop = Fixed(2000)), data = data)
  )
})

test_that("create_rt_data uses pop_period correctly", {
  # Test that use_pop is set correctly for different pop_period values
  result_forecast <- create_rt_data(rt_opts(
    pop = Fixed(1000000),
    pop_period = "forecast"
  ))
  expect_equal(result_forecast$use_pop, 1)

  result_all <- create_rt_data(rt_opts(
    pop = Fixed(1000000),
    pop_period = "all"
  ))
  expect_equal(result_all$use_pop, 2)
})

test_that("create_rt_data passes pop_floor correctly", {
  result <- create_rt_data(rt_opts(
    pop = Fixed(1000000),
    pop_floor = 10.0
  ))
  expect_equal(result$pop_floor, 10.0)
})
