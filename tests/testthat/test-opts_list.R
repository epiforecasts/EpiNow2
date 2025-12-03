# Test opts_list and filter_opts functions

test_that("opts_list creates named list with default opts for each region", {
  cases <- data.table::data.table(
    region = c(rep("region_a", 5), rep("region_b", 5)),
    date = rep(seq(as.Date("2024-01-01"), by = "day", length.out = 5), 2),
    confirm = rep(100, 10)
  )

  result <- opts_list(rt_opts(), cases)

  expect_true(is.list(result))
  expect_equal(names(result), c("region_a", "region_b"))

  # Each region should have rt_opts
  expect_s3_class(result$region_a, "rt_opts")
  expect_s3_class(result$region_b, "rt_opts")
})

test_that("opts_list allows region-specific overrides", {
  cases <- data.table::data.table(
    region = c(rep("region_a", 5), rep("region_b", 5)),
    date = rep(seq(as.Date("2024-01-01"), by = "day", length.out = 5), 2),
    confirm = rep(100, 10)
  )

  result <- opts_list(rt_opts(), cases, region_b = rt_opts(rw = 7))

  # region_a should have default (rw = 0)
  expect_equal(result$region_a$rw, 0)

  # region_b should have override (rw = 7)
  expect_equal(result$region_b$rw, 7)
})

test_that("opts_list handles single region", {
  cases <- data.table::data.table(
    region = rep("only_region", 5),
    date = seq(as.Date("2024-01-01"), by = "day", length.out = 5),
    confirm = rep(100, 5)
  )

  result <- opts_list(gp_opts(), cases)

  expect_length(result, 1)
  expect_equal(names(result), "only_region")
  expect_s3_class(result$only_region, "gp_opts")
})

test_that("filter_opts returns region-specific opts when available", {
  opts <- list(
    region_a = rt_opts(rw = 3),
    region_b = rt_opts(rw = 7)
  )

  result <- EpiNow2:::filter_opts(opts, "region_a")

  expect_equal(result$rw, 3)
})

test_that("filter_opts returns default opts when region not found", {
  opts <- rt_opts(rw = 5)

  result <- EpiNow2:::filter_opts(opts, "nonexistent_region")

  # Should return the opts unchanged when region not in names
  expect_equal(result$rw, 5)
})

test_that("filter_opts handles empty region name", {
  opts <- list(
    region_a = rt_opts(rw = 3)
  )

  # Empty string is not in names, so should return opts
  result <- EpiNow2:::filter_opts(opts, "")

  expect_true(is.list(result))
})
