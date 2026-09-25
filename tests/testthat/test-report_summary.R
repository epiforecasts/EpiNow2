summarised <- data.table::data.table(
  date = as.Date("2020-03-22"),
  variable = c("R", "infections", "growth_rate"),
  strat = NA_character_,
  type = "estimate",
  median = c(1.234, 120.6, 0.1),
  mean = c(1.25, 121.2, 0.11),
  sd = c(0.2, 15.1, 0.04),
  lower_90 = c(0.912, 95.2, 0.05),
  upper_90 = c(1.567, 150.9, 0.2)
)
# half of the samples are at or below 1
rt_samples <- data.table::data.table(
  sample = 1:4, value = c(0.5, 0.9, 1.1, 1.3)
)

test_that("report_summary produces expected output", {
  out <- report_summary(summarised, rt_samples)
  expect_named(out, c("measure", "estimate"))
  expect_equal(
    out$measure,
    c(
      "New infections per day", "Expected change in reports",
      "Effective reproduction no.", "Rate of growth",
      "Doubling/halving time (days)"
    )
  )
  expect_equal(
    out$estimate,
    c(
      "120 (95 -- 150)",
      "Stable",
      "1.2 (0.91 -- 1.6)",
      "0.1 (0.05 -- 0.2)",
      # log(2) / r, so the upper growth rate gives the lower doubling time
      "6.9 (3.5 -- 14)"
    )
  )
})

test_that("report_summary returns numeric estimates when requested", {
  out <- report_summary(summarised, rt_samples, return_numeric = TRUE)
  expect_named(out, c("measure", "estimate", "numeric_estimate"))
  expect_length(out$numeric_estimate, 5)
  expect_equal(out$numeric_estimate[[2]], 0.5)
  expect_equal(out$numeric_estimate[[3]]$median, 1.2)
  expect_equal(out$numeric_estimate[[3]]$upper_90, 1.6)
  expect_equal(out$numeric_estimate[[5]]$median, signif(log(2) / 0.1, 2))
})

test_that("report_summary saves the summary to a target folder", {
  tmp <- withr::local_tempdir()
  out <- report_summary(summarised, rt_samples, target_folder = tmp)
  expect_equal(readRDS(file.path(tmp, "summary.rds")), out)
})
