
# define example cases
cases <- data.table::copy(example_confirmed)[, cases := as.integer(confirm)]

# define a single report delay distribution
delay <- lognormal(
  meanlog = normal(1.4, 0.3), sdlog = normal(0.6, 0.2), max = 30
)

test_that("adjust_infection_to_report can correctly handle a simple mapping", {
  reports <- adjust_infection_to_report(
    cases,
    delay_defs = example_incubation_period + delay
  )
  expect_true(nrow(reports) > 80)
  expect_true(all(!is.infinite(reports$cases)))
  expect_true(all(!is.na(reports$cases)))
})

test_that("adjust_infection_to_report can correctly handle a mapping with a day
           of the week effect", {
  reports <- adjust_infection_to_report(
    cases,
    delay_defs = example_incubation_period + delay,
    reporting_effect = c(1.1, rep(1, 4), 0.95, 0.95)
  )
  expect_true(nrow(reports) > 80)
  expect_true(all(!is.infinite(reports$cases)))
  expect_true(all(!is.na(reports$cases)))
})

test_that("passing data tables to adjust_infection_to_report is deprecated", {
  suppressWarnings(delay_def <- lognorm_dist_def(
    mean = 5, mean_sd = 1, sd = 3, sd_sd = 1,
    max_value = 30, samples = 1, to_log = TRUE
  ))
  expect_deprecated(adjust_infection_to_report(
    cases,
    delay_defs = list(delay_def)
  ))
})
