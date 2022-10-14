 # define example cases
 cases <- data.table::copy(example_confirmed)[, cases := as.integer(confirm)]

 # define a single report delay distribution
 delay_def <- lognorm_dist_def(
   mean = 5, mean_sd = 1, sd = 3, sd_sd = 1,
   max_value = 30, samples = 1, to_log = TRUE
 )

 # define a single incubation period
 incubation_def <- lognorm_dist_def(
   mean = incubation_periods[1, ]$mean,
   mean_sd = incubation_periods[1, ]$mean_sd,
   sd = incubation_periods[1, ]$sd,
   sd_sd = incubation_periods[1, ]$sd_sd,
   max_value = 30, samples = 1
 )

test_that("adjust_infection_to_report can correctly handle a simple mapping", {
  expect_snapshot(adjust_infection_to_report(
    cases, delay_defs = list(incubation_def, delay_def)
  ))
})

test_that("adjust_infection_to_report can correctly handle a mapping with a day
           of the week effect", {
  expect_snapshot(adjust_infection_to_report(
    cases,
    delay_defs = list(incubation_def, delay_def),
    reporting_effect = c(1.1, rep(1, 4), 0.95, 0.95)
  ))
})
