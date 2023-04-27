test_that("report_cases can simulate infections forward", {
  # define example cases
  cases <- example_confirmed[1:10]

  # set up example delays
  generation_time <- get_generation_time(
    disease = "SARS-CoV-2", source = "ganyani"
  )
  incubation_period <- get_incubation_period(
    disease = "SARS-CoV-2", source = "lauer"
  )
  reporting_delay <- dist_spec(
    mean = convert_to_logmean(2, 1), mean_sd = 0.1,
    sd = convert_to_logsd(2, 1), sd_sd = 0.1, max = 5
  )

  # Instead of running them model we use example
  # data for speed in this example.
  cases <- cases[, cases := as.integer(confirm)]
  cases <- cases[, confirm := NULL][, sample := 1]
  set.seed(123)
  reported_cases <- report_cases(
    case_estimates = cases,
    delays = delay_opts(incubation_period + reporting_delay),
    type = "sample"
  )
  expect_equal(class(reported_cases), "list")
  expect_equal(class(reported_cases$samples), c("data.table", "data.frame"))
  expect_equal(class(reported_cases$summarised), c("data.table", "data.frame"))
  expect_equal(nrow(reported_cases$summarised), 9)
  expect_equal(class(reported_cases$summarised$median), "numeric")
})
