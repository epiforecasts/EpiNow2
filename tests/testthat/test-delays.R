test_stan_data <- function(generation_time = generation_time_opts(),
                           delays = delay_opts(),
                           truncation = trunc_opts(),
                           params = NULL) {
  data <- create_stan_data(
    reported_cases = example_confirmed,
    generation_time = generation_time,
    delays = delays,
    truncation = truncation,
    rt = rt_opts(),
    gp = gp_opts(),
    obs = obs_opts(),
    backcalc = backcalc_opts(),
    shifted_cases = NULL,
    horizon = 7
  )
  return(unlist(unname(data[params])))
}

test_that("generation times can be specified in different ways", {
  gt_params <-
    c("gt_mean_mean", "gt_mean_sd", "gt_sd_mean", "gt_sd_sd", "gt_max",
      "gt_np_pmf")
  expect_equal(
    test_stan_data(params = gt_params),
    c(0, 1)
  )
  expect_equal(
    test_stan_data(
      generation_time = generation_time_opts(dist_spec(mean = 3)),
      params = gt_params
    ),
    c(0, 0, 0, 1)
  )
  expect_equal(
    round(test_stan_data(
      generation_time = generation_time_opts(dist_spec(mean = 3, sd = 1, max = 5)),
      params = gt_params
    ), digits = 2),
    c(0.02, 0.11, 0.22, 0.30, 0.35)
  )
  expect_equal(
    round(test_stan_data(
      generation_time = generation_time_opts(
        get_generation_time(
          disease = "SARS-CoV-2", source = "ganyani",
          max = 10, fixed = TRUE
        )
      ),
      params = gt_params
    ), digits = 2),
    c(0.18, 0.20, 0.17, 0.13, 0.10, 0.07, 0.05, 0.04, 0.03, 0.02)
  )
  expect_equal(
    round(test_stan_data(
      generation_time = generation_time_opts(
        get_generation_time(
          disease = "SARS-CoV-2", source = "ganyani", max = 10
        )
      ),
      params = gt_params
    ), digits = 2),
    c(3.64, 0.71, 3.08, 0.77, 10.00)
  )
})

test_that("delay parameters can be specified in different ways", {
  delay_params <-
    c(
      "delay_mean_mean", "delay_mean_sd", "delay_sd_mean", "delay_sd_sd",
      "delay_max", "delay_np_pmf"
    )
  expect_equal(
    test_stan_data(
      delays = delay_opts(dist_spec(mean = 3)),
      params = delay_params
    ),
    c(0, 0, 0, 1)
  )
  expect_equal(
    round(test_stan_data(
      delays = delay_opts(dist_spec(mean = 3, sd = 1, max = 5)),
      params = delay_params
    ), digits = 2),
    c(0.02, 0.11, 0.22, 0.30, 0.35)
  )
})

test_that("truncation parameters can be specified in different ways", {
  trunc_params <-
    c(
      "trunc_mean_mean", "trunc_mean_sd", "trunc_sd_mean", "trunc_sd_sd",
      "trunc_max", "trunc_np_pmf"
    )
  expect_equal(
    round(test_stan_data(
      truncation = trunc_opts(dist = dist_spec(mean = 3, sd = 1, max = 5)),
      params = trunc_params
    ), digits = 2),
    c(0.02, 0.11, 0.22, 0.30, 0.35)
  )
})

test_that("contradictory generation times are caught", {
  expect_error(generation_time_opts(dist_spec(mean = 3.5)), "must be an integer")
  expect_error(
    generation_time_opts(dist_spec(mean = 3, mean_sd = 1)),
    "must be 0"
  )
})

test_that("contradictory delays are caught", {
  expect_error(
    test_stan_data(delays = delay_opts(dist_spec(mean = 3.5))),
    "must be an integer"
  )
  expect_error(
    test_stan_data(delays = delay_opts(dist_spec(mean = 3, mean_sd = 1))),
    "must be 0"
  )
})
