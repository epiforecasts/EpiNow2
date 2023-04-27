test_stan_delays <- function(generation_time = generation_time_opts(),
                             delays = delay_opts(),
                             truncation = trunc_opts(),
                             params = c()) {
  data <- create_stan_delays(
    generation_time = generation_time,
    delays = delays,
    truncation = truncation,
    ot = 10
  )
  return(unlist(unname(data[params])))
}

delay_params <-
  c("delay_mean_mean", "delay_mean_sd", "delay_sd_mean", "delay_sd_sd", "delay_max",
    "delay_np_pmf")

test_that("generation times can be specified in different ways", {
  expect_equal(
    test_stan_delays(params = delay_params),
    c(0, 1)
  )
  expect_equal(
    test_stan_delays(
      generation_time = generation_time_opts(dist_spec(mean = 3)),
      params = delay_params
    ),
    c(0, 0, 0, 1)
  )
  expect_equal(
    round(test_stan_delays(
      generation_time = generation_time_opts(dist_spec(mean = 3, sd = 1, max = 5)),
      params = delay_params
    ), digits = 2),
    c(0.02, 0.11, 0.22, 0.30, 0.35)
  )
  expect_equal(
    round(test_stan_delays(
      generation_time = generation_time_opts(
        get_generation_time(
          disease = "SARS-CoV-2", source = "ganyani",
          max = 10, fixed = TRUE
        )
      ),
      params = delay_params
    ), digits = 2),
    c(0.18, 0.20, 0.17, 0.13, 0.10, 0.07, 0.05, 0.04, 0.03, 0.02)
  )
  expect_equal(
    round(test_stan_delays(
      generation_time = generation_time_opts(
        get_generation_time(
          disease = "SARS-CoV-2", source = "ganyani", max = 10
        )
      ),
      params = delay_params
    ), digits = 2),
    c(3.64, 0.71, 3.08, 0.77, 10.00)
  )
})

test_that("delay parameters can be specified in different ways", {
  expect_equal(
    tail(test_stan_delays(
      delays = delay_opts(dist_spec(mean = 3)),
      params = delay_params
    ), n = -2),
    c(0, 0, 0, 1)
  )
  expect_equal(
    tail(round(test_stan_delays(
      delays = delay_opts(dist_spec(mean = 3, sd = 1, max = 5)),
      params = delay_params
    ), digits = 2), n = -2),
    c(0.02, 0.11, 0.22, 0.30, 0.35)
  )
})

test_that("truncation parameters can be specified in different ways", {
  expect_equal(
    tail(round(test_stan_delays(
      truncation = trunc_opts(dist = dist_spec(mean = 3, sd = 1, max = 5)),
      params = delay_params
    ), digits = 2), n = -2),
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
    test_stan_delays(delays = delay_opts(dist_spec(mean = 3.5))),
    "must be an integer"
  )
  expect_error(
    test_stan_delays(delays = delay_opts(dist_spec(mean = 3, mean_sd = 1))),
    "must be 0"
  )
})
