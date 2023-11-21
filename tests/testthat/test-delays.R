test_stan_delays <- function(generation_time = generation_time_opts(),
                             delays = delay_opts(),
                             truncation = trunc_opts(),
                             params = c()) {
  data <- EpiNow2:::create_stan_delays(
    generation_time = generation_time,
    delays = delays,
    truncation = truncation,
    weight = 10
  )
  return(unlist(unname(data[params])))
}

delay_params <-
  c("delay_params_mean", "delay_params_sd", "delay_max", "delay_np_pmf")

test_that("generation times can be specified in different ways", {
  expect_equal(
    test_stan_delays(params = delay_params),
    c(0, 1, 1, 1)
  )
  expect_equal(
    test_stan_delays(
      generation_time = generation_time_opts(dist_spec(mean = 3)),
      params = delay_params
    ),
    c(0, 0, 0, 1, 1, 1)
  )
  expect_equal(
    round(test_stan_delays(
      generation_time = generation_time_opts(dist_spec(mean = 3, sd = 1, max = 4)),
      params = delay_params
    ), digits = 2),
    c(0.02, 0.11, 0.22, 0.30, 0.35, 1.00, 1.00)
  )
})

test_that("delay parameters can be specified in different ways", {
  expect_equal(
    tail(test_stan_delays(
      delays = delay_opts(dist_spec(mean = 3)),
      params = delay_params
    ), n = -2),
    c(0, 0, 0, 1, 1)
  )
  expect_equal(
    tail(round(test_stan_delays(
      delays = delay_opts(dist_spec(mean = 3, sd = 1, max = 4)),
      params = delay_params
    ), digits = 2), n = -2),
    c(0.02, 0.11, 0.22, 0.30, 0.35, 1.00)
  )
})

test_that("truncation parameters can be specified in different ways", {
  expect_equal(
    tail(round(test_stan_delays(
      truncation = trunc_opts(dist = dist_spec(mean = 3, sd = 1, max = 4)),
      params = delay_params
    ), digits = 2), n = -2),
    c(1.00, 0.02, 0.11, 0.22, 0.30, 0.35)
  )
})

test_that("deprecated arguments are caught", {
  options(lifecycle_verbosity = "warning")
  expect_warning(
    test_stan_delays(
      generation_time = generation_time_opts(mean = 3),
      params = delay_params
    ), "deprecated"
  )
  expect_error(
    test_stan_delays(
      delays = delay_opts(mean = 3),
      params = delay_params
    ), "named arguments"
  )
  expect_warning(
    test_stan_delays(
      delays = delay_opts(list(mean = 3)),
      params = delay_params
    ), "deprecated"
  )
  expect_warning(
    test_stan_delays(
      delays = delay_opts(list(mean = 3)),
      params = delay_params
    ), "deprecated"
  )
  expect_warning(
    test_stan_delays(
      delays = trunc_opts(list(mean = 3)),
      params = delay_params
    ), "deprecated"
  )
  options(lifecycle_verbosity = NULL)
})
