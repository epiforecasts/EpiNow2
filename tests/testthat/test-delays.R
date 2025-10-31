test_stan_delays <- function(generation_time = gt_opts(Fixed(1)),
                             delays = delay_opts(),
                             truncation = trunc_opts(),
                             params = c()) {
  data <- EpiNow2:::create_stan_delays(
    generation_time = generation_time,
    delays = delays,
    truncation = truncation,
    time_points = 10
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
      generation_time = gt_opts(Fixed(value = 3)),
      params = delay_params
    ),
    c(0, 0, 0, 1, 1, 1)
  )
  expect_equal(
    round(test_stan_delays(
      generation_time = gt_opts(
        LogNormal(meanlog = 3, sdlog = 1, max = 4)
      ),
      params = delay_params
    ), digits = 2),
    c(0.01, 0.12, 0.34, 0.53, 1.00, 1.00)
  )
})

test_that("delay parameters can be specified in different ways", {
  expect_equal(
    tail(test_stan_delays(
      delays = delay_opts(Fixed(value = 3)),
      params = delay_params
    ), n = -2),
    c(0, 0, 0, 1, 1)
  )
  expect_equal(
    tail(round(test_stan_delays(
      delays = delay_opts(
        LogNormal(meanlog = 3, sdlog = 1, max = 4)
      ),
      params = delay_params
    ), digits = 2), n = -2),
    c(0.01, 0.12, 0.34, 0.53, 1.00)
  )
  expect_equal(
    tail(round(test_stan_delays(
      delays = suppressMessages(delay_opts(
        LogNormal(meanlog = 0.5, sdlog = 0.5)
      )),
      params = delay_params
    ), digits = 2), n = -2),
    c(0.03, 0.38, 0.37, 0.14, 0.05, 0.02, 0.01, 0.00, 0.00, 1.00)
  )
  expect_equal(
    test_stan_delays(
      delays = delay_opts(NonParametric(pmf = c(0.1, 0.6, 0.3))),
      params = delay_params
    ),
    c(0.0, 1.0, 0.1, 0.6, 0.3, 1.0)
  )
})

test_that("truncation parameters can be specified in different ways", {
  expect_equal(
    tail(round(test_stan_delays(
      truncation = trunc_opts(
        dist = LogNormal(meanlog = 3, sdlog = 1, max = 4)
      ),
      params = delay_params
    ), digits = 2), n = -2),
    c(1.00, 0.01, 0.12, 0.34, 0.53)
  )
})

test_that("distributions incompatible with stan models are caught", {
  expect_error(suppressMessages(gt_opts(
    Gamma(2, 2),
    default_cdf_cutoff = 0
  )), "maximum")
  expect_error(delay_opts(
    Normal(2, 2, max = 10)
  ), "lognormal")
})
