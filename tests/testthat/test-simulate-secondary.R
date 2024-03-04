skip_on_cran()

cases <- as.data.table(example_confirmed)[, primary := confirm]
test_simulate_secondary <- function(obs = obs_opts(family = "poisson"), ...) {
  sim <- simulate_secondary(
    primary =  cases,
    obs = obs,
    ...
  )
  return(sim)
}

test_that("simulate_secondary works as expected with standard parameters", {
  set.seed(123)
  sim <- test_simulate_secondary()
  expect_equal(nrow(sim), nrow(cases))
  expect_snapshot_output(sim)
  set.seed(Sys.time())
})

test_that("simulate_secondary works as expected with additional parameters", {
  set.seed(123)
  sim <- test_simulate_secondary(
    delays = delay_opts(fix_dist(example_reporting_delay)),
    obs = obs_opts(family = "negbin", phi = list(mean = 0.5, sd = 0))
  )
  expect_equal(nrow(sim), nrow(cases))
  expect_snapshot_output(sim)
  set.seed(Sys.time())
})

test_that("simulate_secondary fails with uncertain parameters", {
  expect_error(
    test_simulate_secondary(obs = obs_opts(family = "negbin")),
    "uncertain"
  )
  expect_error(
    test_simulate_secondary(
      obs = obs_opts(scale = list(mean = 1, sd = 1))
    ),
    "uncertain"
  )
  expect_error(
    test_simulate_secondary(
      delays = delay_opts(example_incubation_period)
    ),
    "uncertain"
  )
})
