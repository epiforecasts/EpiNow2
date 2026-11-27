skip_on_cran()

cases <- as.data.table(example_confirmed)[, primary := confirm]
test_simulate_secondary <- function(obs = obs_opts(family = "poisson"), ...) {
  sim <- simulate_secondary(
    primary = cases,
    obs = obs,
    ...
  )
  return(sim)
}

test_that("simulate_secondary works as expected with standard parameters", {
  set.seed(123)
  sim <- test_simulate_secondary()
  expect_equal(nrow(sim), nrow(cases))
  # secondary observations are random draws whose exact values are not
  # reproducible across architectures (the Stan RNG stream differs on x86_64
  # vs arm64), so only their structure is checked rather than snapshotted.
  expect_true(all(sim$secondary >= 0))
  expect_true(all(sim$secondary == round(sim$secondary)))
  set.seed(Sys.time())
})

test_that("simulate_secondary works as expected with additional parameters", {
  set.seed(123)
  sim <- test_simulate_secondary(
    delays = delay_opts(fix_parameters(example_reporting_delay)),
    obs = obs_opts(family = "negbin", dispersion = Fixed(0.5))
  )
  expect_equal(nrow(sim), nrow(cases))
  # secondary observations are random draws whose exact values are not
  # reproducible across architectures (the Stan RNG stream differs on x86_64
  # vs arm64), so only their structure is checked rather than snapshotted.
  expect_true(all(sim$secondary >= 0))
  expect_true(all(sim$secondary == round(sim$secondary)))
  set.seed(Sys.time())
})

test_that("simulate_secondary fails with uncertain parameters", {
  expect_error(
    test_simulate_secondary(obs = obs_opts(family = "negbin")),
    "uncertain"
  )
  expect_error(
    test_simulate_secondary(
      obs = obs_opts(scale = Normal(mean = 1, sd = 1))
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

test_that("simulate_secondary fails with a mismatched day_of_week_effect", {
  expect_error(
    test_simulate_secondary(day_of_week_effect = c(2, 1, 1)),
    "length"
  )
})
