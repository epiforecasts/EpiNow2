skip_on_cran()

R <- data.frame(
  date = seq.Date(as.Date("2023-01-01"), length.out = 14, by = "day"),
  R = c(rep(1.2, 7), rep(0.8, 7))
)
initial_infections <- 100
test_simulate_infections <- function(obs = obs_opts(family = "poisson"), ...) {
  sim <- simulate_infections(
    R = R,
    initial_infections = 100,
    obs = obs,
    ...
  )
  return(sim)
}

test_that("simulate_infections works as expected with standard parameters", {
  set.seed(123)
  sim <- test_simulate_infections()
  expect_equal(nrow(sim), 2 * nrow(R))
  expect_snapshot_output(sim)
  set.seed(Sys.time())
})

test_that("simulate_infections works as expected with additional parameters", {
  set.seed(123)
  sim <- test_simulate_infections(
    generation_time = generation_time_opts(fix_dist(example_generation_time)),
    delays = delay_opts(fix_dist(example_reporting_delay)),
    obs = obs_opts(family = "negbin", phi = list(mean = 0.5, sd = 0))
  )
  expect_equal(nrow(sim), 2 * nrow(R))
  expect_snapshot_output(sim)
  set.seed(Sys.time())
})

test_that("simulate_infections fails with uncertain parameters", {
  expect_error(
    test_simulate_infections(obs = obs_opts(family = "negbin")),
    "uncertain"
  )
  expect_error(
    test_simulate_infections(
      obs = obs_opts(scale = list(mean = 1, sd = 1))
    ),
    "uncertain"
  )
  expect_error(
    test_simulate_infections(
      delays = delay_opts(example_incubation_period)
    ),
    "uncertain"
  )
})
