test_that("check_stan_delay accepts supported distributions", {
  expect_silent(EpiNow2:::check_stan_delay(Fixed(1)))
  expect_silent(
    EpiNow2:::check_stan_delay(LogNormal(meanlog = 1, sdlog = 0.5, max = 10))
  )
  expect_no_error(suppressMessages(EpiNow2:::check_stan_delay(
    Gamma(shape = Normal(2, 0.1), rate = Normal(1, 0.1), max = 10)
  )))
  expect_silent(EpiNow2:::check_stan_delay(NonParametric(c(0.2, 0.8))))
})

test_that("check_stan_delay errors for bad 'dist' specifications", {
  expect_error(EpiNow2:::check_stan_delay(c(0.2, 0.8)), "dist_spec")
  expect_error(
    EpiNow2:::check_stan_delay(Normal(mean = 2, sd = 1, max = 10)),
    "lognormal"
  )
  # uncertain parameters must be normally distributed
  expect_error(
    EpiNow2:::check_stan_delay(
      LogNormal(meanlog = Gamma(mean = 1, sd = 1), sdlog = 0.5, max = 10)
    ),
    "normally distributed"
  )
  expect_error(
    suppressMessages(EpiNow2:::check_stan_delay(
      LogNormal(meanlog = Normal(1, 0.1), sdlog = 0.5)
    )),
    "finite maximum"
  )
})

test_that("check_generation_time errors if a nonparametric PMF starts above 0", {
  expect_error(
    EpiNow2:::check_generation_time(NonParametric(c(0.5, 0.5))),
    "zero as first element"
  )
  expect_silent(
    EpiNow2:::check_generation_time(NonParametric(c(0, 0.5, 0.5)))
  )
  expect_error(gt_opts(NonParametric(c(0.1, 0.9))), "zero as first element")
})

test_that("check_generation_time uses the prior mean of estimated PMFs", {
  expect_error(
    EpiNow2:::check_generation_time(
      NonParametric(Dirichlet(prior = c(0.2, 0.8), concentration = 10))
    ),
    "zero as first element"
  )
  expect_silent(EpiNow2:::check_generation_time(
    NonParametric(Dirichlet(prior = c(0, 0.2, 0.8), concentration = 10))
  ))
})

test_that("check_truncation_length does not warn for parametric truncation", {
  stan_args <- list(data = EpiNow2:::create_stan_delays(
    reporting = delay_opts(NonParametric(rep(0.05, 20))),
    truncation = trunc_opts(
      LogNormal(meanlog = Normal(1, 0.1), sdlog = 0.5, max = 20)
    )
  ))
  expect_silent(
    EpiNow2:::check_truncation_length(stan_args, time_points = 5)
  )
})
