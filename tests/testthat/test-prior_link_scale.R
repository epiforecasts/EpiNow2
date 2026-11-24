test_that("prior_link_scale standardises tight priors on bounded parameters", {
  link <- prior_link_scale(
    mean = c(1e6, 0.5, 2, -1), sd = c(1e3, 0.1, 1, 1),
    lower = c(0, 0, -Inf, -Inf), upper = c(Inf, 1, Inf, 3)
  )
  expect_equal(link$loc, c(log(1e6), 0, 2, log(4)))
  expect_equal(link$scale, c(1e-3, 0.1 / 0.25, 1, 1 / 4))
})

test_that("prior_link_scale falls back when the mean is outside the bounds", {
  link <- prior_link_scale(
    mean = c(0, 1), sd = c(0.5, 0), lower = c(0, 0), upper = c(Inf, Inf)
  )
  expect_equal(link$loc, c(0, 0))
  expect_equal(link$scale, c(1, 1))
})

test_that("create_stan_params returns the prior location and scale", {
  params <- create_stan_params(list(
    make_param("pop", Normal(mean = 1e6, sd = 1e3), lower_bound = 0)
  ))
  expect_equal(params$params_loc, array(log(1e6)), tolerance = 1e-3)
  expect_equal(params$params_scale, array(1e-3), tolerance = 1e-3)
})

test_that("params_link maps parameters to the scale of the bounds", {
  expect_equal(
    params_link(c(0.2, 3, 1), c(0, 1, -Inf), c(1, Inf, Inf)),
    c(qlogis(0.2), log(2), 1)
  )
})
