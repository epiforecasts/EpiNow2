test_that("create_stan_args returns the expected defaults when the exact method is used", {
  expect_equal(names(create_stan_args()), c(
    "data", "init", "refresh", "object", "method", "chains", "save_warmup",
    "seed", "future", "max_execution_time", "cores", "warmup", "control", "iter",
    "pars", "include"
  ))
})

test_that("create_stan_args returns the expected defaults when the approximate method is used", {
  expect_equal(names(create_stan_args(stan = stan_opts(method = "vb"))), c(
    "data", "init", "refresh",
    "object", "method",
    "trials", "iter", "output_samples",
    "pars", "include"
  ))
})

test_that("create_stan_args can modify arguments", {
  expect_equal(create_stan_args(stan = stan_opts(warmup = 1000))$warmup, 1000)
})

fake_pathfinder_model <- function() {
  fake_fit <- structure(list(), class = "fake_pathfinder_fit")
  structure(list(pathfinder = function(...) fake_fit), class = "CmdStanModel")
}

test_that("create_stan_args does not leak init_method into the returned arguments", {
  args <- create_stan_args(
    stan = stan_opts(object = fake_pathfinder_model(), init_method = "pathfinder")
  )
  expect_false("init_method" %in% names(args))
})

test_that("create_stan_args uses pathfinder output as init when init_method is pathfinder", {
  fake_model <- fake_pathfinder_model()
  args <- create_stan_args(
    stan = stan_opts(object = fake_model, init_method = "pathfinder"),
    data = list(a = 1),
    init = "random"
  )
  expect_identical(args$init, fake_model$pathfinder())
})

test_that("create_stan_args ignores init_method for fixed-parameter sampling", {
  fake_model <- fake_pathfinder_model()
  stan <- list(
    object = fake_model, method = "sampling", backend = "cmdstanr",
    init_method = "pathfinder"
  )
  args <- create_stan_args(
    stan = stan, data = list(a = 1), init = 5, fixed_param = TRUE
  )
  expect_identical(args$init, 5)
})
