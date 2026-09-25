test_that("epinow2_stan_model returns precompiled rstan models by default", {
  model <- epinow2_stan_model()
  expect_s4_class(model, "stanmodel")
  expect_identical(model, epinow2_rstan_model("estimate_infections"))
  expect_identical(
    epinow2_stan_model(model = "estimate_secondary"),
    epinow2_rstan_model("estimate_secondary")
  )
})

test_that("epinow2_stan_model compiles the requested cmdstanr model", {
  local_mocked_bindings(
    epinow2_cmdstan_model = function(model, ...) {
      structure(list(model = model), class = "CmdStanModel")
    }
  )
  model <- epinow2_stan_model("cmdstanr", "estimate_truncation")
  expect_s3_class(model, "CmdStanModel")
  expect_equal(model$model, "estimate_truncation")
})

test_that("epinow2_stan_model errors for unknown backends and models", {
  expect_error(epinow2_stan_model("stan"), "backend")
  expect_error(epinow2_stan_model(model = "estimate_everything"), "model")
})
