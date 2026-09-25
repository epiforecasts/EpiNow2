skip_on_cran()
skip_on_os("windows")

# params.stan overloads get_param(), which rstan cannot expose, so only the
# prior density function is exposed here
params_src <- readLines(
  system.file("stan", "functions", "params.stan", package = "EpiNow2")
)
fn_start <- grep("^real param_prior_lpdf", params_src)
stopifnot("param_prior_lpdf() not found in params.stan" = length(fn_start) == 1)
fn_end <- fn_start + which(params_src[-seq_len(fn_start - 1)] == "}")[1] - 1
suppressMessages(rstan::expose_stan_functions(rstan::stanc(
  model_code = paste(
    c("functions {", params_src[fn_start:fn_end], "}"),
    collapse = "\n"
  )
)))

test_that("param_prior_lpdf is not truncated within finite bounds", {
  expect_equal(
    param_prior_lpdf(1.5, 0L, 0.2, 0.5, 0, 3),
    dlnorm(1.5, 0.2, 0.5, log = TRUE)
  )
  expect_equal(
    param_prior_lpdf(1.5, 1L, 2, 3, 0.5, 3),
    dgamma(1.5, shape = 2, rate = 3, log = TRUE)
  )
  expect_equal(
    param_prior_lpdf(1.5, 2L, 1, 2, -1, 2),
    dnorm(1.5, 1, 2, log = TRUE)
  )
})

test_that("param_prior_lpdf matches the density with unbounded support", {
  expect_equal(
    param_prior_lpdf(1.5, 2L, 1, 2, -Inf, Inf),
    dnorm(1.5, 1, 2, log = TRUE)
  )
  expect_equal(
    param_prior_lpdf(1.5, 0L, 0.2, 0.5, 0, Inf),
    dlnorm(1.5, 0.2, 0.5, log = TRUE)
  )
})

test_that("param_prior_lpdf is -Inf outside the bounds", {
  expect_identical(param_prior_lpdf(3.5, 0L, 0.2, 0.5, 0, 3), -Inf)
  expect_identical(param_prior_lpdf(-1.5, 2L, 1, 2, -1, 2), -Inf)
})

test_that("param_prior_lpdf rejects unknown distributions", {
  expect_error(param_prior_lpdf(1.5, 3L, 1, 2, 0, 3), "dist must be <= 2")
})
