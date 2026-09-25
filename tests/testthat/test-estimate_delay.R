skip_on_cran()

# Integration tests (MCMC-based) ------------------------------------------
# These tests run actual MCMC sampling and are slow. Tests are divided into:
# - Core tests: Essential tests that always run to catch critical failures
# - Variant tests: Configuration variations that only run weekly (gated by EPINOW2_SKIP_INTEGRATION)

delays <- rlnorm(1:250, log(5), 0.2)
samples <- 50
dist <- "lognormal"

# Core test: Run dist_fit once (1 Stan fit)
dist_fit_out <- dist_fit(
  delays,
  samples = samples,
  dist = dist,
  cores = ifelse(interactive(), 4, 1)
)

test_that("dist_fit produces expected output", {
  expect_s4_class(dist_fit_out, "stanfit")
  expect_equal(length(extract(dist_fit_out)$mu), samples)
  expect_equal(length(extract(dist_fit_out)$sigma), samples)
})

# Variant test: bootstrapped_dist_fit runs Stan 3 times (original + 2 bootstraps)
test_that("bootstrapped_dist_fit produces expected output", {
  skip_integration()
  bootstrapped_dist_fit_out <- bootstrapped_dist_fit(
    delays,
    samples = samples,
    bootstraps = 2,
    dist = dist
  )
  expect_s3_class(bootstrapped_dist_fit_out, "dist_spec")
})

# bootstrapped_dist_fit logic around the Stan fits, with dist_fit and
# rstan::extract mocked so no sampling is needed.
local_mock_dist_fit <- function(env = parent.frame()) {
  calls <- new.env()
  calls$values <- list()
  calls$samples <- integer(0)
  local_mocked_bindings(
    dist_fit = function(values, samples, dist, ...) {
      calls$values <- c(calls$values, list(values))
      calls$samples <- c(calls$samples, samples)
      # posterior draws depend on the bootstrap number
      list(draw = length(calls$samples), n = 2 * samples)
    },
    extract = function(fit) {
      draws <- rep(fit$draw, fit$n)
      list(mu = draws, sigma = draws / 10, alpha = draws, beta = draws / 2)
    },
    .package = "EpiNow2",
    .env = env
  )
  calls
}

test_that("bootstrapped_dist_fit errors for unsupported distributions", {
  expect_error(
    bootstrapped_dist_fit(1:10, dist = "weibull"),
    "Unsupported distribution"
  )
})

test_that("bootstrapped_dist_fit drops missing and negative values", {
  calls <- local_mock_dist_fit()
  out <- bootstrapped_dist_fit(
    c(1, 2, NA, -1, 3.7), samples = 10, bootstraps = 1
  )
  expect_equal(calls$values, list(c(1L, 2L, 3L)))
  expect_equal(calls$samples, 10)
  expect_s3_class(out, "dist_spec")
  expect_equal(get_distribution(out), "lognormal")
  expect_equal(get_parameters(out), list(meanlog = 1, sdlog = 0.1))
  # max defaults to the largest retained value
  expect_equal(max(out), 3)
})

test_that("bootstrapped_dist_fit pools bootstrapped gamma fits", {
  calls <- local_mock_dist_fit()
  out <- bootstrapped_dist_fit(
    rep(1:5, 4), dist = "gamma", samples = 4, bootstraps = 2,
    bootstrap_samples = 8, max_value = 20
  )
  # each bootstrap fits a subsample of bootstrap_samples values and
  # contributes samples / bootstraps posterior draws
  expect_equal(lengths(calls$values), c(8, 8))
  expect_true(all(unlist(calls$values) %in% 1:5))
  expect_equal(calls$samples, c(2, 2))
  expect_equal(get_distribution(out), "gamma")
  expect_equal(max(out), 20)
  params <- get_parameters(out)
  expect_named(params, c("shape", "rate"))
  # draws of 1, 1, 2, 2 for shape and half that for rate
  expect_equal(mean(params$shape), 1.5)
  expect_equal(sd(params$shape), sd(c(1, 1, 2, 2)))
  expect_equal(mean(params$rate), 0.75)
})

test_that("bootstrapped_dist_fit uses at least one sample per bootstrap", {
  calls <- local_mock_dist_fit()
  bootstrapped_dist_fit(1:5, samples = 1, bootstraps = 3)
  expect_equal(calls$samples, c(1, 1, 1))
  expect_equal(lengths(calls$values), c(5, 5, 5))
})
