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
