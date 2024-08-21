skip_on_cran()

delays <- rlnorm(1:250, log(5), 0.2)
samples <- 1000
dist <- "lognormal"

# Run dist_fit
dist_fit_out <- dist_fit(
  delays,
  samples = samples,
  dist = dist,
  cores = ifelse(interactive(), 4, 1)
)

# Run bootstrapped_dist_fit
bootstrapped_dist_fit_out <- bootstrapped_dist_fit(
  delays,
  samples = samples,
  bootstraps = 2,
  dist = dist
)

test_that("dist_fit produces expected output", {
  expect_s4_class(dist_fit_out, "stanfit")
  expect_equal(length(extract(dist_fit_out)$mu), samples)
  expect_equal(length(extract(dist_fit_out)$sigma), samples)
})

test_that("dist_fit produces expected warning", {
  expect_warning(
    dist_fit(
      delays,
      samples = 999, # should be at least 1000 so warning is expected
      dist = dist
    ),
    "must be at least 1000."
  )
})

test_that("bootstrapped_dist_fit produces expected output", {
  expect_s3_class(bootstrapped_dist_fit_out, "dist_spec")
})

test_that("bootstrapped_dist_fit produces expected warning", {
  expect_warning(
    bootstrapped_dist_fit(
      delays,
      samples = 999, # should be at least 1000 so warning is expected
      bootstraps = 1,
      dist = dist
    ),
    "must be at least 1000."
  )
})
