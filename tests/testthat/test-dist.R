skip_on_cran()
skip_on_os("windows")

test_that("distributions are the same in R and stan", {
  args <- list(mean = 3, sd = 2, max = 15)

  lognormal_dist <- do.call(LogNormal, args)
  gamma_dist <- do.call(Gamma, args)

  lognormal_params <- unname(as.numeric(lognormal_dist[[1]]$parameters))
  gamma_params <- unname(as.numeric(gamma_dist[[1]]$parameters))

  pmf_r_lognormal <- discretise(lognormal_dist)[[1]]$pmf
  pmf_r_gamma <- discretise(gamma_dist)[[1]]$pmf

  pmf_stan_lognormal <- discretised_pmf(lognormal_params, args$max + 1, 0)
  pmf_stan_gamma <- discretised_pmf(gamma_params, args$max + 1, 1)

  expect_equal(pmf_r_lognormal, pmf_stan_lognormal)
  expect_equal(pmf_r_gamma, pmf_stan_gamma)
})

test_that("deprecated functions are deprecated", {
  delay_fn <- function(n, dist, cum) {
      pgamma(n + 0.9999, 2, 1) - pgamma(n - 1e-5, 2, 1)
  }
  expect_deprecated(
    sample_approx_dist(
      cases = example_confirmed[1:5],
      dist_fn = delay_fn,
      direction = "forwards",
      type = "median"
    )
  )
  args <- list(mean = 3, mean_sd = 0, sd = 2, sd_sd = 0, max_value = 15)
  expect_deprecated(
    do.call(lognorm_dist_def, (c(args, list(samples = 1))))$params[[1]]
  )
  expect_deprecated(
    do.call(gamma_dist_def, (c(args, list(samples = 1))))$params[[1]]
  )
})
