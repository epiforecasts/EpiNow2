skip_on_cran()
skip_on_os("windows")

test_that("distributions are the same in R and stan", {
  args <- list(mean = 3, sd = 2)
  max_args <- c(args, list(max = 15))

  lognormal_params <- do.call(lognormal, args)$params_mean
  gamma_params <- do.call(gamma, args)$params_mean

  pmf_r_lognormal <- as.vector(do.call(lognormal, max_args)$np_pmf)
  pmf_r_gamma <- as.vector(do.call(gamma, max_args)$np_pmf)

  pmf_stan_lognormal <- discretised_pmf(lognormal_params, max_args$max + 1, 0)
  pmf_stan_gamma <- discretised_pmf(gamma_params, max_args$max + 1, 1)

  expect_equal(pmf_r_lognormal, pmf_stan_lognormal)
  expect_equal(pmf_r_gamma, pmf_stan_gamma)
})

test_that("deprecated functions are deprecated", {
  args <- list(mean = 3, mean_sd = 0, sd = 2, sd_sd = 0, max_value = 15)
  expect_deprecated(
    do.call(lognorm_dist_def, (c(args, list(samples = 1))))$params[[1]]
  )
  expect_deprecated(
    do.call(gamma_dist_def, (c(args, list(samples = 1))))$params[[1]]
  )
})
